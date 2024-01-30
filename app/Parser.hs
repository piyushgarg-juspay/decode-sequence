{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Debug where

import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class (MonadIO(..))
import Control.Reference ((^.), biplateRef, (^?), (!~))
import Data.List.Split (splitOn)
import Data.Maybe (Maybe(..), fromJust)
import GHC.Generics (Generic(..))
import System.FilePath (pathSeparator, (</>), (<.>))
import Data.List (isInfixOf)
import FastString


import GHC hiding (loadModule, ModuleName, DataDecl)
import GHC.Paths ( libdir )
import Language.Haskell.TH.LanguageExtensions (Extension(..))
import StringBuffer (hGetStringBuffer, stringToStringBuffer)
import Outputable
import HscTypes
import qualified HsDecls as GHC
import TcRnDriver
import TcRnTypes
import TcRnMonad
import Data.IORef
import DynFlags
import Avail
import SrcLoc

import qualified Language.Haskell.GHC.ExactPrint as EP
import Data.Either

import System.IO.Strict as StrictIO (hGetContents)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Algorithm.Diff (Diff(..), getGroupedDiff)
import Data.Algorithm.DiffContext (prettyContextDiff, getContextDiff)
import System.IO
import System.Directory
import Text.PrettyPrint as PP (text, render)
import DynamicLoading (initializePlugins)
import EnumSet (toList)
import Bag (bagToList)
import Data.Generics.Uniplate.Operations (universeBi)
import qualified Data.Map as Map
import Id as GHC (Id, mkVanillaGlobal)
import Control.Applicative (Applicative(..), (<$>), Alternative(..))
import Language.Haskell.Tools.BackendGHC.GHCUtils (getTopLevelId)
import UniqSupply as GHC (uniqFromSupply, mkSplitUniqSupply)
import Type as GHC (Type, mkTyVarTy, mkTyConTy)
import Name as GHC hiding (varName)
import TysWiredIn as GHC (anyTyCon)
import TcEvidence as GHC (EvBind(..), TcEvBinds(..))

import GhcMonad (modifySession, withTempSession)
import GHCi (purgeLookupSymbolCache)
import NameCache (NameCache(..))
import Module (delModuleEnvList)
import Linker (unload)
import Finder (flushFinderCaches)
import HeaderInfo

import Data.Time.Clock (getCurrentTime)

import Control.Concurrent.Async
import UnliftIO.Async
import Control.Concurrent
import System.Directory (copyFile)

import qualified Data.Aeson as Aeson

import System.Environment (lookupEnv, getEnvironment)
import Text.Read (readMaybe)

import System.Mem (performGC, performMajorGC, performMinorGC)

import Data.List (sort)

type ModuleName = String
type GhcS = StateT TypesState Ghc
type ConNameMap = Map.Map String String
type DepMap = Map.Map String [String]
type DiffMap = Map.Map String Bool
type FieldsMap = [(String, String, Bool)]

data TypesState = TypeState {
      allRecFields :: [String]
    , test :: Bool 
  }

-- | Should be only used for testing
demoRefactor :: String -> String -> [String] -> String -> IO ()
demoRefactor command workingDir args gatewayName = do
  -- Run the refactoring
  runGhc (Just libdir) $ do
    -- Setup flags and environment
    initGhcFlagsForGateway
    _ <- useDirs [workingDir]
    _ <- useFlags args
    !_ <- setTempFilesFlags False

    -- create temp directory & set temp directory
    liftIO $ createDirectoryIfMissing False (workingDir ++ "/temp")
    !oldFlags <- getSessionDynFlags
    !_ <- setSessionDynFlags $ setTmpDir (workingDir ++ "/temp") oldFlags

    -- Create moduleNames for loading and fileNames for target
    let modFileNames = ["Types"]
        modNames = fmap (\x -> "Gateway." ++ gateway ++ x) modFileNames
        moduleName = modNames !! 0
        modFile = workingDir ++ (modFileNames !! 0) ++ ".hs"

    -- Debugging prints
    liftIO $ print $ "::------------------------------------------ DEBUG ------------------------------------------::"
    liftIO $ print $ ("Module Names :: " ++ show modNames)
    flags <- getSessionDynFlags
    liftIO $ print ("Import Paths :: " ++ (show $ importPaths flags))
    liftIO $ print ("Load file :: " ++ modFile ++ " , Module :: " ++ moduleName)
    liftIO $ print $ "::-------------------------------------------------------------------------------------------::"

    -- Load module and mod summary
    liftIO $ putStrLn "=========== parsed source:"
    addGatewayModuleByPath modFile moduleName
    loadModSummaries -- loadGatewayModule (workingDir ++ "Transforms.hs") "Gateway.Razorpay.Transforms"
    ms <- getModSummary (GHC.mkModuleName moduleName)
    
    -- Parse the module and get AST
    ast <- pm_parsed_source <$> parseModule ms 

    -- Debugging prints
    liftIO $ print $ "::------------------------------------------ AST ------------------------------------------::"
    
    liftIO $ print $ "::-------------------------------------------------------------------------------------------::"

    -- Start processing the AST
    -- let decls = hsmodDecls $ unLoc ast
    --     valDecls = filter valDeclsOnly decls
    --     (binds :: [HsBind GhcPs]) = valDecls ^? biplateRef
    --     functions = filter funcOnly binds
    --     finalRes = fmap getFunctionMapping functions
    
    -- liftIO $ putStrLn $ "Function Bindings = "
    -- -- liftIO $ putStrLn $ showF flags $ functions
    -- liftIO $ putStrLn $ showAst $ functions !! 6
    -- liftIO $ putStrLn $ "Final result = " ++ show finalRes

    -- liftIO $ putStrLn "============="
    -- liftIO $ putStrLn $ "Total functions = " ++ (show $ length functions)

getFunctionMapping :: HsBindLR GhcPs GhcPs -> (String, [String])
getFunctionMapping func =
  let funName = show' $ fun_id func
      matches = fun_matches func
      (MG _ mgs _) = matches
      (allMatches :: [GHC.Match GhcPs (LHsExpr GhcPs)]) = mgs ^? biplateRef
      namePats = concatMap namePatsOnly allMatches
      res = (funName, namePats)
  in res

namePatsOnly :: GHC.Match GhcPs (LHsExpr GhcPs) -> [String]
namePatsOnly (GHC.Match _ _ pats _) = concatMap fun pats
  where 
    fun pat = 
      let occNames :: [OccName] = (pat ^? biplateRef)
          occNamesString = fmap show' occNames
          lits :: [HsLit GhcPs] = (pat ^? biplateRef)
          litsString = fmap show' lits
      in occNamesString ++ litsString

valDeclsOnly :: LHsDecl GhcPs -> Bool
valDeclsOnly decl = case (unLoc decl) of
  ValD _ _ -> True
  _        -> False 

funcOnly :: HsBind GhcPs -> Bool
funcOnly bind = case bind of
      (FunBind { fun_id = id, fun_matches = GHC.MG { mg_alts = L _ [L _ (GHC.Match { m_ctxt = FunRhs { mc_strictness = SrcStrict }, m_pats = [], m_grhss = GRHSs _ [L _ (GRHS _ [] expr)] (L _ locals) })]} }) -> False -- Strict Annotation (Bang pattern) Value Binding
      (FunBind { fun_id = id, fun_matches = GHC.MG { mg_alts = L _ [L _ (GHC.Match { m_pats = [], m_grhss = GRHSs _ [L _ (GRHS _ [] expr)] (L _ locals) })]} }) -> False -- Value Binding
      (FunBind {}) -> True -- Function Binding
      _ -> False

getKeywordLoc :: TyClDecl GhcPs -> Either String (Int, Int)
getKeywordLoc (GHC.DataDecl _ (L (RealSrcSpan loc) _) _ _ _) = 
  let startLine = srcSpanStartLine loc 
      endLine   = srcSpanEndLine loc 
      startCol  = srcSpanStartCol loc 
      endCol    = srcSpanEndCol loc
    in
      if startLine == endLine && startCol <= endCol
        then Right (1, startCol - 2)
        else Left "Data Declaration, but invalid locations"
getKeywordLoc _ = Left "Not a Data Declaration, Should not have reached here"


-- Function to Apply maybe refactoring over a particular type and field
applyMaybe sourced command moduleName = do 
  transformed <- performCommand builtinRefactorings (splitOn " " command)
                                (Right ((SourceFileKey (moduleSourceFile moduleName) moduleName), sourced))
                                []
  case transformed of
    Right [change] -> do
      case change of
        ContentChanged (mod, correctlyTransformed) -> do
          -- liftIO $ putStrLn $ "=========== transformed AST (" ++ (mod ^. sfkModuleName) ++ "):"
          -- liftIO $ putStrLn $ srcInfoDebug correctlyTransformed
          -- liftIO $ putStrLn $ "=========== transformed & prettyprinted (" ++ (mod ^. sfkModuleName) ++ "):"
          -- let prettyPrinted = prettyPrint correctlyTransformed
          -- liftIO $ putStrLn prettyPrinted
          return correctlyTransformed
        ModuleRemoved mod -> do
          liftIO $ putStrLn $ "=========== module removed: " ++ mod
          return sourced
        ModuleCreated mod cont _ -> do
          liftIO $ putStrLn $ "=========== created AST (" ++ mod ++ "):"
          -- liftIO $ putStrLn $ srcInfoDebug cont
          liftIO $ putStrLn $ "=========== created & prettyprinted (" ++ mod ++ "):"
          -- let prettyPrinted = prettyPrint cont
          -- liftIO $ putStrLn prettyPrinted
          return cont 
    Right _ -> return sourced
    Left transformProblem -> do
      liftIO $ putStrLn "==========="
      liftIO $ putStrLn transformProblem
      liftIO $ putStrLn "==========="
      return sourced

-- replaceInFile :: String -> FilePath -> String
-- replaceInFile word span filePath = do
--   contents <- readFile filePath
--   let modifiedContents = unlines $ map (replaceInColumns word startCol endCol) (lines contents) 

filterDataDecls :: Decl -> Bool 
filterDataDecls (DataDecl _ _ (NameDeclHead name) (AnnList [RecordConDecl _ fields]) _) = responseTypesOnly && notStandardResponse 
  where 
    responseTypesOnly = let typ = showName name in not (isInfixOf "req" typ || isInfixOf "Req" typ)
    notStandardResponse = 
      let (allFields :: [String]) = getFieldsList fields
        in not (sort allFields == sort ["code", "status", "response"])
filterDataDecls _ = False

getFieldsList :: FieldDeclList -> [String]
getFieldsList (AnnList ls) = foldr getFields [] ls
  where 
    getFields :: FieldDecl -> [String] -> [String]
    getFields (FieldDecl (AnnList names) typ) r = fmap showName names ++ r
    getOptionalFieldsFromList _ r = r 

recTypesOnly :: Decl -> [(String, String)] -> [(String, String)]
recTypesOnly (DataDecl _ _ (NameDeclHead name) (AnnList [RecordConDecl _ fields]) _) r = 
  let typeName = showName name 
      constructorFields = getFieldsFromList fields
  in (fmap (\x -> (typeName, x)) constructorFields) ++ r
  where 
        getFieldsFromList :: FieldDeclList -> [String]
        getFieldsFromList (AnnList ls) = foldr getOptionalFieldsFromList [] ls 

        getOptionalFieldsFromList :: FieldDecl -> [String] -> [String]
        getOptionalFieldsFromList (FieldDecl (AnnList names) typ) r =
          let ((typName:_) :: [String]) = fmap showName $ typ ^? biplateRef 
              (fieldNames :: [String]) = fmap showName names 
          in if typName == "Maybe" then r 
             else fieldNames ++ r
        getOptionalFieldsFromList _ r = r 
recTypesOnly _ r = r 

-- Function write the transformation changes
applyChanges cmod mod tdir file = do
          let m = cmod 
              n = mod
              diffMode = False
          setCurrentDirectory tdir
          let newCont = prettyPrint m
              -- file = n ^. sfkFileName
          origCont <- liftIO $ withBinaryFile file ReadMode $ \handle -> do
            hSetEncoding handle utf8
            StrictIO.hGetContents handle
          let undo = createUndo 0 $ getGroupedDiff origCont newCont
          let unifiedDiff = createUnifiedDiff file origCont newCont
          when (not diffMode) $ do
            liftIO $ withBinaryFile file WriteMode $ \handle -> do
              hSetEncoding handle utf8
              hPutStr handle newCont
              hFlush handle
          return (undo, unifiedDiff, origCont)

-- Function to get the contents from the modules
getContentsFromModules cmod mod =
  let newCont = prettyPrint cmod
      origCont = prettyPrint mod 
  in (origCont, newCont)

-- Function write the transformation changes in memory only
applyChanges' cmod target = do
          let newCont = stringToStringBuffer $ prettyPrint cmod
          utcTime <- liftIO $ getCurrentTime
          removeTarget $ targetId target 
          let target' = target {targetContents = Just (newCont, utcTime)}
          addTarget target'

-- function to write the changed code to the file
writeToFile tdir file str = do 
  setCurrentDirectory tdir
  liftIO $ withBinaryFile file WriteMode $ \handle -> do
              hSetEncoding handle utf8
              hPutStr handle str
              hFlush handle
  return ()

-- | Creates a compressed set of changes in one file
createUndo :: Eq a => Int -> [Diff [a]] -> [(Int, Int, [a])]
createUndo i (Both str _ : rest) = createUndo (i + length str) rest
createUndo i (First rem : Second add : rest)
  = (i, i + length add, rem) : createUndo (i + length add) rest
createUndo i (First rem : rest) = (i, i, rem) : createUndo i rest
createUndo i (Second add : rest)
  = (i, i + length add, []) : createUndo (i + length add) rest
createUndo _ [] = []

-- Returns a pair of boolean (isInCached, cachedRes)
isCachedField :: (String, String) -> FieldsMap -> (Bool, Bool)
isCachedField _ [] = (False, False)
isCachedField field@(typeName, fieldName) ((typeName', fieldName', res) : rem) = 
  if typeName == typeName' && fieldName == fieldName' 
    then (True, res)
    else isCachedField field rem

getCachedFieldsMap :: String -> String -> IO FieldsMap
getCachedFieldsMap workingDir gatewayName = do 
    filePath' <- lookupEnv "HT_GATEWAY_CACHE"
    let filePath = fromMaybe workingDir filePath'
    cachedFieldsMapStr <- withBinaryFile (filePath ++ "/CachedFields_" ++ gatewayName ++ ".txt") ReadWriteMode $ \handle -> do
                                                                            hSetEncoding handle utf8
                                                                            BS.hGetContents handle
    let (cachedFieldsMap :: FieldsMap) = case Aeson.decodeStrict (cachedFieldsMapStr) of
                                Just res -> res 
                                Nothing  -> trace ("Unable to decode") [] 
    return cachedFieldsMap           

-- Update the fieldsMap in the file after updating the values if already exist
updateCachedFieldsFile :: String -> String -> FieldsMap -> IO ()
updateCachedFieldsFile workingDir gatewayName fieldsMap = do 
  cachedFieldsMap <- getCachedFieldsMap workingDir gatewayName
  let updatedFieldsMap = updateFieldsMap cachedFieldsMap fieldsMap
  filePath' <- lookupEnv "HT_GATEWAY_CACHE"
  let filePath = fromMaybe workingDir filePath'
  withBinaryFile (filePath ++ "/CachedFields_" ++ gatewayName ++ ".txt") WriteMode $ \handle -> do
                                                                            hSetEncoding handle utf8
                                                                            BSL.hPut handle (Aeson.encode updatedFieldsMap) 

-- Function to update the cachedFieldsMap as per the newFieldsMap
updateFieldsMap :: FieldsMap -> FieldsMap -> FieldsMap
updateFieldsMap oldFieldsMap newFieldsMap = foldr addUpdateIfNotExist oldFieldsMap newFieldsMap
  where 
     addUpdateIfNotExist :: (String, String, Bool) -> FieldsMap -> FieldsMap 
     addUpdateIfNotExist field@(typeName, fieldName, isUsed) [] = [field] 
     addUpdateIfNotExist field@(typeName, fieldName, _) (field'@(typeName2, fieldName2, _) : xs) = 
      if typeName == typeName2 && fieldName == fieldName2
        then field  : xs 
        else field' : addUpdateIfNotExist field xs
      -- if ((typeName, fieldName, True) `elem` res) || ((typeName, fieldName, False) `elem` res)
      --   then updateVal field res
      --   else field : res 

-- | Creates a unified-style diff of two texts. Only used when the user wants to know what would change.
createUnifiedDiff :: FilePath -> String -> String -> String
createUnifiedDiff name left right
  = render $ prettyContextDiff (PP.text name) (PP.text name) PP.text $ getContextDiff 3 (lines left) (lines right)

deriving instance Generic SrcSpan
deriving instance Generic (NodeInfo sema src)
instance Show AvailInfo where show = showSDocUnsafe . ppr

forcedTypecheck :: ModSummary -> ParsedModule -> Ghc (Maybe RenamedSource, TypecheckedSource)
forcedTypecheck ms p = do
  env <- getSession
  store <- liftIO $ newIORef (error "not found")
  let hpm = HsParsedModule (pm_parsed_source p) (pm_extra_src_files p) (pm_annotations p)
  tcRes <- liftIO $ runTcInteractive env $ (,) <$> getGblEnv <*> getLclEnv
  case tcRes of
    (_, Just (gblEnv, lclEnv)) -> do
      let finalizeModule = do gbl <- getGblEnv
                              liftIO $ writeIORef store ( (,,,) <$> tcg_rn_decls gbl
                                                                <*> return (tcg_rn_imports gbl)
                                                                <*> return (tcg_rn_exports gbl)
                                                                <*> return (tcg_doc_hdr gbl)
                                                        , tcg_binds gbl)
      -- liftIO $ modifyIORef (tcg_th_modfinalizers gblEnv) (finalizeModule :)
      let gblEnv' = gblEnv { tcg_rn_exports = Just [], tcg_rn_decls = Just emptyRnGroup }
      liftIO $ initTcRnIf 'a' env gblEnv' lclEnv $ void (tcRnModuleTcRnM env ms hpm (ms_mod ms, getLoc (pm_parsed_source p)))
                                                     `gcatch` \(_ :: SomeException) -> return ()
      liftIO $ readIORef store
    _ -> error "forcedTypecheck: runTcInteractive failed" 


addGatewayModuleByPath :: FilePath -> ModuleName -> Ghc Target
addGatewayModuleByPath filePath moduleName
  = do 
       target <- guessTarget filePath Nothing
       addTarget target
       return target

addGatewayModule :: FilePath -> ModuleName -> Ghc Target
addGatewayModule filePath moduleName
  = do 
       setDirs [filePath]
       target <- guessTarget moduleName Nothing
       addTarget target
       return target

loadAllTargets :: Ghc ()
loadAllTargets = do 
  void $ load LoadAllTargets
  return ()

loadModSummaries :: Ghc ()
loadModSummaries = depanal [] False >> return ()

-- | Load the summary of a module given by the working directory and module name.
loadGatewayModule :: FilePath -> ModuleName -> Ghc ModSummary
loadGatewayModule filePath moduleName
  = do 
       target <- guessTarget filePath Nothing
       addTarget target
       void $ load (LoadUpTo $ GHC.mkModuleName moduleName)
       targets <- getTargets
       liftIO $ print ("Target = " ++ (show' targets))
       getModSummary $ GHC.mkModuleName moduleName

initGhcFlagsForGateway :: Ghc ()
initGhcFlagsForGateway = do 
                         initGhcFlagsForGateway' False True
                         dfs <- getSessionDynFlags
                         void $ setSessionDynFlags $ dfs { hscTarget = HscNothing
                                                        --  , pluginModNames = pluginModNames dfs ++ [mkModuleName "Data.Record.Anon.Plugin", mkModuleName "RecordDotPreprocessor"]
                                                          }

setTempFilesFlags :: Bool -> Ghc ()
setTempFilesFlags shouldSet = do 
  dflags <- getSessionDynFlags
  let setOrUnset = if shouldSet then gopt_set else gopt_unset
  void $ setSessionDynFlags
    $ flip setOrUnset Opt_KeepHcFiles
    $ flip setOrUnset Opt_KeepHscppFiles
    $ flip setOrUnset Opt_KeepSFiles
    $ flip setOrUnset Opt_KeepTmpFiles
    $ flip setOrUnset Opt_KeepHiFiles
    $ flip setOrUnset Opt_KeepOFiles
    $ dflags

-- | Sets up basic flags and settings for GHC
initGhcFlagsForGateway' :: Bool -> Bool -> Ghc ()
initGhcFlagsForGateway' needsCodeGen errorsSuppressed = do
  dflags <- getSessionDynFlags
  void $ setSessionDynFlags
    $ flip gopt_set Opt_KeepRawTokenStream
    $ flip gopt_set Opt_NoHsMain
    $ flip gopt_set Opt_BreakOnError
    $ flip gopt_set Opt_BreakOnException
    $ flip gopt_unset Opt_WriteInterface
    $ flip gopt_unset Opt_WriteHie
    $ (if errorsSuppressed then
                                    flip gopt_set Opt_DeferTypedHoles
                                  . flip gopt_set Opt_DeferTypeErrors
                                  . flip gopt_set Opt_DeferOutOfScopeVariables
                           else id)
    $ dflags { importPaths = []
            --  , maxErrors = Nothing
             , parMakeCount = Nothing
             , hscTarget = if needsCodeGen then HscInterpreted else HscNothing
             , ghcLink = if needsCodeGen then LinkInMemory else NoLink
             , ghcMode = CompManager
             , packageFlags = ExposePackage "template-haskell" (PackageArg "template-haskell") (ModRenaming True []) : packageFlags dflags
            --  , pluginModNames = pluginModNames dflags ++ [mkModuleName "Data.Record.Anon.Plugin", mkModuleName "RecordDotPreprocessor"]
             }

validateChanges :: [String] -> [FilePath] -> Ghc Bool 
validateChanges [] [] = pure True
validateChanges (modName : modNames) (file : files) = do
  liftIO $ putStrLn $ "Typecheck " ++ modName 
  isTypeChecked <- typeCheck (modName, file)
  if isTypeChecked
    then validateChanges modNames files 
    else pure False 

typeCheck :: (String, FilePath) -> Ghc Bool
typeCheck (moduleName, modFile) = do
    -- Debugging prints
    flags <- getSessionDynFlags
    liftIO $ print ("Import Paths :: " ++ (show $ importPaths flags))
    liftIO $ print ("Load file :: " ++ modFile ++ " , Module :: " ++ moduleName)

    -- Load module
    -- ms <- loadGatewayModule modFile moduleName
    ms <- getModSummary $ GHC.mkModuleName moduleName

    -- Update session Dyn Flags for plugins
    flags <- return $ ms_hspp_opts ms
      
    p' <- withTempSession (addPluginFlags flags) $
      do 
        -- Initialize plugins
        dflags <- getSessionDynFlags
        hsc_env <- getSession
        dflags <- liftIO (initializePlugins hsc_env dflags)
        ms' <- pure $ modSumNormalizeFlags $ ms { ms_hspp_opts = dflags }

        p' <- parseModule ms'
        return p'
    
    ((\_ -> True) <$> typecheckModule p')
                         `gcatch` \(e :: SourceError) -> trace ("Some exception : " ++ show e) (return False)

clearModules :: [ModSummary] -> Ghc ()
clearModules [] = return ()
clearModules mods = do
  let reachableMods = map ms_mod_name mods
      notReloaded = (`notElem` reachableMods) . GHC.moduleName . mi_module . hm_iface
  env <- getSession
  let hptStay = filterHpt notReloaded (hsc_HPT env)
  -- clear the symbol cache for iserv
  liftIO $ purgeLookupSymbolCache env
  -- clear the global linker state
  dfs <- getSessionDynFlags
  when (not $ gopt Opt_ExternalInterpreter dfs) $
    liftIO $ unload env (mapMaybe hm_linkable (eltsHpt hptStay))
  -- clear name cache
  nameCache <- liftIO $ readIORef $ hsc_NC env
  let nameCache' = nameCache { nsNames = delModuleEnvList (nsNames nameCache) (map ms_mod mods) }
  liftIO $ writeIORef (hsc_NC env) nameCache'
  -- clear home package table and module graph
  modifySession (\s -> s { hsc_HPT = hptStay
                                , hsc_mod_graph = mkModuleGraph $ filter ((`notElem` reachableMods) . ms_mod_name) (mgModSummaries $ hsc_mod_graph s)
                                })
  ss <- getSession
  liftIO $ flushFinderCaches ss

getAllTypesDepsMap :: HT.Module -> [String] -> Ghc DepMap
getAllTypesDepsMap source recTypesNames = do
  let (dataDecls :: [Decl]) = source ^? biplateRef
      (conTypes :: [Decl]) = filter conDeclTypeHavingRecTypes dataDecls
      (conTypesNames :: [String]) = fmap (\(DataDecl _ _ (NameDeclHead name) _ _) -> showName name) conTypes
      (conDecls :: [HT.ConDecl]) = conTypes ^? biplateRef
      conNameMap = Map.fromList $ foldr getConNameTypeMap [] conDecls
      fromJSONInstances = getJSONInstances conTypesNames
      orderedListFromInstancesConst = getOrderedListFromInstances fromJSONInstances conNameMap False
      orderedListFromInstances = getOrderedListFromInstances fromJSONInstances conNameMap True
      (depMap :: DepMap) = orderedListToMap orderedListFromInstances
  !_ <- liftIO $ putStrLn ""
  !_ <- liftIO $ putStrLn $ "Con Data Type Names = " ++ show conTypesNames
  !_ <- liftIO $ putStrLn ""
  !_ <- liftIO $ putStrLn $ "Con Name-Type Map   = " ++ show conNameMap
  -- !_ <- liftIO $ putStrLn ""
  -- !_ <- liftIO $ putStrLn "" >> putStrLn $ "Instance decls = " ++ foldr (\x r -> srcInfoDebug x ++ r) "" fromJSONInstances
  !_ <- liftIO $ putStrLn ""
  !_ <- liftIO $ putStrLn $ "Instance decls ordered names list (Constructor Names) = " ++ show orderedListFromInstancesConst
  !_ <- liftIO $ putStrLn ""
  !_ <- liftIO $ putStrLn $ "Instance decls ordered names list (Types Names) = " ++ show orderedListFromInstances
  return depMap

  where 
    conDeclTypeHavingRecTypes :: Decl -> Bool 
    conDeclTypeHavingRecTypes (DataDecl _ _ _ (AnnList cons) _) = any checkInRecTypesNames cons 
    conDeclTypeHavingRecTypes _ = False 

    checkInRecTypesNames :: HT.ConDecl -> Bool 
    checkInRecTypesNames (ConDecl name (AnnList types)) = any checkTypeInRecTypeNames types
    checkInRecTypesNames _ = False

    checkTypeInRecTypeNames :: HT.Type -> Bool 
    checkTypeInRecTypeNames typ = let (names :: [HT.Name]) = typ ^? biplateRef in any (\n -> (showName n) `elem` recTypesNames) names

    getConNameTypeMap :: HT.ConDecl -> [(String, String)] -> [(String, String)]
    getConNameTypeMap (ConDecl name (AnnList types)) res = 
      if length types <= 0
      then res
      else 
        let conName = showName name 
            (typeNames :: [HT.Name]) = (head types) ^? biplateRef
            conType = (showName . head) typeNames
        in (conName, conType) : res

    getJSONInstances :: [String] -> [Decl]
    getJSONInstances dataNames = filter (isJust . getInstanceDecl dataNames) (source ^? biplateRef)

    getInstanceDecl :: [String] -> Decl -> Maybe Decl 
    getInstanceDecl dataNames decl@(InstanceDecl (InstanceRule _ _ (AppInstanceHead fun arg)) body) = 
      if (showName $ head $ fun ^? biplateRef) == "FromJSON" && (showName $ head $ arg ^? biplateRef) `elem` dataNames
      then Just decl 
      else Nothing
    getInstanceDecl _ _ = Nothing

    getOrderedListFromInstances :: [Decl] -> ConNameMap -> Bool -> [[String]]
    getOrderedListFromInstances instances conNameMap convert = fmap (getNamesFromInstances conNameMap convert) instances

    getNamesFromInstances :: ConNameMap -> Bool -> Decl -> [String]
    getNamesFromInstances conNameMap convert (InstanceDecl _ body) =
      if convert
      then fmap (fromJust . flip Map.lookup conNameMap . showName) $ filter (\x -> isJust $ Map.lookup (showName x) conNameMap) (body ^? biplateRef)
      else fmap showName $ filter (\x -> isJust $ Map.lookup (showName x) conNameMap) (body ^? biplateRef)

orderedListToMap :: [[String]] -> DepMap 
orderedListToMap = foldr lsToMap mempty
  where 
    lsToMap :: [String] -> DepMap -> DepMap
    lsToMap (x : xs) depMap = Map.insertWith (++) x xs $ lsToMap xs depMap
    lsToMap [] depMap = depMap 

addPluginFlags :: DynFlags -> HscEnv -> HscEnv 
addPluginFlags flags env = let dflags = hsc_dflags env in 
                    env {hsc_dflags = dflags {pluginModNames = pluginModNames flags, pluginModNameOpts = pluginModNameOpts flags}}

isUsedField :: (String, String, String) -> [(String, String, Bool)] -> Bool 
isUsedField _ [] = False 
isUsedField field@(typeName, fieldName, fieldType) ((typ, fld, res) : xs) = if typeName == typ && fieldName == fld then res else isUsedField field xs

isDistinguishable :: String -> DiffMap -> Bool 
isDistinguishable typ diffMap = fromMaybe True $ Map.lookup typ diffMap

canDistinguish :: HT.Module -> FieldsMap -> String -> String -> DiffMap -> DiffMap
canDistinguish source fieldsMap typeA typeB res = 
  let recTypes = getRecordTypes source in
    if typeA `elem` recTypes && typeB `elem` recTypes -- if not primitive types
      then 
        let typeA' = getDeclFromName source typeA -- get from source the decl with typeA name 
            typeB' = getDeclFromName source typeB -- get from source the decl with typeB name 
        in 
          if checkCriteriaOne typeA' typeB' fieldsMap -- if extra *REQUIRED* field is there in typeA
            then Map.insertWith (&&) typeA True res -- then this type can be distinguished as of now
            else -- now this type can have either same fields with different types or fields without any distinctor, these cases are needed to be logged
              let 
                emptyMap = Map.singleton typeA False 
                tempRes = getCriteriaTwo source fieldsMap typeA' typeB' emptyMap -- use canDistinguish for all the fields present in both 
              in Map.unionWith (&&) tempRes res
    else res -- if primitive types

checkCriteriaOne :: Decl -> Decl -> FieldsMap ->  Bool 
checkCriteriaOne typeA typeB fieldsMap = 
  let fieldsA     = getFieldsFromDecl typeA
      fieldsB     = getFieldsFromDecl typeB
      fieldsOnlyA = snd3 <$> fieldsA
      fieldsOnlyB = snd3 <$> fieldsB
  in any (notInBAndIsUsed fieldsOnlyB) fieldsA-- get fields and check if any extra field is there which is usedField
    where 
      notInBAndIsUsed :: [String] -> (String, String, String) -> Bool 
      notInBAndIsUsed fieldsOnlyB field@(_, fieldName, _) = (fieldName `notElem` fieldsOnlyB) && isUsedField field fieldsMap

getCriteriaTwo :: HT.Module -> FieldsMap -> Decl -> Decl -> DiffMap -> DiffMap 
getCriteriaTwo source fieldsMap typeA typeB diffMap = 
  let fieldsA     = getFieldsFromDecl typeA
      fieldsB     = getFieldsFromDecl typeB
      fieldsOnlyA = snd3 <$> fieldsA
      fieldsOnlyB = snd3 <$> fieldsB
  in foldr (isInBAndHasDistinguishableType fieldsA fieldsB fieldsOnlyB) diffMap fieldsOnlyA
    where 
      isInBAndHasDistinguishableType :: [(String, String, String)] -> [(String, String, String)] -> [String] -> String -> DiffMap -> DiffMap
      isInBAndHasDistinguishableType fieldsA fieldsB fieldsOnlyB fieldName res = 
        if fieldName `elem` fieldsOnlyB
          then canDistinguish source fieldsMap (getFieldType fieldsA fieldName) (getFieldType fieldsB fieldName) res
        else res

      getFieldType :: [(String, String, String)] -> String -> String
      getFieldType [] _ = ""
      getFieldType ((typeName, fieldName, fieldType) : xs) field = if fieldName == field then fieldType else getFieldType xs field

getDistinguishableMap :: HT.Module -> DepMap -> FieldsMap -> DiffMap
getDistinguishableMap source depMap fieldsMap = Map.foldrWithKey evaluateDistinguishablePrimary mempty depMap
  where 
    evaluateDistinguishablePrimary :: String -> [String] -> DiffMap -> DiffMap 
    evaluateDistinguishablePrimary typeName deps res = 
      let
        emptyMap = Map.singleton typeName True
        tempRes = foldr (canDistinguish source fieldsMap typeName) emptyMap deps 
      in trace ("evaluateDistinguishablePrimary :: " ++ typeName ++ " with deps " ++ show deps ++ " with res = " ++ show tempRes) $ Map.unionWith (&&) tempRes res

getDeclFromName :: HT.Module -> String -> Decl 
getDeclFromName source name = 
  let (decls :: [Decl]) = source ^? biplateRef
  in head $ filter (\x -> getDeclName x == name) decls 

getDeclName :: Decl -> String 
getDeclName (DataDecl _ _ (NameDeclHead name) _ _) = showName name 
getDeclName _ = ""

getFieldsFromDecl :: Decl -> [(String, String, String)] -- returns (typeName, fieldName, fieldType)
getFieldsFromDecl (DataDecl _ _ (NameDeclHead name) (AnnList [RecordConDecl _ (AnnList ls)]) _) = fmap (\(x,y) -> (showName name, x, y)) $ foldr (\x r -> getFieldsFromFieldDecl x ++ r) [] ls
  where 
        getFieldsFromFieldDecl :: FieldDecl -> [(String, String)]
        getFieldsFromFieldDecl (FieldDecl (AnnList names) typ) =
          let ((typName:_) :: [String]) = fmap showName $ typ ^? biplateRef 
              (fieldNames :: [String]) = fmap showName names 
          in fmap (,typName) fieldNames 
        getOptionalFieldsFromList _ = []
getFieldsFromDecl _ = []

getRecordTypes :: HT.Module -> [String]
getRecordTypes source = 
  let (decls :: [Decl]) = source ^? biplateRef
      (recordDecls :: [Decl]) = filter isRecordType decls
  in getDeclName <$> recordDecls

isRecordType :: Decl -> Bool 
isRecordType (DataDecl _ _ (NameDeclHead _) (AnnList [RecordConDecl _ _]) _) = True 
isRecordType _ = False

setDirs :: [FilePath] -> Ghc ()
setDirs workingDirs = do
  dynflags <- getSessionDynFlags
  void $ setSessionDynFlags dynflags { importPaths = workingDirs }