{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}

module ModuleParser where

-- import Retrie
import Debug.Trace
import Data.Data
import GHC hiding (loadModule)
import qualified GHC
import GHC.Utils.Outputable (ppr, showSDocUnsafe,Outputable(..))
import GHC.Unit.Module.Graph (mgModSummaries')
import GHC.Paths ( libdir )
import Control.Monad
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class
import GHC.Driver.Session
import qualified Data.HashMap.Strict as HM
import Data.List as List
import Data.List.Extra (splitOn,trim,replace, cons)
import GHC.LanguageExtensions
import qualified Language.Haskell.GHC.ExactPrint as E
import qualified Data.Text as T
import Text.CSV
import Data.Csv
import qualified Data.ByteString.Lazy as BSL

useDirs :: [FilePath] -> Ghc ()
useDirs workingDirs = do
  dynflags <- getSessionDynFlags
  void $ setSessionDynFlags dynflags { importPaths = importPaths dynflags ++ workingDirs }

initGhcFlags :: Ghc ()
initGhcFlags = initGhcFlags' False True

initGhcFlags' :: Bool -> Bool -> Ghc ()
initGhcFlags' needsCodeGen errorsSuppressed = do
  dflags <- getSessionDynFlags
  void $ setSessionDynFlags
    $ flip gopt_set Opt_KeepRawTokenStream
    $ flip gopt_set Opt_NoHsMain
    $ (if errorsSuppressed then flip gopt_set Opt_DeferTypeErrors
                                  . flip gopt_set Opt_DeferTypedHoles
                                  . flip gopt_set Opt_DeferOutOfScopeVariables
                           else id)
    $ foldl' (\acc x -> xopt_set acc x) (dflags { importPaths = []
            --  , hscTarget = if needsCodeGen then HscInterpreted else HscNothing
             , ghcLink = if needsCodeGen then LinkInMemory else NoLink
             , ghcMode = CompManager
             , packageFlags = ExposePackage "template-haskell" (PackageArg "template-haskell") (ModRenaming True []) : packageFlags dflags
            --  , pluginModNames = pluginModNames dflags ++ [mkModuleName "Data.Record.Anon.Plugin", mkModuleName "RecordDotPreprocessor"]
             }) [
                                BangPatterns
                                ,BlockArguments
                                ,ConstraintKinds
                                -- ,Cpp
                                ,DataKinds
                                ,DeriveAnyClass
                                ,DeriveDataTypeable
                                ,DeriveFoldable
                                ,DeriveFunctor
                                ,DeriveGeneric
                                ,DeriveTraversable
                                ,DerivingStrategies
                                ,DerivingVia
                                ,DuplicateRecordFields
                                ,EmptyCase
                                ,ExplicitForAll
                                ,ExplicitNamespaces
                                ,FlexibleContexts
                                ,FlexibleInstances
                                ,GADTs
                                ,GeneralizedNewtypeDeriving
                                ,ImplicitParams
                                ,ImplicitPrelude
                                ,ImportQualifiedPost
                                ,InstanceSigs
                                ,KindSignatures
                                ,LambdaCase
                                ,MagicHash
                                ,MultiParamTypeClasses
                                ,MultiWayIf
                                ,OverloadedLabels
                                ,OverloadedStrings
                                ,PatternSynonyms
                                ,QuasiQuotes
                                ,RankNTypes
                                ,RecordWildCards
                                ,ScopedTypeVariables
                                ,TemplateHaskell
                                ,TupleSections
                                ,TypeApplications
                                ,TypeFamilies
                                ,TypeOperators
                                ,TypeSynonymInstances
                                ,UndecidableInstances
                                ,ViewPatterns
                                ,GHC.LanguageExtensions.UnicodeSyntax
                          ]

initGhcFlagsForTest :: Ghc ()
initGhcFlagsForTest = do initGhcFlags' True False
                         dfs <- getSessionDynFlags
                         void $ setSessionDynFlags dfs

loadModule :: FilePath -> String -> Ghc ModSummary
loadModule workingDir moduleName
  = do initGhcFlagsForTest
       useDirs [workingDir]
       target <- guessTarget moduleName Nothing
       setTargets [target]
      --  void $ load (LoadUpTo $ mkModuleName moduleName)
       depanal [] True
       getModSummary $ mkModuleName moduleName

show' :: HsDecl GhcPs -> String
show' = showSDocUnsafe . ppr

keepevery :: [a] -> Int -> [a]
keepevery xs n = map snd $ filter (\(i, _) -> i `mod` n == 0) $ zip [1..] xs

punctuations :: String
punctuations = "()"

removePunctuation :: String -> String
removePunctuation [] = []
removePunctuation (ch : str) = if ch `elem` punctuations then removePunctuation str else ch : removePunctuation str

makeFnAndBody :: [(String, [[HsDecl GhcPs]])] -> ([String], [String])
makeFnAndBody groupedFunctions =
  let functions    = fmap fst groupedFunctions
      functionBody = fmap show' (keepevery (concat (fmap (concat . snd) groupedFunctions)) 2)
  in (functions, functionBody)

isWordInSentence :: String -> String -> Bool
isWordInSentence word sent = any (\x -> word == x || show word == x) $ (concat $ fmap words (tails sent))
  where
      tails [] = []
      tails [x] = [[x]]
      tails xs@(_:ts) = xs : tails ts

isWordInSentenceV2 :: String -> String -> Bool
isWordInSentenceV2 word sentence = any (\x -> word == x || show word == x) $ foldr (\x r -> splitOn "." x ++ r) [] $ removePunctuation <$> words sentence

returnGatewaysExistsInFn :: [String] -> String -> [String]
returnGatewaysExistsInFn [] _       = []
-- returnGatewaysExistsInFn (x : xs) y = if isWordInSentence x y then getGwName x : returnGatewaysExistsInFn xs y
returnGatewaysExistsInFn (x : xs) y = if isWordInSentenceV2 x y then getGwName x : returnGatewaysExistsInFn xs y
                                      else returnGatewaysExistsInFn xs y

-- utilFn :: ([String], [String]) -> [(String, [String])] -> IO [(String, [String])]
-- utilFn ([], _) res              = pure res
-- utilFn ((x : xs), (y : ys)) res = do 
--                                     print ("Function : " <> x)
--                                     let ls = returnGatewaysExistsInFn gateways y
--                                         newRes = (x, ls) : res
--                                     print ls
--                                     utilFn (xs, ys) newRes

utilFn :: ([String], [String]) -> [(String, [String])] -> IO [(String, [String])]
utilFn ([], _) res              = pure res
utilFn ((x : xs), (y : ys)) res = do
                                    print ("Function : " <> x)
                                    let ls = returnGatewaysExistsInFn gateways y
                                        newRes = (x, ls) : res
                                    print ls
                                    utilFn (xs, ys) newRes

-- :set +s
runFn :: String -> String -> IO ()
runFn path mod = do
                (x, _, _, _) <- moduleParser path mod
                let fnAndBodyPair = makeFnAndBody x
                !res   <- utilFn fnAndBodyPair []
                !final <- pure $ fmap (\(f, s) -> (f, show s)) res
                writeFile "/Users/piyush.garg/project-code-to-doc/output/output.txt" (show res)
                BSL.writeFile "/Users/piyush.garg/project-code-to-doc/output/gateway.csv" $ encode final

moduleParser :: String -> String -> IO ([(String, [[HsDecl GhcPs]])], [HsDecl GhcPs], [ImportDecl GhcPs], ParsedSource)
moduleParser modulePath moduleName = do
    print modulePath
    dflags <- runGhc (Just libdir) getSessionDynFlags
    modSum <- runGhc (Just libdir) $ loadModule modulePath moduleName
    y <- runGhc (Just libdir) $ parseModule modSum

    let (functions, nonFunctions) = foldl' (\acc res -> do
            if isFunction res
              then ((fst acc <> [(unLoc res)]),snd acc)
              else ((fst acc),snd acc <> [(unLoc res)])
            ) ([],[]) (hsmodDecls $ unLoc $ pm_parsed_source y)

        groupedFunctions = groupBy (\a b -> isFunSig a && isFunVal b ) functions
        groupFunctionsMore = groupByUltimate groupedFunctions --unionBy (\a b -> getInfixlFun a b || getInfixlFun b a) (groupedFunctions)
        importList = unLoc <$> (hsmodImports $ unLoc $ pm_parsed_source y)

    -- print $ showSDocUnsafe $ ppr groupedFunctions
    -- print $ showSDocUnsafe $ ppr groupFunctionsMore

    -- writeFile "/Users/salokya.kumar/Work/haskell-tool/sample/app/output.txt" $ show $ fmap (\(n, m) -> (n, showSDocUnsafe $ ppr m)) groupFunctionsMore

    -- Returns Fn name in 1D array
    -- let fnList = fmap fst groupFunctionsMore
    -- writeFile "/Users/salokya.kumar/Work/haskell-tool/sample/app/fns.txt" $ show fnList

    -- Return Fn body in 1D array
    -- let fn1 = fmap (concat . snd) groupFunctionsMore
    -- writeFile "/Users/salokya.kumar/Work/haskell-tool/sample/app/temp.txt" $ show' $ (keepevery (concat fn1) 2)

    -- let fnsAndBody = makeFnAndBody groupFunctionsMore

    pure $ (groupFunctionsMore, nonFunctions, importList, pm_parsed_source y)

    where
      getInfixlFun fun fun1 =
        let funShow = replace "]" "" $ replace "[" "" $ showSDocUnsafe $ ppr fun
            funShow1 = showSDocUnsafe $ ppr fun1
        in if any (\x -> isInfixOf x funShow) ["infixr", "infixl", "infix","NOINLINE","INLINE"]  then any (\x -> isInfixOf (trim $ traceShowId x) $ traceShowId $ funShow1) (traceShowId $ splitOn " " funShow) else False

isFunction :: _ -> Bool
isFunction (L _ (SigD _ sigDecls)) = True
isFunction (L _ (ValD _ valDecls)) = True
isFunction _ = False

isFunSig :: _ -> Bool
isFunSig (SigD _ sigDecls) = True
isFunSig _ = False

isFunVal :: _ -> Bool
isFunVal (ValD _ valDecls) = True
isFunVal _ = False

getFunctionName :: _ -> String
getFunctionName str =
  if any (\x -> x `isInfixOf` str) ["infixl", "infixr", "infix", "INLINE", "NOINLINE"]
    -- infixl 7 /
    then (splitOn " " $  replace "]" "" $ replace "[" "" str) !! 2
    else head . splitOn " " . replace "(" "" . replace ")" "" . replace "]" "" $ replace "[" "" $ str

groupByUltimate :: [[HsDecl GhcPs]] -> [(String,[[HsDecl GhcPs]])]
groupByUltimate = (HM.toList . foldl' (\acc x -> addToBucket acc x) HM.empty)
  where
    addToBucket :: HM.HashMap String [[HsDecl GhcPs]] -> [HsDecl GhcPs] -> HM.HashMap String [[HsDecl GhcPs]]
    addToBucket acc el =
      let funcName = getFunctionName $ showSDocUnsafe $ ppr $ el
      in (HM.insert funcName $
              case HM.lookup funcName acc of
                Just x -> x ++ [el]
                _ -> [el]
          ) acc


exceptionsGW :: [(String, String)]
exceptionsGW = [("PAYTMV2", "PAYTM_V2"), ("CASHFREE", "GOCASHFREE"), ("OLA","OLAPOSTPAID"), ("PAYUVoid", "PAYU"), ("CASHFREEVoid", "GOCASHFREE"), ("TPSLVoid", "TPSL"), ("RAZORPAYVoid", "RAZORPAY"), ("SimplCancelResponse", "SIMPL"), ("AMAZONPAYVoid", "AMAZONPAY"), ("PAYUCapture", "PAYU"), ("CASHFREECapture", "GOCASHFREE"), ("TPSLCapture", "TPSL"), ("RAZORPAYCapture", "RAZORPAY"), ("SimplClaimResponse", "SIMPL"), ("AMAZONPAYCapture", "AMAZONPAY"), ("PAYUAUTH", "PAYU"), ("RAZORPAYAUTH", "RAZORPAY"), ("PAYTMV2AUTH", "PAYTM_V2"), ("AMAZONAUTH", "AMAZONPAY"), ("FREECHARGEAUTH", "FREECHARGE"), ("PAYTMWALLETAUTHV2", "PAYTM_V2"), ("OLAAUTH", "OLAPOSTPAID"), ("PAYPALAUTH", "PAYPAL"), ("SIMPLAUTH", "SIMPL"), ("PHONEPEAUTH", "PHONEPE"), ("LAZYPAYAUTH", "LAZYPAY"), ("PAYTMWALLETAUTH", "PAYTM"), ("AIRTELMONEYAUTH", "AIRTELMONEY"), ("AMAZONPAYAUTH", "AMAZONPAY"), ("MOBIKWIKAUTH", "MOBIKWIK"), ("LOANTAPAUTH", "LOANTAP"), ("DUMMYAUTH", "DUMMY")]

normalGW :: [String]
normalGW = ["ADYEN","AFTERPAY","AIRPAY","AIRTELMONEY","AMAZONPAY","AMEX","ATOM","AUTHORIZEDOTNET","AXIS","AXIS_BIZ","AXIS_UPI","AXISNB","BAJAJFINSERV","BHARATX","BILLDESK","BLAZEPAY","BRAINTREE","CAMSPAY","CAPITALFLOAT","CAREEMPAY","CASH","CASHTOCODE","CCAVENUE","CCAVENUE_V2","CHECKOUT","CITI","CITRUS","CRED","CYBERSOURCE","CYBS_CHASE","DLOCAL","DUMMY","EARLYSALARY","EASEBUZZ","EBS","EBS_V3","EPAYLATER","FDCONNECT","FINBOX","FLEXMONEY","FREECHARGE","FREECHARGE_V2","FSS_ATM_PIN","FSS_ATM_PIN_V2","FSSPAY","GOCASHFREE","GOKWIK","GOOGLEPAY","HDFC","HDFC_DC_EMI","HDFC_CC_EMI","HDFC_EBS_VAS","HDFC_FLEXIPAY","HDFC_IVR","HDFC_UPI","HDFCNB","HSBC_UPI","HYPERPG","ICICI","ICICI_UPI","ICICINB","ICICIPAYLATER","INDUS_UPI","INSTACRED","INDUS_PAYU","IPG","ITZCASH","JIOMONEY","KLARNA","KOTAK","KOTAK_UPI","LAZYPAY","LINEPAY","LOANTAP","LOTUSPAY","LOYLTYREWARDZ","LSP","LYRA","M2P","MERCHANT_CONTAINER","MIGS","MOBIKWIK","MOBIKWIKZIP","MORPHEUS","MPESA","MPGS","MSWIPE","NOON","NONE","OLAMONEY","OLAPOSTPAID","ONEPAY","PAYFORT","PAYGLOCAL","PAYLATER","PAYPAL","PAYTM","PAYTM_UPI","PAYTM_V2","PAYU","PAYZAPP","PHONEPE","PINELABS","POSTPAY","QWIKCILVER","RAZORPAY","RBL","RBL_BIZ","SBI","SBI_UPI","SBIBUDDY","SEZZLE","SHOPSE","SIMPL","SNAPMINT","SODEXO","STRIPE","TABBY","TATA_UPI","TATANEU","TATAPAY","TATAPAYLATER","TPSL","TPSL_SI","TRUSTPAY","TWID","VIJAYA_UPI","VOLT","WORLDPAY","YES_BIZ","YESBANK_UPI","ZAAKPAY","ZESTMONEY"]


-- We can import Euler.Types.Gateway to make this array dynamic
gateways :: [String]
gateways = fmap fst exceptionsGW ++ normalGW

getGwName :: String -> String
getGwName gw = fromMaybe gw (List.lookup gw exceptionsGW)


-- runFn "/Users/salokya.kumar/Work/euler-hs/euler-api-txns/euler-x/src-generated" "Gateway.CommonGateway"
-- runFn "/Users/salokya.kumar/Work/euler-hs/euler-api-gateway/src" "Euler.API.Gateway.Gateway.Common"

data Dummy = 
    ABExport
  | B
  | C
  deriving (Show, Eq)