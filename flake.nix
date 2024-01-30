{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
      overlay = final: prev: {
        sashakt = final.callCabal2nix "sashakt" ./. { };
      };
      myHaskellPackages = pkgs.haskell.packages.ghc8107.extend overlay;
    in
    {
      # mark as non broken
      haskellProjects.default.settings.classyplate.broken = false;

      # What to build for nix build
      packages.${system}.default = myHaskellPackages.sashakt;

      # What to run for nix run
      apps.${system}.default = {
        type = "app";
        program = "${self.packages.${system}.default}/bin/sashakt";
      };

      # What to include in shells for nix develop
      devShells.${system}.default = myHaskellPackages.shellFor {
        packages = p : [
          p.sashakt
        ];
        nativeBuildInputs = with myHaskellPackages; [
          ghcid
          cabal-install
        ];
      };
    };
}