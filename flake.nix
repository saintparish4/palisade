{
  description = "PALISADE: a proof-gated execution kernel for machine-initiated money";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hs = pkgs.haskell.packages.ghc96;
        palisade = hs.callCabal2nix "palisade" (pkgs.lib.cleanSource ./.) {};
      in {
        packages.default = palisade;

        apps.default = flake-utils.lib.mkApp {
          drv = palisade;
        };

        devShells.default = hs.shellFor {
          packages = p: [ palisade ];
          buildInputs = [
            pkgs.cabal-install
            hs.haskell-language-server
            pkgs.ghcid
          ];
          withHoogle = true;
        };
      });
}
