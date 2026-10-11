{ pkgs ? import <nixpkgs> {} }:
let
  hs = pkgs.haskell.packages.ghc96;
  palisade = hs.callCabal2nix "palisade" (pkgs.lib.cleanSource ./.) {};
in
hs.shellFor {
  packages = p: [ palisade ];
  buildInputs = [
    pkgs.cabal-install
    hs.haskell-language-server
    pkgs.ghcid
  ];
  withHoogle = true;
}
