{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          base cabal-install system-filepath hakyll
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "llog";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
