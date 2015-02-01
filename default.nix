{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal cabalInstall
    hakyll systemFilepath dataDefaultInstancesOldLocale; # Haskell dependencies here

in cabal.mkDerivation (self: {
  pname = "llog";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    # As imported above
    hakyll systemFilepath
  ];
  buildTools = [ cabalInstall ];
  enableSplitObjs = false;
})
