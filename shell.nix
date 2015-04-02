# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

with haskellPackages;
cabal.mkDerivation (self: {
  pname = "simplechat2";
  version = "0.1.0.0";
  src="./.";
  isLibrary = false;
  isExecutable = true;
  buildTools = [ cabalInstall ];
  buildDepends = [
    async iproute network parsec reactiveBanana time tls HUnit
  ];
  meta = {
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
