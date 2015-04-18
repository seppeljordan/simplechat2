{ stdenv, haskellngPackages}:

let
  env=haskellngPackages.ghcWithPackages (p: with p; [
    network
    async
    reactive-banana
    parsec
    tls
    bytestring
    containers
    time
    iproute
  ]);
in
  stdenv.mkDerivation {
    name = "simplechat2";
    buildInputs = [env];
    shellHook   = ''
      export NIX_GHC="${env}/bin/ghc"
      export NIX_GHCPKG="${env}/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
    '';
  }
