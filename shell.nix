{ rev ? "9169ee6b588c8c7077645d3ab76948a3093e25cf"
, sha ? "1wzbjh20vb7ykd4ysm7lg6m0g2icagxvpsy7hq94mpp6001cjqi1"
, pkgs ? import (builtins.fetchTarball {
                   url = "https://github.com/kadena-io/nixpkgs/archive/${rev}.tar.gz";
                   sha256 = sha; }) { config.allowUnfree = true; }
}: (import ./. {}).overrideAttrs
    (attrs: { buildInputs = attrs.buildInputs ++
        [pkgs.haskellPackages.cabal-install
         pkgs.haskellPackages.ghcid
         pkgs.zlib
        ]; 
    })