{ compiler ? "ghc8107"
, rev      ? "7a94fcdda304d143f9a40006c033d7e190311b54"
, sha256   ? "0d643wp3l77hv2pmg2fi7vyxn4rwy0iyr8djcw1h5x72315ck9ik"
, pkgs     ?
    import (builtins.fetchTarball {
      url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      inherit sha256; }) {
      config.allowBroken = false;
      config.allowUnfree = true;
      overlays = [
        (self: super: {
           tbb = super.tbb.overrideAttrs(attrs: {
             patches = attrs.patches ++ [
               (super.fetchurl {
                 name = "aarch64-darwin.patch";
                 url = "https://github.com/oneapi-src/oneTBB/pull/258/commits/86f6dcdc17a8f5ef2382faaef860cfa5243984fe.patch";
                 sha256 = "sha256-JXqrFPCb3q1vfxk752tQu7HhApCB4YH2LoVnGRwmspk=";
               })
             ];
           });
         })
      ];
    }
, returnShellEnv ? false
, mkDerivation ? null
}:
let gitignoreSrc = import (pkgs.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "gitignore";
      rev = "9e80c4d83026fa6548bc53b1a6fab8549a6991f6";
      sha256 = "04n9chlpbifgc5pa3zx6ff3rji9am6msrbn1z3x1iinjz2xjfp4p";
    }) {};
    nix-thunk = import ./dep/nix-thunk {};
in
pkgs.haskell.packages.${compiler}.developPackage {
  name = "chainweb";
  root = gitignoreSrc.gitignoreSource ./.;

  overrides = self: super: with pkgs.haskell.lib; {
      sha-validation = self.callCabal2nix "sha-validation" (pkgs.fetchFromGitHub {
        owner = "larskuhtz";
        repo = "hs-sha-validation";
        rev = "0542786e7e7b4b24a37243d7168f81800abe59f0";
        sha256 = "1fp3m6jwzykpfkbwi447rylg9616ph1k0avrr0i73p1pippxzqpx";
      }) {};

      rosetta = self.callCabal2nix "rosetta" (pkgs.fetchFromGitHub {
        owner = "kadena-io";
        repo = "rosetta";
        rev = "6c8dd2eea1f6d0dba925646dbcb6e07feeccbfd5";
        sha256 = "19pjy06xrx2siggzybcmly0qaq4ds3yzxcsvqwgs4qh9kkzh0kqh";
      }) {};

      rocksdb-haskell-kadena = overrideCabal
        (self.callCabal2nix "rocksdb-haskell-kadena" (pkgs.fetchFromGitHub {
          owner = "kadena-io";
          repo = "rocksdb-haskell";
          rev = "2161777750bf879856251289e551e8dc2cd512e2";
          sha256 = "09cmjrhkjv6mccaaasb2lz1sjndha1df3wyygi7166axj0srw8ds";
          # date = "2022-08-10T09:03:56-04:00";
        }) {})
        (attrs: {
          preConfigure = (attrs.preConfigure or "") +
            pkgs.lib.optionalString (!pkgs.stdenv.hostPlatform.sse4_2Support) ''
              perl -i -ne 'print unless /HAVE_SSE42/;' rocksdb-haskell-kadena.cabal
            '' +
            pkgs.lib.optionalString (pkgs.stdenv.hostPlatform.system == "aarch64-darwin") ''
              patch -p1 < ${./rocksdb-arm64.patch}
            '';
          librarySystemDepends = (attrs.librarySystemDepends or []) ++ [
            pkgs.snappy.dev
            pkgs.zlib.dev
            pkgs.zstd.dev
            pkgs.lz4.dev
            pkgs.perl
            pkgs.patch
          ];
        });

      ethereum = dontCheck (self.callCabal2nix "ethereum" (pkgs.fetchFromGitHub {
        owner = "kadena-io";
        repo = "kadena-ethereum-bridge";
        rev = "10f21e96af1dce4f13e261be9dfad8c28cd299f7";
        sha256 = "1vab2m67ign6x77k1sjfjmv9sbrrl5sl2pl07rw1fw8bjqnp5vqk";
      }) {});

      resource-pool = self.callHackageDirect {
        pkg = "resource-pool";
        ver = "0.3.0.0";
        sha256 = "0bpf868b6kq1g83s3sad26kfsawmpd3j0xpkyab8370lsq6zhcs1";
      } {};

      direct-sqlite = self.callHackageDirect {
        pkg = "direct-sqlite";
        ver = "2.3.27";
        sha256 = "0w8wj3210h08qlws40qhidkscgsil3635zk83kdlj929rbd8khip";
      } {};

      yet-another-logger = self.callHackageDirect {
        pkg = "yet-another-logger";
        ver = "0.4.1";
        sha256 = "1qb0ns764sb5az8z1dn7pflizi8ni8qivbhx79sj9kfaa68hyhsl";
      } {};

      hashes = self.callHackageDirect {
        pkg = "hashes";
        ver = "0.2.2.1";
        sha256 = "1039smdvx03j52md2vv1lvkllm47rf0zi9bqmnh1lk1zhcv0sag3";
      } {};

      pact = appendConfigureFlag super.pact "-f-build-tool";

      autodocodec    = unmarkBroken super.autodocodec;
      hashable       = doJailbreak super.hashable;
      ixset-typed    = unmarkBroken super.ixset-typed;
      rebase         = doJailbreak super.rebase;
      token-bucket   = unmarkBroken super.token-bucket;
      validity-aeson = unmarkBroken super.validity-aeson;

      # Cuckoo tests fail due to a missing symbol
      cuckoo        = dontCheck super.cuckoo;

      # These tests pull in unnecessary dependencies
      http2         = dontCheck super.http2;
      prettyprinter = dontCheck super.prettyprinter;
      aeson         = dontCheck super.aeson;
      generic-data  = dontCheck super.generic-data;
  };

  source-overrides = {
    chainweb-storage = nix-thunk.thunkSource ./dep/chainweb-storage;
    pact             = nix-thunk.thunkSource ./dep/pact;

    OneTuple                    = "0.3";
    aeson                       = "1.5.6.0";
    ansi-terminal               = "0.11.3";
    prettyprinter-ansi-terminal = "1.1.2";
    time-compat                 = "1.9.5";
    trifecta                    = "2.1.1";
    unordered-containers        = "0.2.15.0";

    # These are required in order to not break payload validation
    base16-bytestring = "0.1.1.7";
    prettyprinter     = "1.6.0";
    hashable          = "1.3.0.0";
    base64-bytestring = "1.0.0.3";
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      pkgs.zlib
      pkgs.haskell.packages.${compiler}.cabal-install
    ];
  });

  inherit returnShellEnv;
}
