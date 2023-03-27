{ compiler ? "ghc8107"
, rev      ? "7a94fcdda304d143f9a40006c033d7e190311b54"
, sha256   ? "0d643wp3l77hv2pmg2fi7vyxn4rwy0iyr8djcw1h5x72315ck9ik"
, pkgs     ?
    import (builtins.fetchTarball {
      url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      inherit sha256; }) {
      config.allowBroken = false;
      config.allowUnfree = true;
      overlays = [];
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

      rosetta = doJailbreak (self.callCabal2nix "rosetta" (pkgs.fetchFromGitHub {
        owner = "kadena-io";
        repo = "rosetta";
        rev = "100733c3c74b61cdf3f921fb466f85a56c1d4de0";
        sha256 = "19pjy06xrx2siggzybcmly0qaq4ds3yzxcsvqwgs4qh9kkzh0kqh";
        # date = "2022-02-11T13:55:11-05:00";
      }) {});

      rocksdb-haskell-kadena = overrideCabal
        (self.callCabal2nix "rocksdb-haskell-kadena" (pkgs.fetchFromGitHub {
          owner = "kadena-io";
          repo = "rocksdb-haskell";
          rev = "c2b3dd8bb714a12ea6763565d168a03df38fcc58";
          sha256 = "122xnsx6wlcxzgdywx1rzg9w6mj37g6vfcvmwz93xq50fxy33fc0";
          # date = "2023-02-13T16:59:35-08:00";
        }) {})
        (attrs: {
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
        rev = "7b8daff3e79e3fcfb1ae7ba61b779993244658ea";
        sha256 = "03dysqfbv9qpsh092glnk33hhb7r4is7k7mypjnk98iawg1mp14r";
        # date = "2022-11-22T08:20:17-08:00";
      }) {});

      resource-pool = self.callHackageDirect {
        pkg = "resource-pool";
        ver = "0.4.0.0";
        sha256 = "0zlnizx0igwmvpx43mcnk0h58v9lakhwr4g9csy1vj74p7a4hxaz";
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
        ver = "0.2.3";
        sha256 = "0rif1hh2zwqmh2p8ca8si1jngzrrl91bpdzhpwpcxj49q3b6sys1";
      } {};

      sbv = dontCheck (self.callHackageDirect {
        pkg = "sbv";
        ver = "9.0";
        sha256 = "14g2qax1vc7q4g78fa562dviqvcd0l52kd5jmgv90g3g3ci15bnl";
      } {});

      hashable = self.callHackageDirect {
        pkg = "hashable";
        ver = "1.4.1.0";
        sha256 = "0ms8df9v4rcy424ggsjaz9ik4fnggs6698zlfq099vqmsp2x93nn";
      } {};

      pact-time = self.callHackageDirect {
        pkg = "pact-time";
        ver = "0.2.0.1";
        sha256 = "0lp0nypq675f652k2pl4kf92agy9jhzvf03zx61gvj4djgjw1rxy";
      } {};

      lens-aeson = self.callHackageDirect {
        pkg = "lens-aeson";
        ver = "1.2.2";
        sha256 = "154lx61zbdgnmd9csnbvfqzpv4y9lqm7k7ssal2d4mxlwwlcx6jf";
      } {};

      pact = dontCheck (appendConfigureFlag (import ../pact/default-new.nix {}) "-f-build-tool");

      autodocodec    = unmarkBroken super.autodocodec;
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
    # pact             = nix-thunk.thunkSource ./dep/pact;

    # OneTuple                    = "0.3.1";
    ansi-terminal               = "0.11.3";
    prettyprinter-ansi-terminal = "1.1.2";
    # time-compat                 = "1.9.6.1";
    trifecta                    = "2.1.2";
    pact-time                   = "0.2.0.1";
    unordered-containers        = "0.2.19.1";

    # These are required in order to not break payload validation
    base16-bytestring = "0.1.1.7";
    prettyprinter     = "1.7.1";
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
