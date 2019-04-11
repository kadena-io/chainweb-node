{ rpRef ? "f3ff81d519b226752c83eefd4df6718539c3efdc"
, rpSha ? "1ijxfwl36b9b2j4p9j3bv8vf7qfi570m1c5fjyvyac0gy0vi5g8j"
, system ? builtins.currentSystem
, runTests ? true
, runCoverage ? false
}:

let rp = builtins.fetchTarball {
      url = "https://github.com/reflex-frp/reflex-platform/archive/${rpRef}.tar.gz";
      sha256 = rpSha;
    };

overlay = self: super: {
  z3 = super.z3.overrideAttrs (drv: {
    src = self.fetchFromGitHub {
      owner = "Z3Prover";
      repo = "z3";
      rev = "727929c9af003d71eab1f0d90cc8e01761943491";
      sha256 = "02p8rhflimc852ysgf7nmaypz6ga3w4iss3z8d3qrby5a2d464p9";
    };
  });
};

in
  (import rp { inherit system; nixpkgsOverlays = [ overlay ]; }).project ({ pkgs, ... }:
  let gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
        owner = "siers";
        repo = "nix-gitignore";
        rev = "4f2d85f2f1aa4c6bff2d9fcfd3caad443f35476e";
        sha256 = "1vzfi3i3fpl8wqs1yq95jzdi6cpaby80n8xwnwa8h2jvcw3j7kdz";
      }) {};
  in
  {
    name = "chainweb";
    overrides = self: super: with pkgs.haskell.lib;
      let # Working on getting this function upstreamed into nixpkgs, but
          # this actually gets things directly from hackage and doesn't
          # depend on the state of nixpkgs.  Should allow us to have fewer
          # github overrides.
          callHackageDirect = {pkg, ver, sha256}@args:
            let pkgver = "${pkg}-${ver}";
            in self.callCabal2nix pkg (pkgs.fetchzip {
                 url = "http://hackage.haskell.org/package/${pkgver}/${pkgver}.tar.gz";
                 inherit sha256;
               }) {};
      in {
        aeson = callHackageDirect {
          pkg = "aeson";
          ver = "1.4.2.0";
          sha256 = "0qcczw3l596knj9s4ha07wjspd9wkva0jv4734sv3z3vdad5piqh";
        };

        chainweb = enableDWARFDebugging (overrideCabal super.chainweb (drv: {
          doCheck = runTests;
          doHaddock = runTests;
          doCoverage = runCoverage;
        }));
        configuration-tools = dontCheck (self.callHackage "configuration-tools" "0.4.0" {});
        rocksdb-haskell = dontCheck (self.callHackage "rocksdb-haskell" "1.0.1" {});

        x509 = callHackageDirect {
          pkg = "x509";
          ver = "1.7.5";
          sha256 = "13r0wdvhb0a9pda2j209j6jy02h59jwyz356jzw9qq2y9ld1ggy9";
        };

        generic-lens = callHackageDirect {
          pkg = "generic-lens";
          ver = "1.1.0.0";
          sha256 = "1s4b8sq40acqpmc9qkzbspc4qn18ym4fxbnh0s55p2nv5v8m1qia";
        };

        vector-algorithms = callHackageDirect {
          pkg = "vector-algorithms";
          ver = "0.8.0.1";
          sha256 = "1kvi2xqpiz7n7713m4gf702bmgbibrh4mnjdmq5s0i6nbb58zylm";
        };

        # --- massiv --- #
        massiv = callHackageDirect {
          pkg = "massiv";
          ver = "0.3.0.0";
          sha256 = "0yv5vq9v18jzs5mbg2qpyh18dbc54s143231b3d0bw9mawp81nsi";
        };

        scheduler = callHackageDirect {
          pkg = "scheduler";
          ver = "1.0.0";
          sha256 = "0kmb7v5bl5rcn37bgz1ghrdpr22dbxkzmrd6h65jkhbvciz8hqlf";
        };
        # --- end massiv --- #

        fast-builder = callHackageDirect {
          pkg = "fast-builder";
          ver = "0.1.0.0";
          sha256 = "1lww53vn38pin1kw87bambqjd7f4bsw1b5ix77zclvg4gj257pm1";
        };

        wai-middleware-throttle = callHackageDirect {
          pkg = "wai-middleware-throttle";
          ver = "0.3.0.0";
          sha256 = "01ay49qwa5g0x00khzn2kxw58bzmafs5n32bz4g4lf14mw7dway7";
        };

        strict-tuple = callHackageDirect {
          pkg = "strict-tuple";
          ver = "0.1.2";
          sha256 = "108rgvqybrvscr5r9h577q4dh4pyjlc5knixla5ha5s8ycxi4c0m";
        };

        # --- tasty and its downstream dependants --- #
        # These can be removed once `tasty-1.2` is natively available in `nixpkgs`.
        tasty = callHackageDirect {
          pkg = "tasty";
          ver = "1.2";
          sha256 = "00pbf8rnissqd0nzykhq9szqdl56mylwqwyci7irmsb78ky1y2dh";
        };

        tasty-ant-xml = callHackageDirect {
          pkg = "tasty-ant-xml";
          ver = "1.1.5";
          sha256 = "05c0fa26ga7n84sidv189ik900p8ngx96v0asyz313hsnfx966y5";
        };

        tasty-hedgehog = doJailbreak (callHackageDirect {
          pkg = "tasty-hedgehog";
          ver = "0.2.0.0";
          sha256 = "0nhjxjj5dsh9h8yff9np6pj48a6lx5cd1zv50xlyfvvribyf6qvk";
        });

        natural-transformation = doJailbreak (callHackageDirect {
          pkg = "natural-transformation";
          ver = "0.4";
          sha256 = "124dabxss40angramlhcid9wbm878vgkfgqf6hrfl3n3dispkbnd";
        });

        aeson-compat = doJailbreak (callHackageDirect {
          pkg = "aeson-compat";
          ver = "0.3.9";
          sha256 = "07xw0chynnwr8i8jzn6ffvh732g9qi15mzj2nbyg685japkwwcrq";
        });

        these = doJailbreak (callHackageDirect {
          pkg = "these";
          ver = "0.7.5";
          sha256 = "0m9d9n7dy7plq20pxbl8pdgq4w2xskx2rbg9d4qnac14412bfcmf";
        });

        insert-ordered-containers = doJailbreak (callHackageDirect {
          pkg = "insert-ordered-containers";
          ver = "0.2.1.0";
          sha256 = "1ys02jz4xg94g8z78cgafi24vjp7fyhf0slcyrhs1ffbhr8gqwm3";
        });
        fake = doJailbreak (callHackageDirect {
          pkg = "fake";
          ver = "0.1.1.1";
          sha256 = "17b2iwqg62cl7r7lafjm8fj1chb104g2gdq8p2bbsgvvr39v0ras";
        });
        # --- end of `tasty` dependents --- #

        extra = dontCheck (callHackageDirect {
          pkg = "extra";
          ver = "1.6.13";
          sha256 = "03kw3jd7779vp4i7nrgvdkb34jxwqn1kvggag2562j1337b5gybr";
        });

        # pact-2.6.1
        pact = dontCheck ( addBuildDepend (self.callCabal2nix "pact" (pkgs.fetchFromGitHub {
          owner = "kadena-io";
          repo = "pact";
          rev = "641542c16dfbd3806c4e646429e90027b1f3d07f";
          sha256 = "00cpdmhqvhzfhx5cw70pxi0afpk2z6km0w7hr7y1a0nkvrcb06ci";
          }) {}) pkgs.z3);

        streaming = callHackageDirect {
          pkg = "streaming";
          ver = "0.2.2.0";
          sha256 = "0yym840jnh2cma5n4c0pv3nh1hyhag1v6pks73wdikhrcajffsh3";
        };

        wai-middleware-metrics = dontCheck super.wai-middleware-metrics;

        wai-cors = dontCheck (callHackageDirect {
          pkg = "wai-cors";
          ver = "0.2.6";
          sha256 = "0rgh2698h6xc6q462lbmdb637wz2kkbnkgbhv1h7a6p3zv097dg2";
        });

        yet-another-logger = callHackageDirect {
          pkg = "yet-another-logger";
          ver = "0.3.1";
          sha256 = "17i5km3bxlp568q9pbnbp2nvpfgnmccpfnvcci0z1f56cw95679n";
        };

        # test suite fails to build with for older versions
        scotty = callHackageDirect {
            pkg = "scotty";
            ver = "0.11.3";
            sha256 = "0y98macg977ps81h9mx3hzdmkxn5y14556a2dyvd22468nsmjid1";
        };

        # need crackNum 2.3
        crackNum = pkgs.haskell.lib.dontCheck (self.callCabal2nix "crackNum" (pkgs.fetchFromGitHub {
          owner = "LeventErkok";
          repo = "crackNum";
          rev = "54cf70861a921062db762b3c50e933e73446c3b2";
          sha256 = "02cg64rq8xk7x53ziidljyv3gsshdpgbzy7h03r869gj02l7bxwa";
        }) {});

        merkle-log = dontCheck (self.callCabal2nix "merkle-log" (pkgs.fetchFromGitHub {
          owner = "kadena-io";
          repo = "merkle-log";
          rev = "a7ae61d7082afe3aa1a0fd0546fc1351a2f7c376";
          sha256 = "05132bqc6724a58kidrqs1xq68d1bmfqsdy7yk5j83ddinw7yvp1";
        }) {});

        ######################################################################
        # Dependencies from pact
        # pact = addBuildDepend super.pact pkgs.z3;

        # tests try to use ghc-pkg and cabal (https://github.com/sol/doctest/issues/213)
        doctest = dontCheck (self.callHackage "doctest" "0.16.0" {});

        # these want to use doctest, which doesn't work on ghcjs
        bytes = dontCheck super.bytes;
        intervals = dontCheck super.intervals;
        bound = dontCheck super.bound;
        trifecta = dontCheck super.trifecta;
        lens-aeson = dontCheck super.lens-aeson;
        # test suite for this is failing on ghcjs:
        hw-hspec-hedgehog = dontCheck super.hw-hspec-hedgehog;

        algebraic-graphs = dontCheck super.algebraic-graphs;

        # Needed to get around a requirement on `hspec-discover`.
        megaparsec = dontCheck super.megaparsec;

        hedgehog = callHackageDirect {
          pkg = "hedgehog";
          ver = "0.6.1";
          sha256 = "041dn82r732vsda9zvyf3p2zkljkdala684g106d13672n2z34km";
        };

        # hlint = self.callHackage "hlint" "2.0.14" {};
        # hoogle = self.callHackage "hoogle" "5.0.15" {};

        # specific revision needed by pact
        sbv = pkgs.haskell.lib.dontCheck (self.callCabal2nix "sbv" (pkgs.fetchFromGitHub {
          owner = "LeventErkok";
          repo = "sbv";
          rev = "365b1a369a2550d6284608df3fbc17e2663c4d3c";
          sha256 = "134f148g28dg7b3c1rvkh85pfl9pdlvrvl6al4vlz72f3y5mb2xg";
        }) {});

        # Our own custom fork
        thyme = dontCheck (self.callCabal2nix "thyme" (pkgs.fetchFromGitHub {
          owner = "kadena-io";
          repo = "thyme";
          rev = "6ee9fcb026ebdb49b810802a981d166680d867c9";
          sha256 = "09fcf896bs6i71qhj5w6qbwllkv3gywnn5wfsdrcm0w1y6h8i88f";
        }) {});
        ######################################################################

      };
    packages = {
      chainweb = gitignore.gitignoreSource
        [".git" ".gitlab-ci.yml" "CHANGELOG.md" "README.md" "future-work.md"] ./.;
    };
    shellToolOverrides = ghc: super: {
      stack = pkgs.stack;
      cabal-install = pkgs.haskellPackages.cabal-install;
      ghcid = pkgs.haskellPackages.ghcid;
    };
    shells = {
      ghc = ["chainweb"];
    };

  })
