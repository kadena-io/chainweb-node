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
          ver = "1.4.3.0";
          sha256 = "13lim8vv78m9lhn7qfjswg7ax825gn0v75gcb80hckxawgk8zxc1";
        };

        chainweb = enableDWARFDebugging (overrideCabal super.chainweb (drv: {
          doCheck = runTests;
          doHaddock = runTests;
          doCoverage = runCoverage;
          testTarget = "--test-option=--hide-successes";
        }));

        rocksdb-haskell = dontCheck (self.callHackage "rocksdb-haskell" "1.0.1" {});

        yaml = callHackageDirect {
          pkg = "yaml";
          ver = "0.11.0.0";
          sha256 = "0nw3k8bijs88ipvqadmiwbab7asj4kmf6r4l1dma30fyam0hj2kv";
        };

        libyaml = callHackageDirect {
          pkg = "libyaml";
          ver = "0.1.1.0";
          sha256 = "1sh71s6hfrn3s2dh80lki8v412dzcrmyljrdvwksj3las3pl4ws9";
        };

        x509 = callHackageDirect {
          pkg = "x509";
          ver = "1.7.5";
          sha256 = "13r0wdvhb0a9pda2j209j6jy02h59jwyz356jzw9qq2y9ld1ggy9";
        };

        generic-lens = dontCheck (callHackageDirect {
          pkg = "generic-lens";
          ver = "1.1.0.0";
          sha256 = "1s4b8sq40acqpmc9qkzbspc4qn18ym4fxbnh0s55p2nv5v8m1qia";
        });

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

        bounded-queue = callHackageDirect {
          pkg = "bounded-queue";
          ver = "1.0.0";
          sha256 = "04p9p8n3l75lhc0f4g7q8qwxwcjpv11mqyf46wxnb3s9wd0wyazc";
        };

        nonempty-containers = callHackageDirect {
          pkg = "nonempty-containers";
          ver = "0.1.1.0";
          sha256 = "09cq35spxppyhyigf2y6fhw4x72hg1jm80agzw8ccq1zbml7pnmv";
        };

        configuration-tools = dontCheck (callHackageDirect {
          pkg = "configuration-tools";
          ver = "0.4.1";
          sha256 = "1sbn4dbb2y1gwdwjvz5vf6a1g349z0jha5iz4dmp2v67dv86fzs5";
        });

        # --- servant 0.16 --- #
        servant = dontCheck (callHackageDirect {
          pkg = "servant";
          ver = "0.16.0.1";
          sha256 = "1z9iyfrf7wfx0849y7jyknfh9332y417qjivh69fh2zi7j6jx84g";
        });
        servant-client = doJailbreak (callHackageDirect {
          pkg = "servant-client";
          ver = "0.16";
          sha256 = "1jy3bdjkdl9bxwb6y2kial0w8ik6k4a8gq7b9yx9l4nkvjv8rc06";
        });
        servant-client-core = doJailbreak (callHackageDirect {
          pkg = "servant-client-core";
          ver = "0.16";
          sha256 = "0panxplcjslsvqxvsabn2fy0fhcqmmr0dqj51hk7bk7yyvgwxklf";
        });
        servant-server = doJailbreak (callHackageDirect {
          pkg = "servant-server";
          ver = "0.16";
          sha256 = "0am0mvj0h09a68mbjlb2rrlxybd2jwi8x2qq6kdd5fr6bhya9d5a";
        });
        base-compat= callHackageDirect {
          pkg = "base-compat";
          ver = "0.10.5";
          sha256 = "0fq38x47dlwz3j6bdrlfslscz83ccwsjrmqq6l7m005331yn7qc6";
        };
        base-compat-batteries = callHackageDirect {
          pkg = "base-compat-batteries";
          ver = "0.10.5";
          sha256 = "1l82l4q7sz336qqdqy3rhh6brgnslczdavn92wnaimpjmwbv256c";
        };
        base-orphans = callHackageDirect {
          pkg = "base-orphans";
          ver = "0.8.1";
          sha256 = "1jg06ykz8fsk1vlwih4vjw3kpcysp8nfsv7qjm42y2gfyzn6jvsk";
        };
        contravariant = callHackageDirect {
          pkg = "contravariant";
          ver = "1.5.2";
          sha256 = "1glq01mdv2xjwx7vkf3yvyd2rs150zx9gr7jy0gk7qylq6ljx8w6";
        };
        free = callHackageDirect {
          pkg = "free";
          ver = "5.1.1";
          sha256 = "1c305xd8k28c04xw3a5q2sab0g42v5k659kx7dv48cnvvjfaz5cn";
        };
        http-api-data = doJailbreak (dontCheck (callHackageDirect {
          pkg = "http-api-data";
          ver = "0.4";
          sha256 = "190b5lhl89lifyh4i1d8w8phx1sqmyz4hq95vc3h1ra288nvrzl5";
        }));
        lens = callHackageDirect {
          pkg = "lens";
          ver = "4.17.1";
          sha256 = "0sig0p5b351sr7fpvhxbkrbj9nvjh1w8yyzlcpl3hy5l1cfn2bh6";
        };
        network = dontCheck (callHackageDirect {
          pkg = "network";
          ver = "2.8.0.1";
          sha256 = "0nrgwcklb7a32wxmvbgxmm4zsbk3gpc6f2d8jpyb0b1hwy0ji4mv";
        });
        HTTP = dontCheck (callHackageDirect {
          pkg = "HTTP";
          ver = "4000.3.13";
          sha256 = "1qh6hskyxf6sljajqprg03jkkjpzwhwlciywpyxdn568s3pfqs2n";
        });
        semigroupoids = callHackageDirect {
          pkg = "semigroupoids";
          ver = "5.3.2";
          sha256 = "0mc9hwvjcidxnjpr0gilywgbnpqh5y384x2id2iai68w1lckpdy2";
        };
        servant-swagger = callHackageDirect {
          pkg = "servant-swagger";
          ver = "1.1.7.1";
          sha256 = "1ymdcmdi234p9jbwa7rgj1j35n9xnx4kgfjba4gs2r8cnhqwak28";
        };
        swagger2 = doJailbreak (callHackageDirect {
          pkg = "swagger2";
          ver = "2.3.1.1";
          sha256 = "0rhxqdiymh462ya9h76qnv73v8hparwz8ibqqr1d06vg4zm7s86p";
        });
        tagged = callHackageDirect {
          pkg = "tagged";
          ver = "0.8.6";
          sha256 = "0dsw809g7pfajbl8zsj2yvnad8vk09cqi40bszw5ia9bpzzqz7n4";
        };
        http-media = callHackageDirect {
          pkg = "http-media";
          ver = "0.8.0.0";
          sha256 = "080xkljq1iq0i8wagg8kbzbp523p2awa98wpn9i4ph1dq8y8346y";
        };
        http-types = callHackageDirect {
          pkg = "http-types";
          ver = "0.12.3";
          sha256 = "0239y1r25n7wnnf2ci5lajj0rf35j2ywa07f9lpccnry13zbh8dv";
        };
        io-streams = callHackageDirect {
          pkg = "io-streams";
          ver = "1.5.1.0";
          sha256 = "1kvy476sdnxyg9318wxg7cvbafak81a1whrcffjc19sbymgg7srp";
        };
        # --- end servant --- #

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
        tdigest = doJailbreak (dontCheck super.tdigest);
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
          rev = "f880d34850b5aa4bd538a23029a74777e1f83a5d";
          sha256 = "17752aallilpvcdqy45jxgwc33a1ljzb3qbn7zbxn7pq4f5iypxw";
          }) {}) pkgs.z3);

        streaming = callHackageDirect {
          pkg = "streaming";
          ver = "0.2.2.0";
          sha256 = "0yym840jnh2cma5n4c0pv3nh1hyhag1v6pks73wdikhrcajffsh3";
        };

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

        merkle-log = callHackageDirect {
            pkg = "merkle-log";
            ver = "0.1.0.0";
            sha256 = "10jk274sbvsrr7varxa72jvh54n22qpw7d4p2wy7415bmij3y81p";
        };

        digraph = dontCheck (callHackageDirect {
            pkg = "digraph";
            ver = "0.1.0.2";
            sha256 = "1alqdzzlw8ns6hy8vh3ic4ign7jjxxa0cyxkv26zz7k2dihf3hzg";
        });

        mwc-random = callHackageDirect {
            pkg = "mwc-random";
            ver = "0.14.0.0";
            sha256 = "10jaajbnlcwqgqdnb94q0k8pzx11ff569af8a8d6k26xc954m48p";
        };

        # --- QuickCheck --- #
        QuickCheck = callHackageDirect {
          pkg = "QuickCheck";
          ver = "2.12.6.1";
          sha256 = "1f6hp0xp2syhinrm47pc88m4rq59w0xc4kwbciqzyrxz3gs895ha";
        };
        hspec = callHackageDirect {
          pkg = "hspec";
          ver = "2.7.0";
          sha256 = "1bsbljq8cp8haini4q1m7q5ij9y36dariipiniwc4h6cbdj9z3kv";
        };
        hspec-core = callHackageDirect {
          pkg = "hspec-core";
          ver = "2.7.0";
          sha256 = "0323sc2ivxw04q2rcadc6yylgx8i2fd66qpm1qz282k1i9i6amdn";
        };
        hspec-discover = callHackageDirect {
          pkg = "hspec-discover";
          ver = "2.7.0";
          sha256 = "0rgrhxhz4yfslhdvcwrqm88yx45v0p7b6hh15lsdr3d3jsz7whkw";
        };
        hspec-meta = callHackageDirect {
          pkg = "hspec-meta";
          ver = "2.6.0";
          sha256 = "0i9crf6hr9m5xsh8w0hdsrmvcnxckphfzamwmks2qgk7mq0yqnz5";
        };
        ChasingBottoms = dontCheck (callHackageDirect {
          pkg = "ChasingBottoms";
          ver = "1.3.1.5";
          sha256 = "1p81agaqny88q6pqx5b3qm96fn7cjf1xxvdp3rzlr8asyqlmml8l";
        });
        optparse-applicative = callHackageDirect {
          pkg = "optparse-applicative";
          ver = "0.14.3.0";
          sha256 = "1sdf0cgv89sf9v7vi6vm6aawa5m6gjmqsxynxf8nsn0420ayb0y3";
        };
        test-framework-quickcheck2 = callHackageDirect {
          pkg = "test-framework-quickcheck2";
          ver = "0.3.0.5";
          sha256 = "0n1hzihnnw60yzvfg98ch9x20g10ic10lx9plni9b9pxsrsxdz37";
        };
        HTF = dontCheck (callHackageDirect {
          pkg = "HTF";
          ver = "0.13.2.5";
          sha256 = "0s9xzgj01rqgyhgvggrgfjsdma1kzqx4n23fy27v7kd2abpzgzaf";
        });
        cereal = dontCheck super.cereal;
        psqueues = dontCheck super.psqueues;
        Diff = dontCheck super.Diff;
        # --- end QuickCheck --- #

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
