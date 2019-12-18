{ pactSrc, pkgs, hackGet }:
self: super: with pkgs.haskell.lib;
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

    # Includes test suite and benchmark binaries in the output derivation.
    # Has the side effect of causing nix-build to not run them.
    convertCabalTestsAndBenchmarksToExecutables = p:
      overrideCabal p (drv: {
        preConfigure = (drv.preConfigure or "") + ''
          sed -i -e 's/^\(test-suite\|benchmark\) /executable /' -e '/^ *type: *exitcode-stdio-1.0$/d' *.cabal
        '';
      });

    ourOverrides = {
      pact = dontCheck ( addBuildDepend (self.callCabal2nix "pact" pactSrc {}) pkgs.z3);

      chainweb = enableCabalFlag (
        justStaticExecutables (enableDWARFDebugging (convertCabalTestsAndBenchmarksToExecutables super.chainweb))) "use_systemd";

      chainweb-storage = dontCheck (self.callCabal2nix "chainweb-storage" (pkgs.fetchFromGitHub {
        owner = "kadena-io";
        repo = "chainweb-storage";
        rev = "17a5fb130926582eff081eeb1b94cb6c7097c67a";
        sha256 = "03ihjgwqpif68870wwsgz1s4yz45zql1slky1lj4ixfxbig06md4";
      }) {});

      aeson = enableCabalFlag (dontCheck (callHackageDirect {
        pkg = "aeson";
        ver = "1.4.6.0";
        sha256 = "05rj0fv5y65dk17v24p3qypvrakkhdj41vrxnyk4wimgaw2g5lq4";
      })) "cffi";

      configuration-tools = dontCheck (callHackageDirect {
        pkg = "configuration-tools";
        ver = "0.4.1";
        sha256 = "1sbn4dbb2y1gwdwjvz5vf6a1g349z0jha5iz4dmp2v67dv86fzs5";
      });

      digraph = dontCheck (callHackageDirect {
        pkg = "digraph";
        ver = "0.1.0.2";
        sha256 = "1alqdzzlw8ns6hy8vh3ic4ign7jjxxa0cyxkv26zz7k2dihf3hzg";
      });

      fake = doJailbreak (callHackageDirect {
        pkg = "fake";
        ver = "0.1.1.2";
        sha256 = "1swp4j80761rfb0xiwshf0zal02ykwrbv49iyjay9ivvka367wk9";
      });

      generic-lens = dontCheck (callHackageDirect {
        pkg = "generic-lens";
        ver = "1.2.0.1";
        sha256 = "0pkwyrmaj8wqlajb7cnswh7jq4pnvnhkjcl1flhw94gqn0vap50g";
      });

      hedgehog-fn = callHackageDirect {
        pkg = "hedgehog-fn";
        ver = "1.0";
        sha256 = "1dhfyfycy0wakw4j7rr01a7v70yms7dw3h60k5af7pi9v700wyb4";
      };

      massiv = callHackageDirect {
        pkg = "massiv";
        ver = "0.4.2.0";
        sha256 = "0md9zs1md32ny9ln0dpw2hw1xka1v67alv68s8xhj0p7fabi5lxm";
      };

      merkle-log = callHackageDirect {
        pkg = "merkle-log";
        ver = "0.1.0.0";
        sha256 = "10jk274sbvsrr7varxa72jvh54n22qpw7d4p2wy7415bmij3y81p";
      };

      nonempty-containers = callHackageDirect {
        pkg = "nonempty-containers";
        ver = "0.3.1.0";
        sha256 = "1hnwvhz9w07z2mlq75iz0bysz586d828725k1bx8mjqvc86ncv8m";
      };

      random-strings = callHackageDirect {
        pkg = "random-strings";
        ver = "0.1.1.0";
        sha256 = "1d70i6hcdxrjnk05x0525lmb8wqzy9n0ipr8qd9fxpba89w24jc5";
      };

      rocksdb-haskell = dontCheck super.rocksdb-haskell;

      scheduler = callHackageDirect {
        pkg = "scheduler";
        ver = "1.4.2.1";
        sha256 = "0xlcvcwf3n4zbhf9pa3hyzc4ds628aki077564gaf4sdg1gm90qh";
      };

      systemd = callHackageDirect {
        pkg = "systemd";
        ver = "1.2.0";
        sha256 = "1mwrrki3zsc4ncr7psjv9iqkzh7f25c2ch4lf2784fh6q46i997j";
      };

      streaming-events = callHackageDirect {
        pkg = "streaming-events";
        ver = "1.0.0";
        sha256 = "1lwb5cdm4wm0avvq926jj1zyzs1g0mpanzw9kmj1r24clizdw6pm";
      };

      these = doJailbreak (callHackageDirect {
        pkg = "these";
        ver = "1.0.1";
        sha256 = "1b2cdc9d9slxjw5cr4pmplfln5kawj2w74zi92hsmwkffqiycjhz";
      });

      tls = callHackageDirect {
        pkg = "tls";
        ver = "1.5.2";
        sha256 = "00bps2bmp3ahlfw6wf7ifnni8kn306bbzapqcgsallnpgzx62gp1";
      };

      wai = dontCheck (callHackageDirect {
        pkg = "wai";
        ver = "3.2.2.1";
        sha256 = "0msyixvsk37qsdn3idqxb4sab7bw4v9657nl4xzrwjdkihy411jf";
      });

      wai-cors = dontCheck (callHackageDirect {
        pkg = "wai-cors";
        ver = "0.2.7";
        sha256 = "10yhjjkzp0ichf9ijiadliafriwh96f194c2g02anvz451capm6i";
      });

      wai-middleware-throttle = dontCheck (callHackageDirect {
        pkg = "wai-middleware-throttle";
        ver = "0.3.0.1";
        sha256 = "13pz31pl7bk51brc88jp0gffjx80w35kzzrv248w27d7dc8xc63x";
      });

      wai-extra = dontCheck (callHackageDirect {
        pkg = "wai-extra";
        ver = "3.0.28";
        sha256 = "1k470vbn2c852syj15m9xzfjnaraw6cyn35ajf2b67i01ghkshgw";
      });

      wai-app-static = doJailbreak (dontCheck (callHackageDirect {
        pkg = "wai-app-static";
        ver = "3.1.6.3";
        sha256 = "00dzhv3cdkmxgid34y7bbrkp9940pcmr2brhl2wal7kp0y999ldp";
      }));

      warp = dontCheck (callHackageDirect {
        pkg = "warp";
        ver = "3.3.5";
        sha256 = "1a5m90m8kq8ma9fh9qlqdf9bd9wy4dz1bi006d2qpg17jhhj5602";
      });

      warp-tls = callHackageDirect {
        pkg = "warp-tls";
        ver = "3.2.9";
        sha256 = "14lwqr1yj9yh2adcm2h4q4kb4k3m5w416fzx5fl4f5qkicdqqjjx";
      };

      time-manager = callHackageDirect {
        pkg = "time-manager";
        ver = "0.0.0";
        sha256 = "0z2fxikx5ax2x5bg8mcjg4y6b6irmf0swrnfprrp2xry6j5ji6hx";
      };

      http2 = callHackageDirect {
        pkg = "http2";
        ver = "2.0.3";
        sha256 = "14bqmxla0id956y37fpfx9v6crwxphbfxkl8v8annrs8ngfbhbr7";
      };

      network-byte-order = callHackageDirect {
        pkg = "network-byte-order";
        ver = "0.1.2.0";
        sha256 = "1a2kq8rmx5q3l1a3b3jcklm7hy3c3z0x08jnnwfik22sy5a5v2nr";
      };

      strict-tuple = callHackageDirect {
        pkg = "strict-tuple";
        ver = "0.1.3";
        sha256 = "1vg0m27phd6yf0pszcy2c2wbqx509fr9gacn34yja521z17cxd8z";
      };

      servant = dontCheck (callHackageDirect {
        pkg = "servant";
        ver = "0.16.2";
        sha256 = "1a83fdcwnlkmyc6fqsrfd26izcgk1jgpdgyqma3l897cnnr62frs";
      });

      servant-client = dontCheck (callHackageDirect {
        pkg = "servant-client";
        ver = "0.16.0.1";
        sha256 = "1236sldcgvk2zj20cxib9yxrdxz7d1a83jfdnn9mxa272srfq9a9";
      });

      servant-client-core = dontCheck (callHackageDirect {
        pkg = "servant-client-core";
        ver = "0.16";
        sha256 = "0panxplcjslsvqxvsabn2fy0fhcqmmr0dqj51hk7bk7yyvgwxklf";
      });

      servant-server = dontCheck (callHackageDirect {
        pkg = "servant-server";
        ver = "0.16.2";
        sha256 = "1klcszpfqy1vn3q1wbqxjghfyddw8wbg4f0ggllqw8qx2f5zp5y1";
      });
    };
in

(import "${pactSrc}/overrides.nix" pkgs hackGet self super) // ourOverrides
