pactSrc: pkgs: self: super: with pkgs.haskell.lib;
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

    ourOverrides = {
      pact = dontCheck ( addBuildDepend (self.callCabal2nix "pact" pactSrc {}) pkgs.z3);

      chainweb = enableCabalFlag (
        justStaticExecutables (enableDWARFDebugging super.chainweb)) "use_systemd";

      chainweb-storage = dontCheck (self.callCabal2nix "chainweb-storage" (pkgs.fetchFromGitHub {
        owner = "kadena-io";
        repo = "chainweb-storage";
        rev = "17a5fb130926582eff081eeb1b94cb6c7097c67a";
        sha256 = "03ihjgwqpif68870wwsgz1s4yz45zql1slky1lj4ixfxbig06md4";
      }) {});

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

      warp-tls = callHackageDirect {
        pkg = "warp-tls";
        ver = "3.2.8";
        sha256 = "0bf6gnyz9pq57y3hgv1xpfi1cnsda0wrwyd18zmh2220hxmvda71";
      };

    };
in

(import "${pactSrc}/overrides.nix" pkgs self super) // ourOverrides
