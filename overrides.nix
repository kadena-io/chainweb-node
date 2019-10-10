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

in

(import "${pactSrc}/overrides.nix" pkgs self super) // {
  pact = dontCheck ( addBuildDepend (self.callCabal2nix "pact" pactSrc {}) pkgs.z3);

  aeson = callHackageDirect {
    pkg = "aeson";
    ver = "1.4.3.0";
    sha256 = "13lim8vv78m9lhn7qfjswg7ax825gn0v75gcb80hckxawgk8zxc1";
  };

  chainweb-storage = pkgs.haskell.lib.dontCheck (self.callCabal2nix "chainweb-storage" (pkgs.fetchFromGitHub {
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

  rocksdb-haskell = dontCheck super.rocksdb-haskell;

  scheduler = callHackageDirect {
    pkg = "scheduler";
    ver = "1.4.2.1";
    sha256 = "0xlcvcwf3n4zbhf9pa3hyzc4ds628aki077564gaf4sdg1gm90qh";
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
    ver = "1.5.1";
    sha256 = "0rh7302v2swbbfkjp15mgvs5xhbzqd0id30rqpjhmawszr1hnnd1";
  };

  warp-tls = callHackageDirect {
    pkg = "warp-tls";
    ver = "3.2.8";
    sha256 = "0bf6gnyz9pq57y3hgv1xpfi1cnsda0wrwyd18zmh2220hxmvda71";
  };

}
