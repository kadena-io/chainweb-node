{ compiler ? "ghc8104"
, rev      ? "7e9b0dff974c89e070da1ad85713ff3c20b0ca97"
, sha256   ? "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36"
, pkgs     ?
    import (builtins.fetchTarball {
      url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      inherit sha256; }) {
      config.allowBroken = false;
      config.allowUnfree = true;
    }
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
      ap-normalize = dontCheck super.ap-normalize;
      rosetta = self.callCabal2nix "rosetta" (pkgs.fetchFromGitHub {
        owner = "kadena-io";
        repo = "rosetta";
        rev = "6c8dd2eea1f6d0dba925646dbcb6e07feeccbfd5";
        sha256 = "19pjy06xrx2siggzybcmly0qaq4ds3yzxcsvqwgs4qh9kkzh0kqh";
      }) {};
      mwc-random = self.callHackageDirect {
        pkg = "mwc-random";
        ver = "0.15.0.2";
        sha256 = "1mpill3lwrrhlzq0ccs8wyzsqhy1a2hmva17qxpgsy2zzqxi1nx1";
       } {};
      resource-pool = self.callHackageDirect {
        pkg = "resource-pool";
        ver = "0.3";
        sha256 = "1kqqclg48s7x4i2sfr5pvzry3jq59794zdmmydk7rr59vh1gbmh0";
      } {};
      hashable = dontCheck super.hashable;
      random = dontCheck (self.callHackageDirect {
        pkg = "random";
        ver = "1.2.1";
        sha256 = "1j146hacca6drd9fvziw8an6zawqllxll3cl9hbwmz0nbj9xa6kb";
       } {});
      network = self.callHackageDirect {
        pkg = "network";
        ver = "3.1.2.2";
        sha256 = "1kqqclg48s7x4i2sfr5pvzry3jq59794zdmmydk7rr59vh1gbmh4";
       } {};
      base64-bytestring = dontBenchmark (dontCheck (self.callHackageDirect {
        pkg = "base64-bytestring";
        ver = "1.0.0.3";
        sha256 = "1wx3zdx5amjyawqlfv2i3mishvvh4pkdk9nh7y8f4d38ykri2bx0";
       } {}));
      base16-bytestring = dontBenchmark (dontCheck (self.callHackageDirect {
        pkg = "base16-bytestring";
        ver = "0.1.1.7";
        sha256 = "0sv4gvaz1hwllv7dpm8b8xkrpsi1bllgra2niiwpszpq14bzpail";
       } {}));
      configuration-tools = dontBenchmark (dontCheck (self.callHackageDirect {
        pkg = "configuration-tools";
        ver = "0.6.0";
        sha256 = "0ia2bhy35qv1xgbqrx0jalxznj8zgg97y0zkp8cnr1r3pq5adbcd";
       } {}));
      cuckoo = dontBenchmark (dontCheck (self.callHackageDirect {
        pkg = "cuckoo";
        ver = "0.3.0";
        sha256 = "172km2552ipi9fqjmd16b4jmqw5a1414976p6xf8bxc83shxp97p";
       } {}));
      hashes = dontCheck (self.callHackageDirect {
        pkg = "hashes";
        ver = "0.1.0.1";
        sha256 = "09n2k0vwwlzjy8ax5dlq3743qkcsd21gwfibqfjmqirv30lgb5b5";
      } {});
      prettyprinter = dontCheck (self.callHackageDirect {
        pkg = "prettyprinter";
        ver = "1.6.0";
        sha256 = "0f8wqaj3cv3yra938afqf62wrvq20yv9jd048miw5zrfavw824aa";
      } {});

      quickcheck-classes-base = dontCheck (self.callHackageDirect {
        pkg = "quickcheck-classes-base";
        ver = "0.6.0.0";
        sha256 = "1mmhfp95wqg6i5gzap4b4g87zgbj46nnpir56hqah97igsbvis70";
      } {});
      pact-time = dontCheck (self.callHackageDirect {
        pkg = "pact-time";
        ver = "0.2.0.0";
        sha256 = "1cfn74j6dr4279bil9k0n1wff074sdlz6g1haqyyy38wm5mdd7mr";
      } {});

      pact = dontCheck (appendConfigureFlag super.pact "-f-build-tool");

      ethereum = dontCheck (self.callCabal2nix "ethereum" (pkgs.fetchFromGitHub {
        owner = "kadena-io";
        repo = "kadena-ethereum-bridge";
        rev = "10f21e96af1dce4f13e261be9dfad8c28cd299f7";
        sha256 = "1vab2m67ign6x77k1sjfjmv9sbrrl5sl2pl07rw1fw8bjqnp5vqk";
      }) {});

      chainweb-storage = dontCheck super.chainweb-storage;

      nothunks = dontCheck (self.callHackageDirect {
        pkg = "nothunks";
        ver = "0.1.2";
        sha256 = "1xj5xvy3x3vixkj84cwsjl3m06z2zfszbcpxbz1j1ca83ha2gb7i";
      } {});


      fast-logger = self.callHackageDirect {
        pkg = "fast-logger";
        ver = "2.4.17";
        sha256 = "1whnbdzcfng6zknsvwgk4cxhjdvwak7yxwykwkh2mlv9ykz8b6iw";
      } {};

      http2 = self.callHackageDirect {
        pkg = "http2";
        ver = "2.0.3";
        sha256 = "14bqmxla0id956y37fpfx9v6crwxphbfxkl8v8annrs8ngfbhbr7";
      } {};

      unordered-containers = dontCheck (self.callHackageDirect {
        pkg = "unordered-containers";
        ver = "0.2.15.0";
        sha256 = "101fjg7jsa0mw57clpjwc2vgrdkrnn0vmf4xgagja21ynwwbl2b5";
      } {});

      wai = dontCheck (self.callHackageDirect {
        pkg = "wai";
        ver = "3.2.2.1";
        sha256 = "0msyixvsk37qsdn3idqxb4sab7bw4v9657nl4xzrwjdkihy411jf";
      } {});

      wai-cors = dontCheck (self.callHackageDirect {
        pkg = "wai-cors";
        ver = "0.2.7";
        sha256 = "10yhjjkzp0ichf9ijiadliafriwh96f194c2g02anvz451capm6i";
      } {});

      wai-logger = self.callHackageDirect {
        pkg = "wai-logger";
        ver = "2.3.5";
        sha256 = "1iv6q7kpa9irjyjv9238pfqqzn7w92ccich5h8xbmv3r8qxwmvld";
      } {};

      wai-middleware-throttle = dontCheck (self.callHackageDirect {
        pkg = "wai-middleware-throttle";
        ver = "0.3.0.1";
        sha256 = "13pz31pl7bk51brc88jp0gffjx80w35kzzrv248w27d7dc8xc63x";
      } {});

      wai-extra = self.callHackageDirect {
        pkg = "wai-extra";
        ver = "3.1.2";
        sha256 = "1cha6hvb071bknw25va07vg1sr5bg44q8fwz0nwa1886j9d4yrr7";
      } {};

      wai-app-static = doJailbreak (dontCheck (self.callHackageDirect {
        pkg = "wai-app-static";
        ver = "3.1.7.2";
        sha256 = "184ql2k7b5i0y3b34kpcv0mxvzbhd1z5wa277z3nd67v48slax7a";
      } {}));

      warp = dontCheck (self.callHackageDirect {
        pkg = "warp";
        ver = "3.3.6";
        sha256 = "044w7ajkqlwnrpzc4zaqy284ac9wsklyby946jgfpqyjbj87985x";
      } {});

      warp-tls = self.callHackageDirect {
        pkg = "warp-tls";
        ver = "3.2.10";
        sha256 = "1zgr83zkb3q4qa03msfnncwxkmvk63gd8sqkbbd1cwhvjragn4mz";
      } {};

  };
  source-overrides = {
    chainweb-storage = nix-thunk.thunkSource ./dep/chainweb-storage;
    pact = nix-thunk.thunkSource ./dep/pact;
  };
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      pkgs.zlib
      pkgs.haskell.packages.${compiler}.cabal-install
      pkgs.haskell.packages.${compiler}.ghcid
    ];
  });
}
