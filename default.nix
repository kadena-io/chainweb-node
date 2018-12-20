{ rpRef ? "f3ff81d519b226752c83eefd4df6718539c3efdc"
, rpSha ? "1ijxfwl36b9b2j4p9j3bv8vf7qfi570m1c5fjyvyac0gy0vi5g8j"
, system ? builtins.currentSystem
}:

let rp = builtins.fetchTarball {
      url = "https://github.com/reflex-frp/reflex-platform/archive/${rpRef}.tar.gz";
      sha256 = rpSha;
    };

in
  (import rp {}).project ({ pkgs, ... }:
  let gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
        owner = "siers";
        repo = "nix-gitignore";
        rev = "ee230196f4ce4778bfe0e84429966a343fcc5248";
        sha256 = "10md0iaic858fvwvc71qs50w13240fwq5rvza9kd2la3s94mzawy";
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

        chainweb = doCoverage (doHaddock super.chainweb);
        configuration-tools = self.callHackage "configuration-tools" "0.4.0" {};

        # pact-2.6.1
        pact = addBuildDepend (self.callCabal2nix "pact" (pkgs.fetchFromGitHub {
          owner = "kadena-io";
          repo = "pact";
          rev = "3316593a690444e0290dfc3605b55dc5aa7e43c9";
          sha256 = "0h5nlwklha8kgcgxybcb5g2ldfs8x78paqr8riaja73ww7ydf496";
        }) {}) pkgs.z3;

        streaming = callHackageDirect {
          pkg = "streaming";
          ver = "0.2.2.0";
          sha256 = "0yym840jnh2cma5n4c0pv3nh1hyhag1v6pks73wdikhrcajffsh3";
        };

        yet-another-logger = callHackageDirect {
          pkg = "yet-another-logger";
          ver = "0.3.1";
          sha256 = "17i5km3bxlp568q9pbnbp2nvpfgnmccpfnvcci0z1f56cw95679n";
        };

        ######################################################################
        # Dependencies from pact
        # pact = addBuildDepend super.pact pkgs.z3;

        # tests for extra were failing due to an import clash (`isWindows`)
        extra = dontCheck super.extra;

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
          rev = "3dc60340634c82f39f6c5dca2b3859d10925cfdf";
          sha256 = "18xcxg1h19zx6gdzk3dfs87447k3xjqn40raghjz53bg5k8cdc31";
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
      chainweb = gitignore.gitignoreSource [] ./.;
    };
    shellToolOverrides = ghc: super: {
      stack = pkgs.stack;
    };
    shells = {
      ghc = ["chainweb"];
    };

  })
