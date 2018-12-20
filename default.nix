# To pin to a specific version of nixpkgs, you can substitute <nixpkgs> with:
# `(builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/<nixpkgs_commit_hash>.tar.gz")`
{ compiler ? "ghc844"
, owner ? "NixOS"
, rev ? "7b54e50c085501c995e38130eb6f5e3b7a8c2523"
, sha ? "1l3083b0fjhnx5lq89bxjvimp0ax4j8shra2z3h6xnp0bd2rjgvv"
, pkgs ? import (builtins.fetchTarball {
                   url = "https://github.com/${owner}/nixpkgs/archive/${rev}.tar.gz";
                   sha256 = sha; }) { config.allowUnfree = true; }
}:
  pkgs.haskell.packages.${compiler}.developPackage {
    name = "chainweb";
    root = builtins.filterSource
      (path: type: !(builtins.elem (baseNameOf path) ["result" "dist" "dist-newstyle" ".git" ".stack-work"]))
      ./.;
    overrides = self: super: with pkgs.haskell.lib; {
      # Don't run a package's test suite
      # foo = dontCheck super.chainweb;
      #
      # Don't enforce package's version constraints
      # bar = doJailbreak super.bar;
      #
      # To discover more functions that can be used to modify haskell
      # packages, run "nix repl", type "pkgs = import <nixpkgs> {}", hit
      # enter, then type "pkgs.haskell.lib.", then hit <TAB> to get a
      # tab-completed list of functions.
      configuration-tools = dontCheck super.configuration-tools;
    };
    source-overrides = {
      # Use a specific hackage version (if nixpkgs knows about it)
      # foo = "0.11.3.1";
      configuration-tools = "0.4.0";
      aeson = "1.4.0.0";

      # streaming >=0.2.2 is on Hackage, but nix doesn't know how get it from there
      streaming = pkgs.fetchFromGitHub {
        owner = "haskell-streaming";
        repo = "streaming";
        rev = "cf4ae245a0c31432776c9ae6c11312c25a8c6c88";
        sha256 = "1b8m1ia2qp3rh09cyacsx9cjb20hdk7vlylc166wmnpwl46g4dvx";
      };

        configuration-tools = self.callHackage "configuration-tools" "0.4.0" {};

        # Use a particular commit from github
        pact = addBuildDepend (self.callCabal2nix "pact" (pkgs.fetchFromGitHub {
          owner = "kadena-io";
          repo = "pact";
          rev = "3316593a690444e0290dfc3605b55dc5aa7e43c9";
          sha256 = "0h5nlwklha8kgcgxybcb5g2ldfs8x78paqr8riaja73ww7ydf496";
        }) {}) pkgs.z3;

        # Use a particular commit from github
        refined = self.callCabal2nix "refined" (pkgs.fetchFromGitHub {
          owner = "nikita-volkov";
          repo = "refined";
          rev = "70c896c10e2aeac4b7f9a328b394b03809351e4c";
          sha256 = "1yjwbrzhr08dgkh2rwrija9i9f3bhfsf8hkc5dih0a58y9h9y8i3";
        }) {};

        # streaming-0.2.2.0
        streaming = self.callCabal2nix "streaming" (pkgs.fetchFromGitHub {
          owner = "haskell-streaming";
          repo = "streaming";
          rev = "cf4ae245a0c31432776c9ae6c11312c25a8c6c88";
          sha256 = "1b8m1ia2qp3rh09cyacsx9cjb20hdk7vlylc166wmnpwl46g4dvx";
        }) {};

        #yet-another-logger = self.callCabal2nix "yet-another-logger" (pkgs.fetchFromGitHub {
        #  owner = "alephcloud";
        #  repo = "hs-yet-another-logger";
        #  rev = "5706c1bb57e18f2793da21808868d947f74005f0";
        #  sha256 = "1lkg6p8s3j48q6cq27k9sldd1f8aqd6b77rsa0vbbzsi69idqb17";
        #}) {};

        ######################################################################
        # Dependencies from pact
        # pact = setFlags (addBuildDepend super.pact pkgs.z3);
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

        hedgehog = self.callCabal2nix "hedgehog" (pkgs.fetchFromGitHub {
          owner = "hedgehogqa";
          repo = "haskell-hedgehog";
          rev = "38146de29c97c867cff52fb36367ff9a65306d76";
          sha256 = "1z8d3hqrgld1z1fvjd0czksga9lma83caa2fycw0a5gfbs8sh4zh";
        } + "/hedgehog") {};
        hlint = self.callHackage "hlint" "2.0.14" {};
        # hoogle = self.callHackage "hoogle" "5.0.15" {};

        # sbv >= 7.9
        sbv = dontCheck (self.callCabal2nix "sbv" (pkgs.fetchFromGitHub {
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
      chainweb = builtins.filterSource
        (path: type: !(builtins.elem (baseNameOf path)
           ["result" "dist" "dist-newstyle" "dist-ghcjs" ".git" ".stack-work"]))
        ./.;
    };
    shellToolOverrides = ghc: super: {
      stack = pkgs.stack;
    };
    shells = {
      ghc = ["chainweb"];
    };

  })
