# To pin to a specific version of nixpkgs, you can substitute <nixpkgs> with:
# `(builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/<nixpkgs_commit_hash>.tar.gz")`
{ compiler ? "ghc822"
, pkgs     ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/6cbd1ec4b043df6f9f6e7c59c79e1025b66d5faa.tar.gz") {
    config.allowUnfree = true;
  }
}:
  pkgs.haskell.packages.${compiler}.developPackage {
    root = ./.;
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
    };
    source-overrides = {
      # Use a specific hackage version (if nixpkgs knows about it)
      # foo = "0.11.3.1";

      # Use a particular commit from github
      refined = pkgs.fetchFromGitHub {
        owner = "nikita-volkov";
        repo = "refined";
        rev = "cd3d99a6a9dff6adfc6dfceb06697ae56261ea6d";
        sha256 = "0ihgr7ldqjx7f3nzpf7g587s0i9acix9xwm9wwp0v6rs4jcdbbm7";
      };

      yet-another-logger = pkgs.fetchFromGitHub {
        owner = "alephcloud";
        repo = "hs-yet-another-logger";
        rev = "5706c1bb57e18f2793da21808868d947f74005f0";
        sha256 = "1lkg6p8s3j48q6cq27k9sldd1f8aqd6b77rsa0vbbzsi69idqb17";
      };
    };
  }
