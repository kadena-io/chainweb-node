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
      # chainweb = enableCabalFlag super.chainweb "dev";
      #
      # Don't enforce package's version constraints
      # bar = doJailbreak super.bar;
      #
      # To discover more functions that can be used to modify haskell
      # packages, run "nix-repl", type "pkgs.haskell.lib.", then hit
      # <TAB> to get a tab-completed list of functions.
    };
    source-overrides = {
      # Use a specific hackage version
      tasty-hunit = "0.9.2";

      # Use a particular commit from github
      yet-another-logger = pkgs.fetchFromGitHub
        { owner = "alephcloud";
          repo = "hs-yet-another-logger";
          rev = "a3a84c2ca26c88485e5663391454b434ff36726b";
          sha256 = "06qxhjah7sc1d6ir38bs1dmmgxc158cj35xfqdss4kbksbc1b351";
        };
    };
  }
