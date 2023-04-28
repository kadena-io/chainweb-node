{
  description = "Chainweb";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=4d2b37a84fad1091b9de401eb450aae66f1a741e";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, nix-filter }:
    flake-utils.lib.eachSystem
      [ "x86_64-linux" "x86_64-darwin"
        "aarch64-linux" "aarch64-darwin" ] (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        inherit (haskellNix) config;
        overlays = [ haskellNix.overlay ];
      };
      defaultNix = import ./default-flake.nix {
        inherit pkgs nix-filter;
        flakePath = self.outPath;
      };
      flake = defaultNix.flake;
      executable = defaultNix.default;
      check-cabal-project = pkgs.writeShellScript "check-cabal-project" ''
        PATH=${pkgs.nix-prefetch-git}/bin:$PATH
        CABAL_PROJECT_PATH=${./cabal.project}
        . ${nix/check_cabal_project.sh}
      '';
    in nixpkgs.lib.recursiveUpdate flake {
      packages.default = executable;
      # This package depends on other packages at buildtime, but its output does not
      # depend on them. This way, we don't have to download the entire closure to verify
      # that those packages build.
      packages.check = pkgs.runCommand "check" {} ''
        echo Checking the hashes in the cabal.project file
        ${check-cabal-project}
        echo chainweb-node: ${executable}
        echo devShell: ${flake.devShell}
        echo works > $out
      '';
      packages.check-cabal-project = check-cabal-project;
    });
}
