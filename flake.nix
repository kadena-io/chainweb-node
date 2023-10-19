{
  description = "Chainweb";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs"; #url = "github:NixOS/nixpkgs?rev=4d2b37a84fad1091b9de401eb450aae66f1a741e";
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  nixConfig = {
    extra-substituters = "https://nixcache.chainweb.com https://cache.iog.io";
    trusted-public-keys = "nixcache.chainweb.com:FVN503ABX9F8x8K0ptnc99XEz5SaA4Sks6kNcZn2pBY= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, nix-filter, ... }:
    flake-utils.lib.eachSystem
      [ "x86_64-linux" "x86_64-darwin"
        "aarch64-linux" "aarch64-darwin" ] (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        inherit (haskellNix) config;
        overlays = [ haskellNix.overlay (import ./overlay-ghc963.nix) ];
      };
      defaultNix = import ./default.nix {
        inherit pkgs nix-filter;
        flakePath = self.outPath;
      };
      flake = defaultNix.flake;
      executables = defaultNix.default;
      # This package depends on other packages at buildtime, but its output does not
      # depend on them. This way, we don't have to download the entire closure to verify
      # that those packages build.
      mkCheck = name: package: pkgs.runCommand ("check-"+name) {} ''
        echo ${name}: ${package}
        echo works > $out
      '';
    in nixpkgs.lib.recursiveUpdate flake {
      packages.default = executables;
      packages.check = pkgs.runCommand "check" {} ''
        echo ${mkCheck "chainweb" executables}
        echo ${mkCheck "devShell" flake.devShell}
        echo works > $out
      '';
    });
}
