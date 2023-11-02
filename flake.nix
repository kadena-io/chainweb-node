{
  description = "Chainweb";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=4d2b37a84fad1091b9de401eb450aae66f1a741e";
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
    empty = {
      url = "github:kadena-io/empty";
      flake = false;
    };
    # By default we use the pact specified in the cabal.project
    pact.follows = "empty";
  };

  nixConfig = {
    extra-substituters = "https://nixcache.chainweb.com https://cache.iog.io";
    trusted-public-keys = "nixcache.chainweb.com:FVN503ABX9F8x8K0ptnc99XEz5SaA4Sks6kNcZn2pBY= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, nix-filter, ... }:
    flake-utils.lib.eachSystem
      [ "x86_64-linux" "x86_64-darwin"
        "aarch64-linux" "aarch64-darwin" ] (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        inherit (haskellNix) config;
        overlays = [ haskellNix.overlay ];
      };
      mkDefaultNix = {
          pact ? if inputs.pact.outPath != inputs.empty.outPath then inputs.pact else null,
          enablePactBuildTool ? false,
        }: import ./default.nix {
          inherit pkgs nix-filter pact enablePactBuildTool;
          flakePath = self.outPath;
        };
      defaultNix = mkDefaultNix {};
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
      lib.mkChainwebProject = args: (mkDefaultNix args).chainweb;
      packages.default = executables;
      packages.check = pkgs.runCommand "check" {} ''
        echo ${mkCheck "chainweb" executables}
        echo ${mkCheck "devShell" flake.devShell}
        echo works > $out
      '';
    });
}
