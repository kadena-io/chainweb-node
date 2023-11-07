{
  description = "Chainweb";

  inputs = {
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    hs-nix-infra = {
      url = "github:kadena-io/hs-nix-infra/enis/experiment-with-recursive-inputs";
      inputs.hackage.follows = "hackage";
    };
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  nixConfig = {
    extra-substituters = "https://nixcache.chainweb.com https://cache.iog.io";
    trusted-public-keys = "nixcache.chainweb.com:FVN503ABX9F8x8K0ptnc99XEz5SaA4Sks6kNcZn2pBY= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
  };

  outputs = { self, hs-nix-infra, flake-utils, nix-filter, ... }:
    flake-utils.lib.eachSystem
      [ "x86_64-linux" "x86_64-darwin"
        "aarch64-linux" "aarch64-darwin" ] (system:
    let
      inherit (hs-nix-infra) nixpkgs haskellNix;
      basePkgs = hs-nix-infra.nixpkgs.legacyPackages.${system};
      pkgs = import nixpkgs {
        inherit system;
        inherit (haskellNix) config;
        overlays = [ haskellNix.overlay ];
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
      runRecursive = hs-nix-infra.lib.recursiveRawFlakeBuilder basePkgs self;
    in {
      packages = {
        default = executables;
        recursive = runRecursive "chainweb"
          {
            outputs = [ "out" "metadata" ];
          } ''
            mkdir -p $out
            ln -s $(nix build ${./.}#default --print-out-paths)/bin $out/bin
            cp $(nix build ${./.}#default.metadata --print-out-paths) $metadata
          '';
        check = pkgs.runCommand "check" {} ''
          echo ${mkCheck "chainweb" executables}
          echo ${mkCheck "devShell" flake.devShell}
          echo works > $out
        '';
      };
      tmp = {
        someVal = 123;
        someDeriv = basePkgs.hello;
      };
    });
}
