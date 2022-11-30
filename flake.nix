{
  description = "Chainweb";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=7a94fcdda304d143f9a40006c033d7e190311b54";
    # nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
    pact.url = ../pact;
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, pact }:
    flake-utils.lib.eachSystem
      [ "x86_64-linux" "x86_64-darwin"
        "aarch64-linux" "aarch64-darwin" ] (system:
    let
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.chainweb-node.flake {
        # crossPlatforms = p: [ p.ghcjs ];
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          chainweb-node =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              shell.tools = {
                cabal = {};
                # hlint = {};
              };
              shell.buildInputs = with pkgs; [
                pact
                zlib
                pkgconfig
              ];
              # shell.crossPlatforms = p: [ p.ghcjs ];
            };
        })
      ];
    in flake // {
      packages.default = flake.packages."chainweb-node:exe:chainweb-node";
    });
}
