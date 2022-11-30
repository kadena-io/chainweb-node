{
  description = "Chainweb";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=7a94fcdda304d143f9a40006c033d7e190311b54";
    # nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
    # pact.url = "github:kadena-io/pact?rev=53c574b92c16faeb9e6134c3d878cd32d606fb5c";
    # pact.url = "git+file:///Users/johnw/kadena/work/nix-flake/pact?rev=53c574b92c16faeb9e6134c3d878cd32d606fb5c";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
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
                # pact.packages.default
                zlib
                pkgconfig
              ];
            };
        })
      ];
    in flake // {
      packages.default = flake.packages."chainweb-node:exe:chainweb-node";
    });
}
