let flakeDefaultNix = (import (
      fetchTarball {
         url = "https://github.com/edolstra/flake-compat/archive/35bb57c0c8d8b62bbfd284272c928ceb64ddbde9.tar.gz";
         sha256 = "1prd9b1xx8c0sfwnyzkspplh30m613j42l1k789s521f4kv4c2z2"; }
     ) {
       src =  ./.;
     }).defaultNix;
    inputs = flakeDefaultNix.inputs;
    pkgsDef = import inputs.nixpkgs (import inputs.haskellNix {}).nixpkgsArgs;
in
{ pkgs ? pkgsDef
, compiler ? "ghc8107"
, flakePath ? flakeDefaultNix.outPath
, nix-filter ? inputs.nix-filter
, ...
}:
let haskellSrc = with nix-filter.lib; filter {
      root = flakePath;
      exclude = [
        ".github"
        ".gitignore"
        ".gitattributes"
        "docs"
        "examples"
        (matchExt "nix")
        "flake.lock"
      ];
    };
    chainweb-node = pkgs.haskell-nix.project' {
      src = haskellSrc;
      compiler-nix-name = compiler;
      projectFileName = "cabal.project";
      shell.tools = {
        cabal = {};
      };
      shell.buildInputs = with pkgs; [
        zlib
        pkgconfig
      ];
      modules = [
        {
          packages.http2.doHaddock = false;
        }
      ];
    };
    flake = chainweb-node.flake {};
    default = flake.packages."chainweb:exe:chainweb-node";
in {
  inherit flake default haskellSrc;
}
