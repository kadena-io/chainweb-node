{ system ? builtins.currentSystem }:
(import ./project.nix { inherit system; }).proj.ghc.chainweb
