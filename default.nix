{ system ? builtins.currentSystem }:
(import ./project.nix { inherit system; }).ghc.chainweb
