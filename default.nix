{ system ? builtins.currentSystem }:
(import ./project.nix {}).proj.ghc.chainweb
