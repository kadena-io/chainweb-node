self: super:

let
  lib = self.lib;
  hlib = self.haskell.lib.compose;
in
{
  haskell-nix = lib.recursiveUpdate super.haskell-nix {
    compiler.ghc963 = super.haskell-nix.compiler.ghc963.override {
      ghcFlavour = "release+llvm+split_sections+debug_info";
    };
  };
}
