self: super:

let
  lib = self.lib;
  hlib = self.haskell.lib.compose;

  overrideGHC = version: super.haskell-nix.compiler."${version}".override;
  debugFlavour = "release+llvm+split_sections+debug_info";
in
{
  haskell-nix = lib.recursiveUpdate super.haskell-nix {
    compiler = {
      ghc963 = overrideGHC "ghc963" {
        ghcFlavour = debugFlavour;
      };

      ghc981 = overrideGHC "ghc981" {
        ghcFlavour = debugFlavour;
      };
    };
  };
}
