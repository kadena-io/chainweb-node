# Includes test suite and benchmark binaries in the output derivation.
# Has the side effect of causing nix-build to not run them.
{ externalTestsAndBench ? false
}:
(import ./project.nix { inherit externalTestsAndBench; }).ghc.chainweb
