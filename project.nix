{ system ? builtins.currentSystem
, kpkgs ? import ./dep/kpkgs {}
}:

let
proj = kpkgs.rp.project ({ pkgs, hackGet, ... }: with pkgs.haskell.lib;

  let
  # Includes test suite and benchmark binaries in the output derivation.
  # Has the side effect of causing nix-build to not run them.
  convertCabalTestsAndBenchmarksToExecutables = p:
    overrideCabal p (drv: {
      preConfigure = (drv.preConfigure or "") + ''
        sed -i -e 's/^\(test-suite\|benchmark\) /executable /' -e '/^ *type: *exitcode-stdio-1.0$/d' *.cabal
      '';
    });
  # Nixpkgs with the `find-libs` command added as a top-level package
  nixpkgsWithFindLibs = import (pkgs.fetchFromGitHub {
    owner = "ryantrinkle";
    repo = "nixpkgs";
    rev = "a9a141a10f32691228ae7668e571359f2aaf82e4";
    sha256 = "0ncqzsq3rwkh4g68bxsxmznq4z4pcqy6wclslnrsnqbq1b2lk2gl";
  }) {};
  in {
    name = "chainweb";
    overrides = self: super: {
      chainweb = enableCabalFlag (
        justStaticExecutables (enableDWARFDebugging (convertCabalTestsAndBenchmarksToExecutables super.chainweb))) "use_systemd";
      chainweb-portable = nixpkgsWithFindLibs.callPackage ./make-portable.nix {} "/opt/chainweb" self.chainweb;
    };

    packages = {
      chainweb = kpkgs.gitignoreSource ./.;
    };

    shellToolOverrides = ghc: super: {
      dnsutils = pkgs.dnsutils;
      stack = pkgs.stack;
      cabal-install = pkgs.haskellPackages.cabal-install;
      ghcid = pkgs.haskellPackages.ghcid;
      z3 = pkgs.z3;
    };

    shells = {
      ghc = ["chainweb"];
    };
  });

in { inherit proj; }
