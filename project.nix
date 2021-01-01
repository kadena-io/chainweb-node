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
  in {
    name = "chainweb";
    overrides = self: super: {
      chainweb = enableCabalFlag (
        justStaticExecutables (enableDWARFDebugging (convertCabalTestsAndBenchmarksToExecutables super.chainweb))) "use_systemd";
      cuckoo = dontBenchmark (dontCheck (self.callHackageDirect {
        pkg = "cuckoo";
        ver = "0.2.1";
        sha256 = "1dsac9qc90aagcgvznzfjd4wl8wgxhq1m8f5h556ys72nkm1ablx";
       } {}));
      quickcheck-classes-base = dontCheck (self.callHackageDirect {
        pkg = "quickcheck-classes-base";
        ver = "0.6.0.0";
        sha256 = "1mmhfp95wqg6i5gzap4b4g87zgbj46nnpir56hqah97igsbvis70";
      } {});

      # TODO Replace with kpkgs bump after everything is ready
      pact = dontCheck (self.callCabal2nix "rosetta" (pkgs.fetchFromGitHub {
        owner = "kadena-io";
        repo = "pact";
        rev = "a9c9b92e313a680574479ffda4700533f19d16d5";
        sha256 = "1idnn3w0i3iq60j4wd5k3rs4c8kq14ii5jx100ka789c85sxhiks";
      }) {});

      ethereum = dontCheck (self.callCabal2nix "ethereum" (pkgs.fetchFromGitHub {
        owner = "kadena-io";
        repo = "kadena-ethereum-bridge";
        rev = "eac47f69541f844b6b93963961e94d8d0d91cc70";
        sha256 = "121wmqhf6rl83syr5rw0xy6mxcp0qs1yi76s80sbkc7a5bky3p70";
      }) {});
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
