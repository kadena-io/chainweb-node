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
      configuration-tools = dontBenchmark (dontCheck (self.callHackageDirect {
        pkg = "configuration-tools";
        ver = "0.6.0";
        sha256 = "0ia2bhy35qv1xgbqrx0jalxznj8zgg97y0zkp8cnr1r3pq5adbcd";
       } {}));
      cuckoo = dontBenchmark (dontCheck (self.callHackageDirect {
        pkg = "cuckoo";
        ver = "0.3.0";
        sha256 = "172km2552ipi9fqjmd16b4jmqw5a1414976p6xf8bxc83shxp97p";
       } {}));
      hashes = dontCheck (self.callHackageDirect {
        pkg = "hashes";
        ver = "0.1.0.1";
        sha256 = "09n2k0vwwlzjy8ax5dlq3743qkcsd21gwfibqfjmqirv30lgb5b5";
      } {});
      quickcheck-classes-base = dontCheck (self.callHackageDirect {
        pkg = "quickcheck-classes-base";
        ver = "0.6.0.0";
        sha256 = "1mmhfp95wqg6i5gzap4b4g87zgbj46nnpir56hqah97igsbvis70";
      } {});
      pact-time = dontCheck (self.callHackageDirect {
        pkg = "pact-time";
        ver = "0.2.0.0";
        sha256 = "1cfn74j6dr4279bil9k0n1wff074sdlz6g1haqyyy38wm5mdd7mr";
      } {});

      # TODO Replace with kpkgs bump after everything is ready
      pact = dontCheck (appendConfigureFlag (self.callCabal2nix "pact" (pkgs.fetchFromGitHub {
        owner = "kadena-io";
        repo = "pact";
        rev = "dfc2d208af13fe0469776cd375f48b322e23971b";
        sha256 = "1zm5s58i7xgwm1b0c9zz5rzb7pdk7hfnpk4ncacahky7s6sv4xfp";
      }) {}) "-f-build-tool");

      ethereum = dontCheck (self.callCabal2nix "ethereum" (pkgs.fetchFromGitHub {
        owner = "kadena-io";
        repo = "kadena-ethereum-bridge";
        rev = "10f21e96af1dce4f13e261be9dfad8c28cd299f7";
        sha256 = "1vab2m67ign6x77k1sjfjmv9sbrrl5sl2pl07rw1fw8bjqnp5vqk";
      }) {});

      chainweb-storage = dontCheck (self.callCabal2nix "chainweb-storage" (pkgs.fetchFromGitHub {
        owner = "kadena-io";
        repo = "chainweb-storage";
        rev = "07e7eb7596c7105aee42dbdb6edd10e3f23c0d7e";
        sha256 = "0piqlj9i858vmvmiis9i8k6cz7fh78zfaj47fsq5cs9v7zpj234z";
      }) {});
      nothunks = dontCheck (self.callHackageDirect {
        pkg = "nothunks";
        ver = "0.1.2";
        sha256 = "1xj5xvy3x3vixkj84cwsjl3m06z2zfszbcpxbz1j1ca83ha2gb7i";
      } {});
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
