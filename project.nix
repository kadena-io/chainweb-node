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
        rev = "9838d1266b9ee43c88af6c01cd819e0c96b685e6";
        sha256 = "01kg3ir7105bd373g2b1kc9l5wv0pm0wjnw2cq46vciw1i9vbqcw";
      }) {});

      # ###################################################################### #
      # Upgrade to random 1.2
      # (Most pins are because of QuickCheck and quickcheck-instances)

      random = dontCheck (self.callHackageDirect {
        pkg = "random";
        ver = "1.2.0";
        sha256 = "06s3mmqbsfwv09j2s45qnd66nrxfp9280gnl9ng8yh128pfr7bjh";
      } {});

      # cuckoo = dontCheck (self.callHackageDirect {
      #   pkg = "cuckoo";
      #   ver = "0.2.1";
      #   sha256 = "1dsac9qc90aagcgvznzfjd4wl8wgxhq1m8f5h556ys72nkm1ablx";
      # } {});

      splitmix = dontCheck (self.callHackageDirect {
        pkg = "splitmix";
        ver = "0.1.0.3";
        sha256 = "08xj8f6kg6liwycpppnq0aj6j3xwv8748kks232avscx9nz9lycv";
      } {});

      uuid = dontCheck (self.callHackageDirect {
        pkg = "uuid";
        ver = "1.3.14";
        sha256 = "068cydiav1wrsb4s34si7h4ynsraqpsp0mabc6ikp8wmbyqvhyrr";
      } {});

      uuid-types = dontCheck (self.callHackageDirect {
        pkg = "uuid-types";
        ver = "1.0.4";
        sha256 = "1c5dph5hx82ij78wjixsz1d169a6wrdx74v7dm78i4z1rfc9dfar";
      } {});

      QuickCheck = dontCheck (self.callHackageDirect {
        pkg = "QuickCheck";
        ver = "2.14.2";
        sha256 = "0rx4lz5rj0s1v451cq6qdxhilq4rv9b9lnq6frm18h64civ2pwbq";
      } {});

      servant = dontCheck (self.callHackageDirect {
        pkg = "servant";
        ver = "0.18.2";
        sha256 = "0l2k895nxvw2ngr9201g3br6s9zab7mk5mhpjibyg8mxfbv75a8y";
      } {});

      servant-client = dontCheck (self.callHackageDirect {
        pkg = "servant-client";
        ver = "0.18.2";
        sha256 = "0yip2s63ivrlrpficdipq60j2a6czg8agn18lpkkaxf3n55j4jr3";
      } {});

      servant-client-core = dontCheck (self.callHackageDirect {
        pkg = "servant-client-core";
        ver = "0.18.2";
        sha256 = "1hazxk1laklpm2c65zgkk2gn8mvlp682437071s04bqggk9b59sx";
      } {});

      servant-server = dontCheck (self.callHackageDirect {
        pkg = "servant-server";
        ver = "0.18.2";
        sha256 = "1kynxl7qg5z45bhi0k61sxn79xkgnq1z97ccqqs39wjyf45fj5yy";
      } {});

      servant-swagger = dontCheck (self.callHackageDirect {
        pkg = "servant-swagger";
        ver = "1.1.10";
        sha256 = "1fymcnaxl10rrhz0q9l7ri3llv9rkixxc05wmh6lc7mj1fdqyjzv";
      } {});

      swagger2 = dontCheck (self.callHackageDirect {
        pkg = "swagger2";
        ver = "2.6";
        sha256 = "0x0s34q9bmrik0vmzpc08r4jq5kbpb8x7h19ixhaakiafpjfm59l";
      } {});

      quickcheck-instances = dontCheck (self.callHackageDirect {
        pkg = "quickcheck-instances";
        ver = "0.3.25.2";
        sha256 = "18hr1cmgghsmpxjlkc17r2rmm8n9n91ld5y5srysy7fbl0g706px";
      } {});

      data-fix = dontCheck (self.callHackageDirect {
        pkg = "data-fix";
        ver = "0.3.1";
        sha256 = "0d9rj6kbaqyr9waj4yih2pp3qaswcip51jn9zdvs83gg8m29pqkj";
      } {});

      strict = dontCheck (self.callHackageDirect {
        pkg = "strict";
        ver = "0.4.0.1";
        sha256 = "0xhr98m2632k2pic8q9bpnm3mp9098mmg4s66ds052b92494k49f";
      } {});

      these = dontCheck (self.callHackageDirect {
        pkg = "these";
        ver = "1.1.1.1";
        sha256 = "1i1nfh41vflvqxi8w8n2s35ymx2z9119dg5zmd2r23ya7vwvaka1";
      } {});

      time-compat = dontCheck (self.callHackageDirect {
        pkg = "time-compat";
        ver = "1.9.5";
        sha256 = "0xy044x713bbvl8i1180bnccn60ji1n7mw1scs9ydy615bgwr82c";
      } {});

      # End of random 1.2 update
      # ###################################################################### #

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
