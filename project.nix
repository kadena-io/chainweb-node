{ pactRef ? "731a9c07dd7486dc8d5c1b11905ceb34d9bd8af0"
, pactSha ? "0gq0yngz69jxv4zxazclq3i5bwsics15f6z0ca5s7cmhg2jy5qbr"
, system ? builtins.currentSystem
, kpkgs ? import ./dep/kpkgs {}
}:

let
pactSrc = builtins.fetchTarball {
  url = "https://github.com/kadena-io/pact/archive/${pactRef}.tar.gz";
  sha256 = pactSha;
};

proj = kpkgs.rp.project ({ pkgs, hackGet, ... }: with pkgs.haskell.lib; {
    name = "chainweb";
    overrides = self: super: (import ./overrides.nix { inherit pkgs; } self super) // {
      pact = dontCheck ( addBuildDepend (self.callCabal2nix "pact" pactSrc {}) pkgs.z3);
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
