{ pactRef ? "a2cc6ef0917c954fb8d51f0606a3c0cebca9cd55"
, pactSha ? "0v2mv5x1v23achfhppirxqsr30ad0fd626cnafzlmhf2fcaajv2n"
}:

let

pactSrc = builtins.fetchTarball {
  url = "https://github.com/kadena-io/pact/archive/${pactRef}.tar.gz";
  sha256 = pactSha;
};

in
  (import pactSrc {}).rp.project ({ pkgs, ... }:
let

gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
  owner = "siers";
  repo = "nix-gitignore";
  rev = "4f2d85f2f1aa4c6bff2d9fcfd3caad443f35476e";
  sha256 = "1vzfi3i3fpl8wqs1yq95jzdi6cpaby80n8xwnwa8h2jvcw3j7kdz";
}) {};

in {
    name = "chainweb";
    overrides = import ./overrides.nix pactSrc pkgs;

    packages = {
      chainweb = gitignore.gitignoreSource
        [ ".git" ".gitlab-ci.yml" "CHANGELOG.md" "README.md" "future-work.md" ] ./.;
    };

    shellToolOverrides = ghc: super: {
      stack = pkgs.stack;
      cabal-install = pkgs.haskellPackages.cabal-install;
      ghcid = pkgs.haskellPackages.ghcid;
      z3 = pkgs.z3;
    };

    shells = {
      ghc = ["chainweb"];
    };
  })
