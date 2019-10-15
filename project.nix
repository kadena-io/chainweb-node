{ pactRef ? "5154b06995502cd975a971d49b22136ca7a521b7"
, pactSha ? "0l4pjbjkvv6x2kds138mlv3lgcbw4b1pdj2j7x9bcghqs3f0h1z5"
}:

let
pactSrc = builtins.fetchTarball {
  url = "https://github.com/kadena-io/pact/archive/${pactRef}.tar.gz";
  sha256 = pactSha;
};

in
  (import pactSrc {}).rp.project ({ pkgs, ... }:
let

gitignoreSrc = pkgs.fetchFromGitHub {
  owner = "hercules-ci";
  repo = "gitignore";
  rev = "f9e996052b5af4032fe6150bba4a6fe4f7b9d698";
  sha256 = "0jrh5ghisaqdd0vldbywags20m2cxpkbbk5jjjmwaw0gr8nhsafv";
};
inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;

in {
    name = "chainweb";
    overrides = import ./overrides.nix pactSrc pkgs;

    packages = {
      chainweb = gitignoreSource ./.;
      #chainweb = gitignoreFilter
      #  [ ".git" ".gitlab-ci.yml" "CHANGELOG.md" "README.md" "future-work.md" ] ./.;
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
