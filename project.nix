{
  pactRef ? "34783f9639a87b391382fe063e87737766fcdbb1"
, pactSha ? "17sgvb1b0hy83bh59vbxcz0rjlz0xl7h1isky6syisw589yw0p2v"
}:

let
pactSrc = builtins.fetchTarball {
  url = "https://github.com/kadena-io/pact/archive/${pactRef}.tar.gz";
  sha256 = pactSha;
};
pactProj = "${pactSrc}/project.nix";

in
  (import pactProj {}).rp.project ({ pkgs, hackGet, ... }:
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
    overrides = import ./overrides.nix pactSrc pkgs hackGet;

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
