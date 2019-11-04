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

gitignoreSrc = pkgs.callPackage (pkgs.fetchFromGitHub {
  owner = "Fresheyeball";
  repo = "nix-gitignore";
  rev = "d27bfb07dc63e36d5ea53f375f023f9ae49aea1d";
  sha256 = "19j47sjxj2fm36y50gfx3kvyxwbiscja45gmbkb01vnqck80rlq6";
}) {};
inherit (gitignoreSrc) gitignoreSource;

in {
    name = "chainweb";
    overrides = import ./overrides.nix pactSrc pkgs hackGet;

    packages = {
      chainweb = gitignoreSource [ ".gitlab-ci.yml" "*.md" "**/*.md" ] ./.;
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
