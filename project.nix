{
  pactRef ? "5c25e8369917eaf07ef66ed3b265f15cb2190378"
, pactSha ? "0bqpkzi5d4hprb5ivybd9rh06x1nbxqv0cmgi8x7rjmyihksqlrx"
, system ? builtins.currentSystem
}:

let
pactSrc = builtins.fetchTarball {
  url = "https://github.com/kadena-io/pact/archive/${pactRef}.tar.gz";
  sha256 = pactSha;
};
pactProj = "${pactSrc}/project.nix";
rp = (import pactProj { inherit system; }).rp;
proj =
  rp.project ({ pkgs, hackGet, ... }:
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
      overrides = import ./overrides.nix { inherit pactSrc pkgs hackGet; };

      packages = {
        chainweb = gitignoreSource ./.;
        #chainweb = gitignoreFilter
        #  [ ".git" ".gitlab-ci.yml" "CHANGELOG.md" "README.md" "future-work.md" ] ./.;
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

in

{ inherit pactRef pactSrc rp proj; }
