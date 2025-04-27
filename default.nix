let flakeDefaultNix = (import (
      fetchTarball {
         url = "https://github.com/edolstra/flake-compat/archive/35bb57c0c8d8b62bbfd284272c928ceb64ddbde9.tar.gz";
         sha256 = "1prd9b1xx8c0sfwnyzkspplh30m613j42l1k789s521f4kv4c2z2"; }
     ) {
       src =  ./.;
     }).defaultNix;
    inputs = flakeDefaultNix.inputs;
    hs-nix-infra = inputs.hs-nix-infra;
    pkgsDef = import hs-nix-infra.nixpkgs {
      config = hs-nix-infra.haskellNix.config;
      overlays = [ hs-nix-infra.haskellNix.overlay] ;
    };
in
{ pkgs ? pkgsDef
, compiler ? "ghc982"
, flakePath ? flakeDefaultNix.outPath
, nix-filter ? inputs.nix-filter
, pact ? null
, enablePactBuildTool ? false
, ...
}:
let haskellSrc = with nix-filter.lib; filter {
      root = flakePath;
      exclude = [
        ".github"
        ".gitignore"
        ".gitattributes"
        "docs"
        "examples"
        (matchExt "nix")
        "flake.lock"
        "cabal.project.freeze"
        "dist-newstyle"
      ];
    };
    overridePact = pkgs.lib.optionalString (pact != null) ''
      # Remove the source-repository-package section for pact and inject the
      # override as a packages section
      ${pkgs.gawk}/bin/awk -i inplace '
        BEGINFILE { delete_block=0; buffer = ""; }
        /^$/ {
          if (delete_block == 0) print buffer "\n";
          buffer="";
          delete_block=0;
          next;
        }
        /location: https:\/\/github.com\/kadena-io\/pact.git/ { delete_block=1; }
        {
          if (delete_block == 0) {
            if (buffer != "") buffer = buffer "\n";
            buffer = buffer $0;
          }
        }
        ENDFILE { if (delete_block == 0) print buffer "\n"; }
      ' $out
      echo 'packages: ${pact}' >> $out
    '';
    overridePactBuildTool = pkgs.lib.optionalString enablePactBuildTool ''
      # Remove the -build-tool flag from the pact package section, this allows
      # us to build the pact executable through the chainweb project
      sed -i '/package pact/,/^[^\ ]/{/^[ \t]/!b; s/-build-tool//}' $out
    '';
    chainweb = pkgs.haskell-nix.cabalProject' {
      src = haskellSrc;
      compiler-nix-name = compiler;
      cabalProject = builtins.readFile (pkgs.runCommand "cabal.project" {} ''
        cat ${./cabal.project} > $out
        ${overridePact}
        ${overridePactBuildTool}
      '').outPath;
      shell.tools = {
        cabal = {};
        #haskell-language-server = {};
      };
      shell.buildInputs = with pkgs; [
        zlib
        pkg-config
        sqlite
      ];
      modules = [
        {
          packages.http2.doHaddock = false;
        }
      ];
    };
    flake = chainweb.flake {};
    pactFromCached = pkgs: pactInput: cached: {
      version = cached.meta.pact.version;
      src = if pactInput == null then pkgs.fetchgit cached.meta.pact.src else pactInput;
    };
    passthru = {
      version = flake.packages."chainweb-node:exe:chainweb-node".version;
      # cached.meta gets propagated through the recursive outputs
      cached.paths.pactSrc = chainweb.hsPkgs.pact.src;
      cached.meta.pact = {
        version = chainweb.hsPkgs.pact.identifier.version;
        src = if pact != null then {} else with chainweb.hsPkgs.pact.src; {
          url = gitRepoUrl;
          hash = outputHash;
          inherit rev;
        };
      };
      pact = pactFromCached pkgs pact passthru.cached;
    };
    default =
      let
        exesByComponentIndex = [
          {
            name = "chainweb-node";
            exes = ["chainweb-node"];
          }

          {
            name = "cwtools";
            exes = [
              "b64"
              "calculate-release"
              "compact"
              "db-checksum"
              "ea"
              "genconf"
              "header-dump"
              "known-graphs"
              "pact-diff"
              "run-nodes"
              "tx-list"
            ];
          }
        ];

        for = xs: f: builtins.map f xs;

        buildSingle = componentName: exe: ''
          cp ${flake.packages."${componentName}:exe:${exe}"}/bin/${exe} $out/bin/${exe}
          chmod +w $out/bin/${exe}
          $STRIP $out/bin/${exe}
          ${pkgs.lib.optionalString (pkgs.stdenv.isLinux) ''
            patchelf --shrink-rpath $out/bin/${exe}
          ''}
        '';
      in
      pkgs.runCommandCC "chainweb" { inherit passthru; } ''
        mkdir -pv $out/bin
        ${builtins.concatStringsSep "\n" (for exesByComponentIndex (component:
          builtins.concatStringsSep "\n" (for component.exes (exe: buildSingle component.name exe))
        ))}
      '';
in {
  # The Haskell project flake: Used by flake.nix
  inherit flake;

  # The default derivation exported by the nix flake. Used by flake.nix
  inherit default;

  # The source of the Haskell project in the nix store.
  # Useful for debugging, e.g. for nix filters.
  #
  # Example:
  # $ ls $(nix-instantiate default.nix -A haskellSrc --eval)
  inherit haskellSrc;

  inherit pactFromCached;

  # The haskell.nix Haskell project (executables, libraries, etc)
  # Also contains the `flake` attribute, and many useful things.
  #
  # Examples
  #
  # Leverage the `hsPkgs` attribute to inspect the `streaming` Haskell package:
  # $ nix show-derivation $(nix-instantiate -E '(import ./default.nix {}).chainweb.hsPkgs.streaming.components.library')
  #
  # Use `getComponent` to get a cabal component (library/executable/test/benchmark) of a package
  # $ nix show-derivation $(nix-instantiate -E '(import ./default.nix {}).chainweb.getComponent "chainweb:exe:cwtool"')
  # $ nix show-derivation $(nix-instantiate -E '(import ./default.nix {}).chainweb.hsPkgs.semirings.getComponent "lib:semirings"')
  inherit chainweb;

  # The nix package set: Not used by anything, but useful for debugging.
  inherit pkgs;
}
