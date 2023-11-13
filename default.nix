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
, compiler ? "ghc963"
, flakePath ? flakeDefaultNix.outPath
, nix-filter ? inputs.nix-filter
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
    chainweb = pkgs.haskell-nix.project' {
      src = haskellSrc;
      compiler-nix-name = compiler;
      projectFileName = "cabal.project";
      shell.tools = {
        cabal = {};
        haskell-language-server = {};
      };
      shell.buildInputs = with pkgs; [
        zlib
        pkgconfig
      ];
      modules = [
        {
          packages.http2.doHaddock = false;
        }
      ];
    };
    flake = chainweb.flake {};
    passthru = {
      version = flake.packages."chainweb:exe:chainweb-node".version;
      # cachedMeta gets propagated through the recursive outputs
      cachedMeta = {
        pact = {
          version = chainweb.hsPkgs.pact.identifier.version;
          src = chainweb.hsPkgs.pact.src;
        };
      };
    };
    default = pkgs.runCommandCC "chainweb" { inherit passthru; } ''
      mkdir -pv $out/bin
      cp ${flake.packages."chainweb:exe:chainweb-node"}/bin/chainweb-node $out/bin/chainweb-node
      cp ${flake.packages."chainweb:exe:cwtool"}/bin/cwtool $out/bin/cwtool
      chmod +w $out/bin/{cwtool,chainweb-node}
      $STRIP $out/bin/chainweb-node
      $STRIP $out/bin/cwtool
      ${pkgs.lib.optionalString (pkgs.stdenv.isLinux) ''
        patchelf --shrink-rpath $out/bin/{cwtool,chainweb-node}
      ''}
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
