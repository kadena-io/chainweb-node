args@{ pkgs ? null }:
import ./default.nix (args // { returnShellEnv = true; })
