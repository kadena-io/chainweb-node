{ rev ? "7e9b0dff974c89e070da1ad85713ff3c20b0ca97"
  , sha256 ? "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36"
  , pkgs ?
  import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      inherit sha256; }) {
          config.allowBroken = false;
          config.allowUnfree = true;
      }
  , skipTests ? true }:

let
    inherit (pkgs.haskell.lib) justStaticExecutables dontCheck;
    chainwebDrv = import ./. {};
    chainwebStatic = justStaticExecutables
                     (if skipTests then dontCheck chainwebDrv else chainwebDrv);
in
    let baseImage = pkgs.dockerTools.buildImage {
            name = "chainweb-base";
            tag = "latest";
            fromImage = pkgs.dockerTools.pullImage {
                imageName = "alpine";
                imageDigest = "sha256:a4d41fa0d6bb5b1194189bab4234b1f2abfabb4728bda295f5c53d89766aa046";
                finalImageTag = "3.8";
                sha256 = "02xr657lzqdydwnxxxpp09h5cc5yww4d4r5z0m2nr6qygshq6qbp";
                os = "linux";
                arch = "amd64";
            };
            contents = [ chainwebStatic ];
            config = {
                Cmd = [ "/bin/sh" ];
                WorkingDir = "/home";
            };
        };
    in
        {
            chainwebBaseImage = baseImage;
            bootstrapNodeImage = pkgs.dockerTools.buildImage {
                name = "chainweb-bootstrap-node";
                tag = "latest";
                fromImage = baseImage;
                config = {
                    Cmd = ["/bin/chainweb-node"
                           "--node-id=0"
                           "--config-file=/tmp/test-bootstrap-node.config"];
                    WorkingDir = "/home";
                };
            };
        }
