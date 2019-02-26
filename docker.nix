{ nixpkgs ? import <nixpkgs> { system = "x86_64-linux"; }, skipTests ? true }:

let
    inherit (nixpkgs) pkgs;
    inherit (nixpkgs.haskell.lib) justStaticExecutables dontCheck;
    chainwebDrv = ( import ./. { system = "x86_64-linux"; } ).ghc.chainweb;
    chainwebStatic = justStaticExecutables 
                     (if skipTests then dontCheck chainwebDrv else chainwebDrv);
in
    let baseImage = pkgs.dockerTools.buildImage {
            name = "chainweb-base";
            tag = "latest";
            fromImage = pkgs.dockerTools.pullImage {
                imageName = "alpine";
                imageDigest = "sha256:e1871801d30885a610511c867de0d6baca7ed4e6a2573d506bbec7fd3b03873f";
                sha256 = "05wcg38vsygjzf59cspfbb7cq98c7x18kz2yym6rbdgx960a0kyq";
            };
            contents = [ chainwebStatic ];
            config = {
                Cmd = [ "/bin/sh" ];
                WorkingDir = "/home";
            };
        };
    in
        {
            bootstrapTestImage = pkgs.dockerTools.buildImage {
                name = "chainweb-bootstrap-test";
                tag = "latest";
                fromImage = baseImage;
                config = {
                    Cmd = ["chainweb-node --node-id=0 --config-file=/tmp/test-bootstrap-node.config"];
                };
            };
        }
