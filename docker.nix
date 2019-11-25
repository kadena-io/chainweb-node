{ nixpkgs ? (import ./project.nix { system = "x86_64-linux"; }).rp.nixpkgs, skipTests ? true }:

let
    inherit (nixpkgs) pkgs;
    inherit (nixpkgs.haskell.lib) justStaticExecutables dontCheck;
    chainwebDrv = ( import ./. { system = "x86_64-linux"; } );
    chainwebStatic = justStaticExecutables chainwebDrv;
in
    let baseImage = pkgs.dockerTools.buildImage {
            name = "chainweb-base";
            tag = "latest";
            fromImage = pkgs.dockerTools.pullImage {
                imageName = "alpine";
                imageDigest = "sha256:a4d41fa0d6bb5b1194189bab4234b1f2abfabb4728bda295f5c53d89766aa046";
                finalImageTag = "3.8";
                sha256 = "17s0np13ygsc16ahx2zzyry60c03p48cq3skqvlwm6bhfshkwvv8";
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
