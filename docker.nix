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
                imageDigest = "sha256:a4d41fa0d6bb5b1194189bab4234b1f2abfabb4728bda295f5c53d89766aa046";
                finalImageTag = "3.8";
                sha256 = "c87736221ed0bcaa60b8e92a19bec2284899ef89226f2a07968677cf59e637a4";
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
