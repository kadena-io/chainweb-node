{ nixpkgs ? import <nixpkgs> { system = "x86_64-linux"; }, 
  chainwebSrc ? import ../chainweb-node/project.nix { },
  skipTests ? true }:

let
    inherit (nixpkgs) pkgs;
    inherit (nixpkgs.haskell.lib) justStaticExecutables dontCheck doStrip;
    chainwebDrv = chainwebSrc.ghc.chainweb;
    chainwebStatic = doStrip (justStaticExecutables
                     (if skipTests then dontCheck chainwebDrv else chainwebDrv));
in
  let testnetConfig = pkgs.copyPathToStore config/defaultTestnetNode.yaml;
      mainnetConfig = pkgs.copyPathToStore config/defaultMainnetNode.yaml;
      baseImage = pkgs.dockerTools.buildImage {
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
        extraCommands = ''
          #!/bin/sh

          mkdir ./config
          cp ${testnetConfig} ./config/testnet.yaml
          cp ${mainnetConfig} ./config/mainnet.yaml
        '';
      };
      testnetVersionName = "testnet02";
      mainnetVersionName = "mainnet01";
      chainwebNodePort = "10000";

    in
        {   chainwebBaseImage = baseImage;
            chainwebNodeImage = pkgs.dockerTools.buildImage {
                name = "chainweb-node";
                tag = "latest";
                fromImage = baseImage;
                config = {
                  Entrypoint = ["/bin/chainweb-node"];
                    Cmd = ["--config-file" "/config/mainnet.yaml"];
                    WorkingDir = "/home";
                    ExposedPorts = {
                      "${chainwebNodePort}/tcp" = {};
                    };
                };
            };
            chainwebMinerImage = pkgs.dockerTools.buildImage {
                name = "chainweb-miner";
                tag = "latest";
                fromImage = baseImage;
                config = {
                  Entrypoint = ["/bin/chainweb-miner"];
                    Cmd = ["--help"];
                    WorkingDir = "/home";
                    ExposedPorts = {
                      "${chainwebNodePort}/tcp" = {};
                    };
                };
            };
            chainweb = chainwebStatic;
        }

