{ config, lib, pkgs, ...}: with lib;
let cfg = config.services.chainweb-miner;
    chainweb-miner = import ./.;
in
{
  options = {
    services.chainweb-miner = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Chainweb-miner, our dedicated Mining Client.
          Work requests are sent to known Nodes, which construct blocks for the client.
          Once solved, the client returns the block to the Node for submission to the wider network.
          This client boasts much higher mining performance and a much simpler setup,
          since a chainweb-miner has no database connections and thus no storage footprint.
        '';
      };
      nodeHostname = mkOption {
        type = types.string;
        description = ''
          Host name for the trusted node https://github.com/kadena-io/chainweb-node/wiki
        '';
      };
      nodePort = mkOption {
        type = types.port;
        default = 443;
        description = ''
          Port number for the trused node https://github.com/kadena-io/chainweb-node/wiki
        '';
      };
      account = mkOption {
        type = types.string;
        description = ''
          Your miner account. This must be unowned, or already claimed by you!
        '';
      };
      chain = mkOption {
        type = types.nullOr types.int;
        default = null;
        description = ''
          Chain in focus https://github.com/kadena-io/chainweb-node/blob/master/miner/README.org#chain-focusing
        '';

      };
      key = mkOption {
        type = types.string;
        description = ''
          Your miner public key. https://github.com/kadena-io/chainweb-node/blob/master/miner/README.org#obtaining-a-key-pair
        '';
      };
    };
  };

  config = {

    systemd.services.chainweb-miner = mkIf cfg.enable {

      serviceConfig = {
        ExecStart = pkgs.writeShellScriptBin "chainweb-miner.sh" ''
          ${chainweb-miner}/bin/chainweb-miner cpu --node=${cfg.nodeHostname}:${toString cfg.nodePort} --miner-account=${cfg.account} --miner-key=${cfg.key} ${if cfg.chain == null then "" else "--chain=${cfg.chain}"}
        '' + "/bin/chainweb-miner.sh";
      };

      wantedBy = [ "multi-user.target" ];

      after = [ "network.target" ];

    };
  };

}
