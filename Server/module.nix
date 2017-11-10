{ config, ... }:

let
  port = 8081;
in

{
  networking.firewall.allowedTCPPorts = [ port ];

  systemd.services.cardgame = {
    description = "Card Game Server";
    wantedBy = [ "multi-user.target" ];
    serviceConfig.ExecStart = "${import ./default.nix}/bin/server ${toString port}";
  };
}
