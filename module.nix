{ options, lib, config, ... }:
let
  serviceConfig = config.services.bradparker-com;
  server = import ./server;
  site = import ./.;
in
  {
    options.services.bradparker-com.enable = lib.mkEnableOption "Enable the bradparker.com service";
    config = lib.mkIf serviceConfig.enable {
      systemd.services.bradparker-com = {
        wantedBy = [ "multi-user.target" ];
        script = ''
          ${server}/bin/server --port 8080 --directory ${site}
        '';
        description = ''
          https://bradparker.com
        '';
        serviceConfig = {
          KillSignal="INT";
          Type = "simple";
          Restart = "on-abort";
          RestartSec = "10";
        };
      };
    };
  }
