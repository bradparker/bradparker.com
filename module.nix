{ options, lib, config, ... }:
let
  server = import ./server;
  site = import ./.;

  serviceConfig = config.services.bradparker-com;
  options = {
    enable = lib.mkEnableOption "bradparker.com service";
    port = lib.mkOption {
      type = lib.types.port;
      description = "Port number.";
      default = 80;
    };
    https = lib.mkOption {
      type = lib.types.nullOr (lib.types.submodule {
        options = {
          certFile = lib.mkOption {
            type = lib.types.string;
            description = "Absolute path to certfile";
            default = "";
          };
          keyFile = lib.mkOption {
            type = lib.types.string;
            description = "Absolute path to keyfile";
            default = "";
          };
        };
      });
      description = "Config required for HTTPS";
      default = null;
    };
  };

  args = with serviceConfig; [
    "--port ${toString port}"
    "--directory ${site}"
  ] ++ lib.optionals (https != null) [
    "--keyFile ${https.keyFile}"
    "--certFile ${https.certFile}"
  ];
in
  {
    options.services.bradparker-com = options;
    config = lib.mkIf serviceConfig.enable {
      systemd.services.bradparker-com = {
        wantedBy = [ "multi-user.target" ];
        script = ''
          ${server}/bin/server ${lib.concatStringsSep " " args}
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
