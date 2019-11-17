{ options, lib, config, pkgs, ... }:
let
  package = import ./.;
  acme = package.acme;
  server = package.server;

  serverName = "bradparker.com";
  webRoot = "/var/www/${serverName}";
  acmeWebRoot =  "/var/lib/acme/acme-challenge";

  serviceConfig = config.services."${serverName}";
  options = {
    enable = lib.mkEnableOption "${serverName} service";
  };
in
  {
    options.services.${serverName} = options;
    config = lib.mkIf serviceConfig.enable {
      systemd.services.${serverName} = {
        wantedBy = [ "multi-user.target" ];
        wants = [
          "acme-${serverName}.service"
          "acme-selfsigned-${serverName}.service"
          "acme-challenge-${serverName}.service"
        ];
        requires = ["source-${serverName}.service"];
        script = ''
          ${server}/bin/server \
            --port 443 \
            --directory /var/www/${serverName} \
            --https-cert-file /var/lib/acme/${serverName}/fullchain.pem \
            --https-key-file /var/lib/acme/${serverName}/key.pem
        '';
        description = ''
          https://${serverName}
        '';
        serviceConfig = {
          KillSignal="INT";
          Type = "simple";
          Restart = "on-abort";
          RestartSec = "10";
        };
      };

      systemd.services."source-${serverName}" = {
        description = ''
          https://${serverName} source
        '';
        serviceConfig = {
          Type = "oneshot";
        };
        startAt = "*:0/5";
        path = with pkgs; [ nix gnutar gzip ];
        script = ''
          set -ex

          result=$(nix-build https://github.com/bradparker/bradparker.com/archive/source.tar.gz -A bradparker-com.site)

          ln -sfT $result${webRoot} ${webRoot}
        '';
      };

      systemd.services."acme-challenge-${serverName}" = {
        wantedBy = [ "multi-user.target" ];
        script = ''
          ${acme}/bin/acme \
            --port 80 \
            --directory ${acmeWebRoot}
        '';
        description = ''
          The acme challenge server
        '';
        serviceConfig = {
          KillSignal="INT";
          Type = "simple";
          Restart = "on-abort";
          RestartSec = "10";
        };
      };

      security.acme.certs = {
        ${serverName} = {
          email = "hi@bradparker.com";
          webroot = "${acmeWebRoot}";
          extraDomains = { "bradparker.com.au" = null; };
          postRun = "systemctl restart ${serverName}.service";
        };
      };
    };
  }
