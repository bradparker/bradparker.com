{ options, lib, config, pkgs, ... }:
let
  package = import ./.;
  server = package.bradparker-com.server;
  site = package.bradparker-com.site;

  groupName = "bradparker-com";
  serverName = "bradparker.com";
  acmeCredentialsFile = "/etc/${serverName}/acme/environment";

  serviceConfig = config.services."${serverName}";
  options = {
    enable = lib.mkEnableOption "${serverName} service";
  };
in
  {
    options.services.${serverName} = options;

    config = lib.mkIf serviceConfig.enable {
      users.groups.${groupName} = {};

      systemd.sockets."${serverName}-http" = {
        description = "http://${serverName} socket";
        wantedBy = [ "sockets.target" ];
        socketConfig = {
          ListenStream = 80;
        };
      };

      systemd.services."${serverName}-http" = {
        description = ''
          https://${serverName}
        '';
        wants = [ "acme-${serverName}.service" ];
        stopIfChanged = false;
        environment = {
          FORCE_SSL = "true";
          WEB_ROOT = "${site}";
        };
        serviceConfig = {
          ExecStart = ''
            ${server}/bin/server \
              --protocol activate
          '';
        };
      };

      systemd.sockets.${serverName} = {
        description = "https://${serverName} socket";
        wantedBy = [ "sockets.target" ];
        socketConfig = {
          ListenStream = 443;
        };
      };

      systemd.services.${serverName} = {
        description = ''
          https://${serverName}
        '';
        wants = [ "acme-${serverName}.service" ];
        stopIfChanged = false;
        environment = {
          WEB_ROOT = "${site}";
        };
        serviceConfig = {
          ExecStart = ''
            ${server}/bin/server \
              --protocol activate+tls \
              --tlscert /var/lib/acme/${serverName}/fullchain.pem \
              --tlskey /var/lib/acme/${serverName}/key.pem \
              --graceful serve-normally
          '';
        };
      };

      systemd.services."acme-environment-file" = {
        wantedBy = [ "acme-${serverName}.service" ];
        before = [ "acme-${serverName}.service" ];
        script = ''
          chown :${groupName} ${acmeCredentialsFile}
        '';
        description = ''
          Makes the ACME credentials file readable by the ACME service
        '';
        serviceConfig = {
          Type = "oneshot";
        };
      };

      security.acme = {
        acceptTerms = true;
        certs = {
          ${serverName} = {
            dnsProvider = "digitalocean";
            group = groupName;
            credentialsFile = acmeCredentialsFile;

            email = "hi@bradparker.com";
            extraDomainNames = [ "bradparker.com.au" ];
            keyType = "rsa4096";

            postRun = "systemctl restart ${serverName}.service";
          };
        };
      };
    };
  }
