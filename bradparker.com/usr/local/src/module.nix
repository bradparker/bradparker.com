{ options, lib, config, pkgs, ... }:
let
  package = import ./.;
  force-https = package.bradparker-com.force-https;
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

      systemd.services."force-https-${serverName}" = {
        wantedBy = [ "multi-user.target" ];
        script = ''
          ${force-https}/bin/force-https --port 80
        '';
        description = ''
          Redirects to https://${serverName}
        '';
        serviceConfig = {
          KillSignal="INT";
          Type = "simple";
          Restart = "on-abort";
          RestartSec = "10";
        };
      };

      systemd.services.${serverName} = {
        wantedBy = [ "multi-user.target" ];
        wants = [ "acme-${serverName}.service" ];
        script = ''
          ${server}/bin/server \
            --port 443 \
            --site-directory ${site} \
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
