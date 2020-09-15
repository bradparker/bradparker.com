{ options, lib, config, pkgs, ... }:
let
  package = import ./.;
  acme = package.bradparker-com.acme;
  server = package.bradparker-com.server;

  groupName = "bradparker-com";
  serverName = "bradparker.com";
  webRoot = "/var/www/${serverName}";
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

      systemd.services."source-${serverName}" = {
        description = ''
          https://${serverName} source
        '';
        serviceConfig = {
          Type = "oneshot";
        };
        startAt = "*:0/5";
        path = with pkgs; [ nix gnutar xz gzip curl jq ];
        script = ''
          set -ex

          rev=$(curl https://api.github.com/repos/bradparker/bradparker.com/git/ref/heads/main | jq -r .object.sha)
          result=$(nix-build https://github.com/bradparker/bradparker.com/archive/$rev.tar.gz -A bradparker-com.site)

          mkdir -p /var/www

          ln -snfT $result${webRoot} ${webRoot}
        '';
      };

      systemd.services.${serverName} = {
        wantedBy = [ "multi-user.target" ];
        wants = [ "acme-${serverName}.service" ];
        requires = ["source-${serverName}.service"];
        script = ''
          ${server}/bin/server \
            --port 443 \
            --site-directory ${webRoot} \
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
            extraDomains = { "bradparker.com.au" = null; };
            keyType = "rsa4096";

            postRun = "systemctl restart ${serverName}.service";
          };
        };
      };
    };
  }
