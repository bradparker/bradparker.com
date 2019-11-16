{ options, lib, config, pkgs, ... }:
let
  server = import ./server;
  builder = import ./builder;
  site = import ./.;
  acme = import ./acme;

  serverName = "bradparker.com";
  siteRoot = "/var/www/${serverName}/public";
  siteRepo = "/var/www/${serverName}/source";

  serviceConfig = config.services."${serverName}";
  options = {
    enable = lib.mkEnableOption "${serverName} service";
    port = lib.mkOption {
      type = lib.types.port;
      description = "Port number.";
      default = 80;
    };
    https = lib.mkOption {
      type = lib.types.submodule {
        options = {
          enable = lib.mkEnableOption "HTTPS";
          acmeWebRoot = lib.mkOption {
            type = lib.types.str;
            description = "Directory for the acme challenge which is PUBLIC, don't put certs or keys in here";
            default = "/var/lib/acme/acme-challenge";
          };
        };
      };
      description = "HTTPS config";
      default = { enable = false; };
    };
  };

  args = with serviceConfig; [
    "--port ${toString port}"
    "--directory ${site}"
  ] ++ lib.optionals https.enable [
    "--https-cert-file /var/lib/acme/${serverName}/fullchain.pem"
    "--https-key-file /var/lib/acme/${serverName}/key.pem"
  ];
in
  {
    options.services.${serverName} = options;
    config = lib.mkIf serviceConfig.enable {
      systemd.services.${serverName} = {
        wantedBy = [ "multi-user.target" ];
        wants = ["source-${serverName}.timer"] ++ lib.optionals serviceConfig.https.enable [
          "acme-${serverName}.service"
          "acme-selfsigned-${serverName}.service"
          "acme-challenge-${serverName}.service"
        ];
        script = ''
          ${server}/bin/server ${lib.concatStringsSep " " args}
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
        startAt = "5 min";
        script = ''
          export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
          export LANG="en_AU.UTF-8";
          export LC_TYPE="en_AU.UTF-8";

          cd ${siteRepo}

          ${pkgs.git}/bin/git clone .

          ${builder}/bin/builder build

          cp -R _site/* ${siteRoot}
        '';
      };

      systemd.services."acme-challenge-${serverName}" = {
        wantedBy = [ "multi-user.target" ];
        script = ''
          ${acme}/bin/acme \
            --port 80 \
            --directory ${serviceConfig.https.acmeWebRoot}
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
        ${serverName} = lib.mkIf serviceConfig.https.enable {
          email = "hi@bradparker.com";
          webroot = "${serviceConfig.https.acmeWebRoot}";
          extraDomains = { "bradparker.com.au" = null; };
          postRun = "systemctl restart ${serverName}.service";
        };
      };
    };
  }
