_:
let
  acmeWebRoot = "/var/www/acme";
  acmeKeyDir = "/var/lib/acme";
in
  {
    imports = [ ../module.nix ];

    security.acme.certs = {
      "bradparker.com" = {
        email = "hi@bradparker.com";
        webroot = "${acmeWebRoot}";
        postRun = "systemctl reload bradparker-com.service";
      };
    };

    services = {
      bradparker-com = {
        enable = true;
        port = 443;
        https = {
          keyFile = "${acmeKeyDir}/bradpaker.com/key.pem";
          certFile = "${acmeKeyDir}/bradparker.com/fullchain.pem";
        };
      };
    };
  }
