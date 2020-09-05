{ config, pkgs, ... }:
let
  bradparker-source = builtins.fetchTarball {
    url = https://github.com/bradparker/bradparker.com/archive/edeacebe3a67656cee4ffb6242fce23109ea7db8.tar.gz;
  };
in
{
  imports = [
    "${bradparker-source}/module.nix"
  ];

  nix.gc = {
    automatic = true;
    dates = "00:00";
  };

  environment.systemPackages = with pkgs; [
    curl
    vim
  ];

  services.openssh = {
    passwordAuthentication = false;
    permitRootLogin = "no";
  };

  users.mutableUsers = false;

  users.users.brad = {
    isNormalUser = true;
    hashedPassword = "$1$jabaNaR7$zvNAZ9gGyeR/Ma.CUMsvP1";
    home = "/home/brad";
    description = "Brad Parker";
    extraGroups = [
      "wheel"
    ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDNb/W1Q8Bw4cUdgDKrQ3H8GEHQHMUvBBZ8qfMwuetR1a5CFXM0MBJtX+20L/P6VEf2hkkFiK3g6aWFCOqhMFFxtBvHeqhZOSMnDKkfsxD9WeRs2spy9djInO6008U8zOb4y0+R6+us5fcNYcsl1SlC/sL2iF4tnwexLAN9edY2jmZjOI8i2WYMG09yMZ/TXUZISe6R+MlYbWDhh0YNJxSZAhwNPK73p7vvgyvzBA35mo44XrneDVhF1L1hBHsEINKmHepI/BwapJng/55H0f4d0zf4T2JM8Ne6Ooq+c5zdwXTbPw/n9KuTGtpnUEPgSGxRaMWcjqcXqYrnC8xS+Bq0bxMd2tZMB1A5AwaRnhXqXCQOYOTUYd1R4j9l21XduQCV2bvPuOfpb52E01/KY2US0PbXAtg4fWtMKAtRpUnQXC+opYyKT0Wcc0AWbZqbakwGAGUQwk2LjSLsthzzjh+3p0DlvYGv+erEt+Oih2q7IfIaNxxQiT4fQwnChcmmD03L2YRDSF3V9Kayh+gWDTgHEqMw29UuuXDAqQipElYDyb5QU5re2D11x6+ECcoNt7UvqS0uabXsIu5fzHQsFGEExyfwskpnakpFdNtT5l7FrwIHEbm2upu1CO56amgu8laNt6/2Vm8h22RoNo4JlLbUXwFpH9whmwd8EgOUUigusw== brad@brad"
    ];
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services."bradparker.com".enable = true;

  services.do-agent.enable = true;
}
