{ modulesPath, config, pkgs, ... }:
{
  imports = [
    "${modulesPath}/virtualisation/digital-ocean-config.nix"
    ../../usr/local/src/bradparker.com/module.nix
  ];

  system.stateVersion = "24.11";

  nix.settings.trusted-users = ["root" "@wheel"];
  nix.gc = {
    automatic = true;
    dates = "00:00";
  };

  environment.systemPackages = with pkgs; [
    rsync
    curl
    vim
  ];

  services.openssh = {
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  users.mutableUsers = false;

  users.users.brad = {
    isNormalUser = true;
    hashedPassword = "$y$j9T$7m6t1HHPS/FDMcd.rp6Sp1$EAbu7mLyNkQDqMLvtQqoN0Id/tjjfLW7i71vMXPV784";
    home = "/home/brad";
    description = "Brad Parker";
    extraGroups = [
      "wheel"
    ];
    openssh.authorizedKeys.keys = [
      # Starbook
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOYaoaPFRbC3cUYqpWmKErINBXl7c6/l1LFz0r7QlCct hi@bradparker.com"
    ];
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services."bradparker.com".enable = true;

  services.do-agent.enable = true;
}
