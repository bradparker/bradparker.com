{ modulesPath, config, pkgs, ... }:
{
  imports = [
    "${modulesPath}/virtualisation/digital-ocean-config.nix"
    <bradparker.com/module.nix>
  ];

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
    passwordAuthentication = false;
    challengeResponseAuthentication = false;
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
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCiv/ll8r72qnuI6pWpFkt9NEiMvYD6nEvAZZhEMKqQ3Ad3GRnfG5x1aWmuxdk2+DcR6OaLVa7uZ9IwrDk2loHWneSFQOH7GvMETOou2JMZ/0YIl7e3ATjBzZGaux5makK5eift/TuEsHohK7sP9cnUu/plc+pxPhxlq6yQi61dAFKUPDU8Y/SsFwzzkuIscO7pM9ohr2Ddukju5k94OyCRhsJnVYucODF0p5TnzBejzwS/9xmRfoJmQwPGn60IyvAMWBxL3VJ+uFvNUUj+uUXj68LXEnT7X4NfLlrRC9GC5wT8j5ehgEQ9L3EYsVDV8p4dRE7lEBoS9obWH+dm6s1ngkEGT+J5c9B9foaNPsTwEVyhiXGmRHMGDm8zxhZOVIuOACzGWSjO6Lx3lKDg6v4CnV5UfSJtj17FwhubnIK+cwTD6jB0GoNZplTG4tOsyxVvk787JJ7uZeo8sbaKl5OtgmNF2lJ3XVx+UhnRm3xpPG1XsGQKkFapb1apGQ869hwRzkhpMfM31LO8HQ4MdF2TQ6duR6/jMlhdWmhQAcsWhIW8c8JhwRvQgcLoiZziBheqXqRu5u9yltoMP2vC1d/ofLtWA3yugCUP5QLzt4E4IjJHSEFlWSYugJjpcg/3fm+JuXjIklHtstRvz9G1001TuzSb5rAGQ7dj8eBqyvUKUQ== brad.parker@FGFMAC66.localdomain"
    ];
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services."bradparker.com".enable = true;

  services.do-agent.enable = true;
}
