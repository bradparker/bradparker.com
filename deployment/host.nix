{ config, pkgs, ... }:
let
  bradparker-source = builtins.fetchTarball {
    url = https://github.com/bradparker/bradparker.github.io/archive/9988711e5d25886b570c68c94847f1881244692a.tar.gz;
  };
in
{
  imports = ["${bradparker-source}/module.nix"];

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
    hashedPassword = "$1$cmckqoXC$eBya/upETQKFbInZPz5y8.";
    home = "/home/brad";
    description = "Brad Parker";
    extraGroups = [
      "wheel"
    ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQD8pPHsHkSNeX+YTVfbmrMltnWs+6dejWClomo2jBQuSv93WAzChWA1lhh8rUo9yxlud4FSV2iJ+3qQ6lpxgt8Dd9rS+xa8SS1rga/nTG5eqJqfq1fDyKXF+TpbHbNO09QdWZjRTcvv1DNCd51FyWUsFojECjoY0KmyjxJCmVyKgdvWjW/9DHiP0a1MS1ILtREr873D5SiQlKKj8T1AaVs7tToZNtZWxE2U4L4ibWYiWoJnVMi85t0nGj2NNBsVJsYhAb5fh5Uj5La9R3nR7RrkCLCHOKzuZu6VkWIQcObFCgb5G80DDt9Vp8uiNDCMwMLZA2HPi4CpGjWEfgHbvYEbgRNZIQcJhid12HscjEChLN4uvdp+9TiQwFsTm3kSBgflERFOdX4qbJMy/XQz8FEgUb2E8/lo3P3DMUeYhNXNGO6jmBvknNBv6XXjhhOFBa9xmT1jnjakSpKBWvICIWqLFmi8MByldqTW7oJKNpDH4qI4ML0goPaZ9ketlGs8PUwr1d4bCR/Dm79VTuiiUNw+i0J4sQrs2ufkEtuBQfBmHywVhl/sDS/3A7vUZWZ+2Cb+9qssAGjEy424anWRdkFx/x+m+9Zreb2tkXDlofEd18fCNbqAZzVTYAoDlQUmen1IJOEhWPpC724p3wyv8Y2SN1pMunPQ7ThmuPEB/hOSEQ== hi@bradparker.com"
    ];
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services."bradparker.com".enable = true;
}
