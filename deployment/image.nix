{ nixpkgs ? <nixpkgs> }:
let
  configuration = {
    imports = [
      "${nixpkgs}/nixos/modules/virtualisation/digital-ocean-image.nix"
    ];
    networking.hostName = "nixos";
    services = {
      sshd.enable = true;
      cloud-init.enable = true;
    };
  };
  nixos = import "${nixpkgs}/nixos" {
    inherit configuration;
    system = "x86_64-linux";
  };
in
  nixos.config.system.build.digitalOceanImage
