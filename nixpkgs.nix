let
  nixpkgs-source = builtins.fetchTarball {
    url = https://releases.nixos.org/nixos/19.09/nixos-19.09.1019.c5aabb0d603/nixexprs.tar.xz;
  };
in
  import nixpkgs-source {}
