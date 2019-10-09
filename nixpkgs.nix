let
  nixpkgs-source = builtins.fetchTarball {
    url = "https://releases.nixos.org/nixos/19.03/nixos-19.03.173575.0e0ee084d6d/nixexprs.tar.xz";
  };
in
  import nixpkgs-source {}
