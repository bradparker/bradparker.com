let
  nixpkgs-source = builtins.fetchTarball {
    url = https://releases.nixos.org/nixos/unstable/nixos-19.09pre186563.b5f5c97f7d6/nixexprs.tar.xz;
  };

  nixpkgs = import nixpkgs-source {};
in
  nixpkgs.mkShell {
    buildInputs = [nixpkgs.haskellPackages.patat];
  }
