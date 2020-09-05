let
  nixpkgs-source = builtins.fetchTarball {
    url = https://releases.nixos.org/nixpkgs/nixpkgs-20.09pre242076.fd457ecb6cc/nixexprs.tar.xz;
  };
in
  import nixpkgs-source {}
