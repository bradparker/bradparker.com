let
  nixpkgs-source = builtins.fetchTarball {
    url = "https://releases.nixos.org/nixos/18.09/nixos-18.09.1922.97e0d53d669/nixexprs.tar.xz";
    sha256 = "0jl72zcsap4xjh483mjyvhmim45ghklw3pqr8mp0khwvh83422z6";
  };
in
  import nixpkgs-source {}
