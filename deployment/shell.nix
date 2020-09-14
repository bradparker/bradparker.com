let
  nixpkgs = import ../nixpkgs.nix;
in
  nixpkgs.mkShell {
    buildInputs = with nixpkgs; [
      doctl
    ];
  }
