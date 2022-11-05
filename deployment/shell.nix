let
  nixpkgs = import ../bradparker.com/usr/local/src/nixpkgs.nix;
in
  nixpkgs.mkShell {
    buildInputs = with nixpkgs; [
      doctl
    ];
  }
