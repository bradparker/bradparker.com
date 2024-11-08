let
  nixpkgs = import ../bradparker.com/usr/local/src/bradparker.com/nixpkgs.nix;
in
  nixpkgs.mkShell {
    buildInputs = with nixpkgs; [
      doctl
    ];
  }
