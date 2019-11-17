let
  nixpkgs = import ./nixpkgs.nix;
in {
  bradparker-com = rec {
    builder = import ./builder;
    server = import ./server;
    site = import ./site;
  };
}
