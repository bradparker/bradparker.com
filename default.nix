let
  nixpkgs = import ./nixpkgs.nix;
in
  {
    bradparker-com = {
      builder = nixpkgs.callPackage ./builder {};
      server = nixpkgs.callPackage ./server {};
      site = nixpkgs.callPackage ./site {};
    };
  }
