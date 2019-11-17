let
  nixpkgs = import ./nixpkgs.nix;
in
  {
    bradparker-com = {
      acme = nixpkgs.callPackage ./acme {};
      builder = nixpkgs.callPackage ./builder {};
      server = nixpkgs.callPackage ./server {};
      site = nixpkgs.callPackage ./site {};
    };
  }
