let
  nixpkgs = import ./nixpkgs.nix;
in
  {
    bradparker-com = {
      builder = nixpkgs.callPackage ./builder {};
      force-https = nixpkgs.callPackage ./force-https {};
      server = nixpkgs.callPackage ./server {};
      site = nixpkgs.callPackage ./site {};
    };
  }
