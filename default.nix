let
  nixpkgs = import ./nixpkgs.nix;
in
  {
    bradparker-com = {
      force-https = nixpkgs.callPackage ./force-https {};
      builder = nixpkgs.callPackage ./builder {};
      server = nixpkgs.callPackage ./server {};
      site = nixpkgs.callPackage ./site {};
    };
  }
