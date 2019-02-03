let
  nixpkgs = import ./nixpkgs.nix;
in
  nixpkgs.haskellPackages.callCabal2nix "bradparker-com" ./. {}
