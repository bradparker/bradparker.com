{ haskellPackages }:
  haskellPackages.callCabal2nix "acme" ./. {}
