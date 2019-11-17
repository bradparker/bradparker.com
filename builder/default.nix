{ haskellPackages }:
  haskellPackages.callCabal2nix "builder" ./. {}
