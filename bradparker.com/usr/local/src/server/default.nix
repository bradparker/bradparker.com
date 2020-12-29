{ haskellPackages }:
  haskellPackages.callCabal2nix "server" ./. {}
