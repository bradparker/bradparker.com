{ haskellPackages }:
  haskellPackages.callCabal2nix "force-https" ./. {}
