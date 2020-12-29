{ haskellPackages, haskell }:
  haskell.lib.appendConfigureFlags
    (haskellPackages.callCabal2nix "builder" ./. {})
    ["--enable-executable-dynamic"]
