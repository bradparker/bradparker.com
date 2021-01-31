let
  sources = import ./nix/sources.nix;

  haskellPackagesOverlay = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = (hself: hsuper: {
        pandoc-lens = hself.callCabal2nix "pandoc-lens" sources.pandoc-lens {};
        wai-cli = hself.callCabal2nix "wai-cli" sources.wai-cli {};
        socket-activation = hself.callCabal2nix "socket-activation" sources.haskell-socket-activation {};
      });
    };
  };
in
  import sources.nixpkgs {
    overlays = [haskellPackagesOverlay];
  }
