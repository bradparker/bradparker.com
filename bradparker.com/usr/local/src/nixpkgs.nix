let
  sources = import ./nix/sources.nix;

  haskellPackagesOverlay = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = (hself: hsuper: {
        pandoc-lens = hself.callCabal2nix "pandoc-lens" sources.pandoc-lens {};
        warp-systemd = hself.callCabal2nix "warp-systemd" sources.warp-systemd {};
      });
    };
  };
in
  import sources.nixpkgs {
    overlays = [haskellPackagesOverlay];
  }
