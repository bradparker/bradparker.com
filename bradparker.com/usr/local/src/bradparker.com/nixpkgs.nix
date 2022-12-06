let
  sources = import ./nix/sources.nix;

  haskellPackagesOverlay = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = (hself: hsuper: {
        pandoc-lens = super.haskell.lib.dontHaddock (hself.callCabal2nix "pandoc-lens" sources.pandoc-lens {});
      });
    };
  };
in
  import sources.nixpkgs {
    overlays = [haskellPackagesOverlay];
  }
