let
  sources = import ./nix/sources.nix;

  haskellPackagesOverlay = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = (hself: hsuper: {
        pandoc-lens = super.haskell.lib.dontHaddock (hself.callCabal2nix "pandoc-lens" sources.pandoc-lens {});
        wai-cli = super.haskell.lib.dontHaddock (hself.callCabal2nix "wai-cli" sources.wai-cli {});
        socket-activation = super.haskell.lib.dontHaddock (hself.callCabal2nix "socket-activation" sources.haskell-socket-activation {});
      });
    };
  };
in
  import sources.nixpkgs {
    overlays = [haskellPackagesOverlay];
  }
