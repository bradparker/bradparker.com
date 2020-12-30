let
  nixpkgs = import ./nixpkgs.nix;
in
  nixpkgs.haskellPackages.shellFor {
    packages = _: [
      (nixpkgs.callPackage ./builder {})
      (nixpkgs.callPackage ./server {})
    ];
    nativeBuildInputs = with nixpkgs; [
      cabal-install
      entr
      overmind
    ];
    shellHook = ''
      export LOCALE_ARCHIVE="${nixpkgs.glibcLocales}/lib/locale/locale-archive"
      export LANG="en_AU.UTF-8";
      export LC_TYPE="en_AU.UTF-8";

      export VENDOR_ASSETS=${nixpkgs.callPackage ./vendor-assets {}}
    '';
  }
