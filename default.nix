let
  nixpkgs = import ./nixpkgs.nix;
  builder = import ./builder;
  bradparker-com = nixpkgs.stdenv.mkDerivation {
    name = "bradparker-com";
    src = ./.;
    buildInputs = [ nixpkgs.glibcLocales ];
    buildPhase = ''
      export LOCALE_ARCHIVE="${nixpkgs.glibcLocales}/lib/locale/locale-archive"
      export LANG="en_AU.UTF-8";
      export LC_TYPE="en_AU.UTF-8";

      ${builder}/bin/builder build
    '';
    installPhase = ''
      mkdir -p $out
      cp -R _site/* $out/var/www/bradparker.com/
    '';
  };
in
  { bradparker-com = bradparker-com; }
