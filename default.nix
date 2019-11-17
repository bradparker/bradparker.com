let
  nixpkgs = import ./nixpkgs.nix;
in {
  bradparker-com = rec {
    builder = import ./builder;
    server = import ./server;
    site = nixpkgs.stdenv.mkDerivation {
      name = "bradparker-com-site";
      src = ./.;
      buildInputs = [ nixpkgs.glibcLocales ];
      buildPhase = ''
        export LOCALE_ARCHIVE="${nixpkgs.glibcLocales}/lib/locale/locale-archive"
        export LANG="en_AU.UTF-8";
        export LC_TYPE="en_AU.UTF-8";

        ${builder}/bin/builder build
      '';
      installPhase = ''
        mkdir -p $out/var/www/bradparker.com/
        cp -R _site/* $out/var/www/bradparker.com/
      '';
    };
  };
}
