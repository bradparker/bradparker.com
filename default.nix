let
  nixpkgs = import ./nixpkgs.nix;
  builder = import ./builder;
  server = import ./server;
in
  nixpkgs.stdenv.mkDerivation {
    name = "bradparker-com";
    src = ./.;
    installPhase = ''
      mkdir -p $out/bin

      ${builder}/bin/builder build

      cp -R _site $out/_site
      ln -s ${server}/bin/server $out/bin/server
    '';
  }
