let
  nixpkgs = import ./nixpkgs.nix;
  builder = import ./builder;
in
  nixpkgs.stdenv.mkDerivation {
    name = "bradparker-com";
    src = ./.;
    buildPhase = ''
      ${builder}/bin/builder build
    '';
    installPhase = ''
      mkdir -p $out
      cp -R _site/* $out/
    '';
  }
