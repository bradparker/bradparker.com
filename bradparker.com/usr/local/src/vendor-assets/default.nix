{ stdenv }:
let
  sources = import ../nix/sources.nix;
  fira = sources.Fira;
  tachyons = sources.tachyons;
  minimal-css = sources."minimal.css";
in
  stdenv.mkDerivation {
    name = "bradparker-com-fonts";
    version = "1.0.0";
    src = ./.;
    installPhase = ''
      mkdir -p $out/assets/fonts/fira

      cp ${fira}/eot/FiraMono-Regular.eot $out/assets/fonts/fira
      cp ${fira}/woff/FiraMono-Regular.woff $out/assets/fonts/fira
      cp ${fira}/woff2/FiraMono-Regular.woff2 $out/assets/fonts/fira
      cp ${fira}/ttf/FiraMono-Regular.ttf $out/assets/fonts/fira

      mkdir -p $out/assets/fonts/{tiempos,founders-grotesk}
      cp $src/proprietary/tiempos/* $out/assets/fonts/tiempos
      cp $src/proprietary/founders-grotesk/* $out/assets/fonts/founders-grotesk

      mkdir -p $out/assets/stylesheets
      cp ${tachyons}/css/tachyons.min.css $out/assets/stylesheets/
      cp ${minimal-css}/minimal.css $out/assets/stylesheets/
    '';
  }
