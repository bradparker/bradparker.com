{ stdenv, glibcLocales, callPackage, runCommand }:
let
  builder = callPackage ../builder {};
  vendor-assets = callPackage ../vendor-assets {};
  timestamp = runCommand "timestamp" {} ''
    date -u '+%a, %d %b %Y %H:%M:%S GMT' -d '@${builtins.toString builtins.currentTime}' > $out
  '';
in
  stdenv.mkDerivation {
    name = "bradparker-com-site";
    src = ./.;
    buildInputs = [ glibcLocales ];
    buildPhase = ''
      export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
      export LANG="en_AU.UTF-8";
      export LC_TYPE="en_AU.UTF-8";

      ${builder}/bin/builder --input $src --input ${vendor-assets} --output dist
      cp ${timestamp} dist/built-at
    '';
    installPhase = ''
      mkdir -p $out
      cp -R dist/* $out/
    '';
  }
