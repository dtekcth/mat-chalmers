{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, css-text, file-embed
      , http-conduit, lens, lucid, mtl, old-locale, scotty, stdenv
      , tagsoup, text, thyme, wai-extra, wai-middleware-static-embedded
      }:
      mkDerivation {
        pname = "mat-chalmers";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        enableSeparateDataOutput = true;
        libraryHaskellDepends = [
          aeson base css-text file-embed http-conduit lens lucid old-locale
          tagsoup text thyme
        ];
        executableHaskellDepends = [
          base bytestring file-embed lens mtl scotty wai-extra
          wai-middleware-static-embedded
        ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
