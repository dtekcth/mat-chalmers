{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, css-text, errors
      , exceptions, heredoc, http-client, http-client-tls
      , logging-effect, lucid, microlens-platform, mtl, old-locale
      , prettyprinter, safe, scotty, stdenv, text, thyme, time, wai-extra
      }:
      mkDerivation {
        pname = "mat-chalmers";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        buildTools = [ haskellPackages.cabal-install ];
        enableSeparateDataOutput = true;
        libraryHaskellDepends = [
          aeson base bytestring css-text errors exceptions heredoc
          http-client logging-effect lucid microlens-platform mtl old-locale
          prettyprinter safe text thyme
        ];
        executableHaskellDepends = [
          base bytestring http-client-tls logging-effect
          microlens-platform mtl scotty time wai-extra
        ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
