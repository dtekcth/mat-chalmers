{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, stdenv, haskellPackages
      , wai-extra, wai-middleware-static-embedded
      }:
      mkDerivation {
        pname = "mat-chalmers";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        buildTools = [ haskellPackages.cabal-install ];
        enableSeparateDataOutput = true;
        libraryHaskellDepends = with haskellPackages; [
          aeson base bytestring css-text file-embed http-conduit lucid
          old-locale microlens-platform old-locale tagsoup text thyme
        ];
        executableHaskellDepends = with haskellPackages; [
          base bytestring file-embed lens mtl scotty wai-extra
          wai-middleware-static-embedded
        ];
        license = stdenv.lib.licenses.mit;
      };



  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  wai-middleware-static-embedded = pkgs.callPackage ./wai-middleware-static-embedded;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
