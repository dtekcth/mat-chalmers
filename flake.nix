{
  description = "mat-chalmers goes flakes";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD

  outputs = { self, nixpkgs, flake-utils }:
    let
      ghcVer = "ghc964";
      makeHaskellOverlay = overlay: final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcVer} = prev.haskell.packages."${ghcVer}".override (oldArgs: {
              overrides =
                prev.lib.composeExtensions (oldArgs.overrides or (_: _: { }))
                  (overlay prev);
            });
          };
        };
      };

      out = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
            config.allowBroken = true;
          };

        in
        {
          packages = rec {
            default = mat;
            mat = pkgs.haskell.packages.${ghcVer}.mat;
          };

          checks = {
            inherit (self.packages.${system}) mat;
          };

          # for debugging
          # inherit pkgs;

          devShells.default =
            let haskellPackages = pkgs.haskell.packages.${ghcVer};
            in
            haskellPackages.shellFor {
              packages = p: [ self.packages.${system}.mat ];
              withHoogle = true;
              buildInputs =
                [
                  pkgs.docker
                  pkgs.zlib
                ] ++
                (with haskellPackages; [
                  haskell-language-server
                  cabal-install
                ]);
              # Change the prompt to show that you are in a devShell
              # shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
            };
        };
    in
    flake-utils.lib.eachDefaultSystem out // {
      # this stuff is *not* per-system
      overlays = {
        default = makeHaskellOverlay (prev: hfinal: hprev:
          let hlib = prev.haskell.lib; in
          {
            mat = hprev.callCabal2nix "mat" ./. { };

            # here's how to do hacks to the package set
            # don't run the test suite
            # fast-tags = hlib.dontCheck hprev.fast-tags;
            #
            # don't check version bounds
            # friendly = hlib.doJailbreak hprev.friendly;
          });
      };
    };
}
