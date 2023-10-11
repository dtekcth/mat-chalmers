# mat-chalmers

[![Build Status](https://travis-ci.org/dtekcth/mat-chalmers.svg?branch=master)](https://travis-ci.org/dtekcth/mat-chalmers)

Lunch menus on Chalmers in Gothenburg. Issues and pull requests welcome.

## Hacking

```
git clone https://github.com/dtekcth/mat-chalmers.git
cd mat-chalmers
cabal build         # (or cabal run)
```

If you're using [NixOS] (or just Nix package manager), use the
following instructions to get a development environment running:

```
nix develop         # Download dependencies and start a dev-shell with everything setup
```

Or, to build everything:

```
nix build           # (or nix run)
```

Pro-tip, to get shorter build times, consider using [Cachix] and use
the `jassob` cache:

```
cachix use jassob   # Only needed once
```

## Credits

Favicon made by Freepik from <a href="http://www.flaticon.com"
title="Flaticon">www.flaticon.com</a> is licensed by <a
href="http://creativecommons.org/licenses/by/3.0/" title="Creative
Commons BY 3.0">CC BY 3.0</a>

[nixos]: https://nixos.org/
[cachix]: https://docs.cachix.org/installation
