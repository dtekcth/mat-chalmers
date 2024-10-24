# mat-chalmers

[![Build Status](https://travis-ci.org/dtekcth/mat-chalmers.svg?branch=master)](https://travis-ci.org/dtekcth/mat-chalmers)

Lunch menus on Chalmers in Gothenburg. Issues and pull requests welcome.

## Hacking

```
git clone https://github.com/dtekcth/mat-chalmers.git
cd mat-chalmers
cabal update
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
the `jassob` or `pingu` cache:

```
cachix use jassob   # Only needed once
cachix use pingu    # Probably more up to date as of 5/6-24
```

### Updating the view?
Currently, the css framework used is [TailwindCSS](https://tailwindcss.com).
To generate the css file, visit the website, wait until menus have loaded, and then save the html to `Lunch at Chalmers.html`.
Then use [Tailwinds cli](https://tailwindcss.com/blog/standalone-cli) program to generate the new css by running `tailwindcss --content Lunch\ at\ Chalmers.html --output static/style.css --minify`.

Worth noting is that the css is embedded in the binary, and changes to it aren't well captured by cabal, so `cabal clean && cabal run` or `docker compose up --build` might be needed to see the updated styling.

## Credits

Favicon made by Freepik from <a href="http://www.flaticon.com"
title="Flaticon">www.flaticon.com</a> is licensed by <a
href="http://creativecommons.org/licenses/by/3.0/" title="Creative
Commons BY 3.0">CC BY 3.0</a>

[nixos]: https://nixos.org/
[cachix]: https://docs.cachix.org/installation
