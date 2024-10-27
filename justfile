default: up

up:
	tailwindcss --content src/View.hs --output static/style.css --minify
	cabal clean
	cabal run
