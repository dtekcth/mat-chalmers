from haskell:9.10.3 as bin
workdir /app/user
copy *.cabal ./

run cabal v2-update && \
    cabal v2-build --dependencies-only

copy . .

run curl -sLO https://github.com/tailwindlabs/tailwindcss/releases/latest/download/tailwindcss-linux-x64 && \
    mv tailwindcss-linux-x64 tailwindcss && \
    chmod +x tailwindcss && \
    ./tailwindcss --content src/View.hs --output static/style.css --minify

run cabal v2-install --install-method copy --installdir . --overwrite-policy=always


from debian:13.7-slim@sha256:a72b4433de1cf7d6bae0c4cee74d0547f58601a643b3375f070c51600d63b6b2
run apt-get update && apt-get install --no-install-recommends -y ca-certificates \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
copy --from=bin /app/user/mat-chalmers /bin/mat-chalmers
env LANG C.UTF-8
cmd /bin/mat-chalmers
expose 5007
