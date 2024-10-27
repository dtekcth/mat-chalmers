from haskell:9.6.6 as bin
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


from debian:12.5-slim
run apt-get update && apt-get install --no-install-recommends -y ca-certificates \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
copy --from=bin /app/user/mat-chalmers /bin/mat-chalmers
env LANG C.UTF-8
cmd /bin/mat-chalmers
expose 5007
