FROM haskell:9.4.5 as BIN
WORKDIR /app/user
COPY *.cabal ./

RUN cabal v2-update
RUN cabal v2-build --dependencies-only

COPY . .
RUN cabal v2-install --install-method copy --installdir . --overwrite-policy=always

FROM debian:12.5-slim
RUN apt update
RUN apt install -y ca-certificates
COPY --from=BIN /app/user/mat-chalmers /bin/mat-chalmers
ENV LANG C.UTF-8
CMD /bin/mat-chalmers
EXPOSE 5007
