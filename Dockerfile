FROM haskell:9.4.5 as DEPS
RUN mkdir -p /app/user
WORKDIR /app/user
COPY *.cabal ./

RUN cabal v2-update
RUN cabal v2-build --dependencies-only

FROM haskell:9.4.5 as BIN
COPY --from=DEPS /app/user /app/user
COPY --from=DEPS /root/.cabal /root/.cabal
COPY . /app/user
WORKDIR /app/user
RUN cabal v2-install --install-method copy --installdir . --overwrite-policy=always

FROM debian:12.5-slim
RUN apt update
RUN apt install -y ca-certificates
COPY --from=BIN /app/user/mat-chalmers /app/user/mat-chalmers
ENV LANG C.UTF-8
CMD /app/user/mat-chalmers
EXPOSE 5007
