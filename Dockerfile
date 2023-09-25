FROM haskell:9.4.5
RUN mkdir -p /app/user
WORKDIR /app/user
COPY *.cabal ./

RUN cabal v2-update
RUN cabal v2-build --dependencies-only

COPY . /app/user
RUN cabal v2-install

ENV LANG C.UTF-8
CMD /root/.local/bin/mat-chalmers
EXPOSE 5007
