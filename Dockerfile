FROM haskell:8.10 AS build
RUN mkdir -p /app/user
WORKDIR /app/user
COPY stack.yaml *.cabal ./

RUN export PATH=$(stack path --local-bin):$PATH
RUN stack build --dependencies-only

COPY . /app/user
RUN stack install

FROM ubuntu:latest AS exec
ENV LANG C.UTF-8
CMD /root/.local/bin/mat-chalmers
EXPOSE 5007
