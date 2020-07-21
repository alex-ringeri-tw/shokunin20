FROM haskell:8.6@sha256:6ae86033bfa75027fc36a63bb69e7fa41d4e7df6ae4a78baeb77dafc94488d81

WORKDIR /opt/shokunin20

RUN cabal v2-update

COPY . /opt/shokunin20
RUN cabal v2-install shokunin20-exe

CMD ["shokunin20-exe"]