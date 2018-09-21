## Dockerfile for a haskell environment
FROM ubuntu

RUN apt-get update && \
    apt-get -y install ocaml && \
    apt-get -y install ocamlbuild && \
    mkdir /ocaml

WORKDIR /ocaml

CMD ["bash"]