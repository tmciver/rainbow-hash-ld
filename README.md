# Caldron

Caldron is a web application that implementes a **C**ontent-**A**dressed file
store with metadata stored using **L**inked **D**ata.

## Building

Caldron is developed using [Nix](https://nixos.org/) and currently that is the
only supported method of building the application.

To build using Nix, run

    $ nix-build

## Running

Once the application is built, you can run it with

    $ ./result/bin/rainbow-hash-ld \
      --file-store-url URL \
      --sparql-url URL

The URL supplied to `--file-store-url` should be a URL to a
[rainbow-hash](https://github.com/tmciver/rainbow-hash) compatible file store
and the URL supplied to `--sparql-url` shoulde be a URL to a [SPARQL
server](https://www.w3.org/TR/sparql11-protocol/) that also supports the [Graph
Store
Protocol](https://www.w3.org/TR/sparql11-http-rdf-update/). `rainbow-hash-ld`
has only been tested using
[Fuseki](https://jena.apache.org/documentation/fuseki2/).

You can also specify a port with `-p` or `--port`. The default port is 80.

Visit the application at http://localhost:8081.
