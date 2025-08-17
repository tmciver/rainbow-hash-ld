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

    $ ./result/bin/rainbow-hash-ld
    
Todo: rename the project and application.

There are several bits of hard-coded configuration data (these will be made
configurable in a future release):

* Port: 8081
* Blob storage URL to `rainbow-hash`: http://localhost:3000/blobs
* SPARQL endpoint URL: http://localhost:3030/ds

Visit the application at http://localhost:8081.
