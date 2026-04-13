# Caldron

Caldron is a web application that implementes a **C**ontent-**A**dressed file
store with metadata stored using **L**inked **D**ata.

## Building

Caldron is developed using [Nix](https://nixos.org/) and currently that is the
only supported method of building the application.

To build using Nix, run

    $ nix-build

## Running

Note that currently this application cannot be used standalone; it must be
behind a proxy. See the note about authentication below.

Once the application is built, you can run it with

    $ ./result/bin/rainbow-hash-ld \
      --file-store-url URL \
      --sparql-url URL

The URL supplied to `--file-store-url` should be a URL to a
[rainbow-hash](https://github.com/tmciver/rainbow-hash) compatible file store
and the URL supplied to `--sparql-url` shoulde be a URL to a [SPARQL
server](https://www.w3.org/TR/sparql11-protocol/) that also supports the [Graph
Store Protocol](https://www.w3.org/TR/sparql11-http-rdf-update/). Caldron has
only been tested using [Fuseki](https://jena.apache.org/documentation/fuseki2/).

You can also specify a port with `-p` or `--port`. The default port is 80.

### A Note about Authentication

Caldron authenticates the client using the [WebID-TLS
protocol](https://www.w3.org/2005/Incubator/webid/spec/tls/). But instead of
using a TLS-Light service described in the spec, Caldron uses a hacky version
that uses a proxy with two different virtual hosts configured both of which use
https. One virtual host is configured normally - this is the entrypoint to the
app - the other is configured to request a client certificate.

Once the proxy validates the client certficate, the certificate is passed on to
the downstream server (this application) in the `X-SSL-CERT` header. If you
visit the application URL without this proxy in place, you will receive an error
with the message

    Missing X-SSL-CERT header

There are plans to [create a proper TLS-Light
service](https://github.com/tmciver/rainbow-hash-ld/issues/28) at some point in
the future.

#### WebID-TLS Client Certificate

The client certificate used with WebID-TLS is a normal self-signed client
certificate with one exception: the user's WebID must be added as a Subject
Alternate Name (URL) to their certificate.

This repository has a facility to assist in the creation of a WebID-TLS client
certificate as a set of Makefile targets.

The first step is to set up the configuration file. There is a sample
configuration file that can be used as a base. First move into the `cert`
directory:

    $ cd cert

Next, run the following `make` command to create the configuration file:

    $ make config

Then, edit the new configuration file which defaults to `client.conf`. Update
the section `req_distinguished_name` with your own data. Enter your WebID as
the value for `URI.1` in the `alt_names` section. Once done, run

    $ make pkcs12

This will create the file `client.p12` which you can load into your browser's
certificate store.
