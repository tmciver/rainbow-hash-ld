# Caldron

Caldron is a web application that implementes a **C**ontent-**A**dressed file
store with metadata stored using **L**inked **D**ata.

## Building

Caldron is developed using [Nix](https://nixos.org/) and currently that is the
only supported method of building the application.

To build the application, run

    $ make build

To build and load the Docker image, run

    $ make docker

## Configuration

### Server

The server is configured via environment variables set in `docker-compose.yml`.
The following table describes the available options.

|Description|Environment Variable Name|Required|Default Value|
|-----------|-------------------------|--------|-------------|
|SPARQL URL - a URL to a SPARQL server|`SPARQL_URL`|Yes|`http://fuseki:3030/ds`|
|Blob Store URL - a URL to a rainbow-hash-compatible file store|`FILE_STORE_URL`|Yes|`http://rainbow-hash:3000/blobs`|
|Hostname - The hostname to use for server-generated URLs; overrides the `HOST` header|`PREFERRED_HOST`|No|Value of `HOST` header|

The default `docker-compose.yml` pre-configures `SPARQL_URL` and `FILE_STORE_URL`
to point at the bundled Fuseki and rainbow-hash services respectively. Edit those
values if you are using external services.

### SSL Certificates

The nginx proxy service expects the server certificate and key to be present in
the project root:

    caldron.timmciver.com.crt
    caldron.timmciver.com.key

Place your certificate and key files there before starting the stack.

### CLI Tool

The `caldron` CLI app operates using the following sub-commands:

* `upload <FILE-OR-DIR> <options>`
* `watch <DIR> <options>`

They both take the same options which can be configured via command line
arguments and/or a configuration file. The following table gives information
about the configuration data:

|Description|Config File Field Name|Command Line Argument Name|Required|Default Value|
|-----------|----------------------|--------------------------|--------|-------------|
|Server URL |`sparql-url`|`--server-uri`|Yes|N/A|
|Path to Certificate and Key PEM file|`file-store-url`|`--pem-path`|Yes|N/A|
|Whether to delete the file after upload|`delete-uploaded-file`|`--delete-after-upload`, `-d`|No|false|

## Running

### Prerequisites

The application has a dependency on the rainbow-hash project. View the README
there for instructions on creating the required Docker image (it is not yet on
Docker Hub).

### Running with Docker Compose

Start the stack with:

    $ docker compose up -d

To stop the stack:

    $ docker compose down

Logs can be viewed with:

    $ docker compose logs -f

Visit https://localhost to use the application.

## Notes

### Authentication

Caldron authenticates the client using the [WebID-TLS
protocol](https://www.w3.org/2005/Incubator/webid/spec/tls/). You'll need to
either have or create this certificate.

#### WebID-TLS Client Certificate

The client certificate used with WebID-TLS is a normal self-signed client
certificate with one exception: the user's WebID must be added as a Subject
Alternate Name (URL) to their certificate.

Also, and this is very important: the WebID URL must point to a profile document
that is available on the web and which includes data about the client
certificate needed by the server for authentication.  See the WebID-TLS spec for
more details.

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

### On-Behalf-of

The Caldron CLI tool, `caldron`, can be used to upload files and directories of
files. You may want to use this tool as a long-running process to watch a
directory and upload files as they are added to this directory. If these files
are coming from different users and you'd like to track which user was
responsible for the file, the WebID of the uploading agent cannot be used for
this. The on-behalf-of feature allows recording of who the uploading agent is
uploading the file _on behalf of_.

For this to work the file being uploaded must be owned by the user on behalf of
whom the agent is doing the uploading. In the `caldron` configuration file add
an entry to the `user-email-map` field with the form:

    <file-owner-name>: <user-email-address>

The configured email address is sent to the server when uploading the file (via
the `From` HTTP header) and the server then maps the email address to a WebID
via it's own configuration.
