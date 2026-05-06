let
  pkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-unstable-2025-02-04";
    # Commit hash for nixos-unstable as of 2018-09-12
    url = "https://github.com/nixos/nixpkgs/archive/32f313e49e42f715491e1ea7b306a87c16fe0388.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1z4ga87qla5300qwib3dnjnkaywwh8y1qqsb8w2mrsrw78k9xmlw";
  }) { config = { allowBroken = true; }; };

  # Import the Caldron application from default.nix
  caldron = import ./default.nix;

  # Create startup script
  startScript = pkgs.writeScript "start-caldron" ''
    #!${pkgs.bash}/bin/bash
    set -e
    
    # Start the Caldron server
    exec ${caldron}/bin/caldron-server --file-store-url FILE_STORE_URL --sparql-url SPARQL_URL"
  '';

in pkgs.dockerTools.buildImage {
  name = "caldron";
  tag = "latest";
  
  copyToRoot = with pkgs; [
    # Include the Haskell application
    caldron
    # Basic utilities
    bash
    coreutils
    # CA certificates for HTTPS
    cacert
    # For debugging (optional)
    curl
  ];

  config = {
    Cmd = [ "${startScript}" ];
    ExposedPorts = {
      "8081/tcp" = {};
    };
    WorkingDir = "/app";
    Env = [
      "PATH=${pkgs.bash}/bin:${pkgs.coreutils}/bin:${caldron}/bin"
      "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
    ];
    
  };
}
