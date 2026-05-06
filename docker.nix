let
  pkgs = import (builtins.fetchTarball {
    name = "nixos-unstable-2025-02-04";
    url = "https://github.com/nixos/nixpkgs/archive/32f313e49e42f715491e1ea7b306a87c16fe0388.tar.gz";
    sha256 = "1z4ga87qla5300qwib3dnjnkaywwh8y1qqsb8w2mrsrw78k9xmlw";
  }) { config = { allowBroken = true; }; };

  # Import the Caldron application (production build without dev tools)
  caldron = import ./production.nix;

in pkgs.dockerTools.buildLayeredImage {
  name = "caldron";
  tag = "latest";
  maxLayers = 120;
  
  contents = [
    caldron
    pkgs.cacert
  ];

  config = {
    Cmd = [ "${caldron}/bin/caldron-server" "--file-store-url" "FILE_STORE_URL" "--sparql-url" "SPARQL_URL" ];
    ExposedPorts = {
      "8081/tcp" = {};
    };
    Env = [
      "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
    ];
    User = "1000:1000";
  };
}
