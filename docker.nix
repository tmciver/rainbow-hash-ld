let
  pkgs = import (import ./nixpkgs.nix) { config = { allowBroken = true; }; };
  caldron = pkgs.haskell.lib.justStaticExecutables
    (import ./default.nix { inherit pkgs; });
in
pkgs.dockerTools.buildLayeredImage {
  name = "com.timmciver/caldron";
  tag = "latest";
  contents = [ caldron pkgs.cacert pkgs.busybox ];
  config = {
    Cmd = [ "/bin/caldron-server" ];
    ExposedPorts = {
      "8081/tcp" = {};
    };
    Env = [
      "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
    ];
  };
}
