let
  pkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-unstable-2025-02-04";
    # Commit hash for nixos-unstable as of 2018-09-12
    url = "https://github.com/nixos/nixpkgs/archive/32f313e49e42f715491e1ea7b306a87c16fe0388.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1z4ga87qla5300qwib3dnjnkaywwh8y1qqsb8w2mrsrw78k9xmlw";
  }) { config = { allowBroken = true; }; };
in
pkgs.haskellPackages.developPackage {
  root = ./.;

  source-overrides = {
    rainbow-hash = pkgs.fetchFromGitHub {
      owner = "tmciver";
      repo = "rainbow-hash";
      rev = "6b5dd642332f71256a80fa2abca222272c204d32";
      hash = "sha256-b+n4HIj+RdCB8FlfGn8cNogNMoXbCE81D8my7kuyeY0=";
    };
    crypton-pem = pkgs.fetchFromGitHub {
      owner = "mpilgrem";
      repo = "crypton-pem";
      rev = "b025588fda5cd64b0cd74989f646ee3bab34395e";
      hash = "sha256-1aSKiS7AR+MLQhJVyTIT49/owMgZGkkX74K55tZeGo0=";
    };
  };

  overrides = final: prev:
    let
      inherit (pkgs.haskell.lib) doJailbreak;

      hsparqlSrc = pkgs.fetchFromGitHub {
        owner = "tmciver";
        repo = "hsparql";
        rev = "4c65cc1069c1fe296a23d189c6cf10d27a9ec817";
        hash = "sha256-EXgv74dtCaKLBZTIP5gFFgQIvwRWgEj2mdyqSgm0Wl8=";
      };
    in {
      rdf4h = doJailbreak prev.rdf4h;
      hsparql = final.callCabal2nix "hsparql" hsparqlSrc {};
    };

  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
      [cabal-install
       ghcid
       hasktags
       stylish-haskell
       hpack
       pkgs.aider-chat
      ]);
}
