let
  pkgs = import (builtins.fetchTarball {
    name = "nixos-unstable-2025-02-04";
    url = "https://github.com/nixos/nixpkgs/archive/32f313e49e42f715491e1ea7b306a87c16fe0388.tar.gz";
    sha256 = "1z4ga87qla5300qwib3dnjnkaywwh8y1qqsb8w2mrsrw78k9xmlw";
  }) { config = { allowBroken = true; }; };

  # Define package overrides
  haskellPackages = pkgs.haskellPackages.override {
    overrides = final: prev:
      let
        inherit (pkgs.haskell.lib) doJailbreak;

        hsparqlSrc = pkgs.fetchFromGitHub {
          owner = "tmciver";
          repo = "hsparql";
          rev = "4c65cc1069c1fe296a23d189c6cf10d27a9ec817";
          hash = "sha256-EXgv74dtCaKLBZTIP5gFFgQIvwRWgEj2mdyqSgm0Wl8=";
        };

        rainbow-hash = final.callCabal2nix "rainbow-hash" (pkgs.fetchFromGitHub {
          owner = "tmciver";
          repo = "rainbow-hash";
          rev = "7fa2f433066d237c9ee2a949d5743fd8d774aa3e";
          hash = "sha256-0YmUAKH/2n5+NVpkueqBrMfo46Pj3/0gGmbGhIGYqjA=";
        }) {};

        crypton-pem = final.callCabal2nix "crypton-pem" (pkgs.fetchFromGitHub {
          owner = "mpilgrem";
          repo = "crypton-pem";
          rev = "b025588fda5cd64b0cd74989f646ee3bab34395e";
          hash = "sha256-1aSKiS7AR+MLQhJVyTIT49/owMgZGkkX74K55tZeGo0=";
        }) {};

      in {
        inherit rainbow-hash crypton-pem;
        rdf4h = doJailbreak prev.rdf4h;
        hsparql = final.callCabal2nix "hsparql" hsparqlSrc {};
      };
  };

in
haskellPackages.callCabal2nix "caldron" ./. {}