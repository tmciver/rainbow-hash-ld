{ pkgs ? import (import ./nixpkgs.nix) { config = { allowBroken = true; }; } }:
pkgs.haskellPackages.developPackage {
  root = ./.;

  source-overrides = {
    rainbow-hash = pkgs.fetchFromGitHub {
      owner = "tmciver";
      repo = "rainbow-hash";
      rev = "7fa2f433066d237c9ee2a949d5743fd8d774aa3e";
      hash = "sha256-0YmUAKH/2n5+NVpkueqBrMfo46Pj3/0gGmbGhIGYqjA=";
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
       pkgs.pylode
      ]);
}
