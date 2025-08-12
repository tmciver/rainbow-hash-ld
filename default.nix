let
  pkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-unstable-2025-02-04";
    # Commit hash for nixos-unstable as of 2018-09-12
    url = "https://github.com/nixos/nixpkgs/archive/fecfeb86328381268e29e998ddd3ebc70bbd7f7c.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0m52nb9p4q468pgi1657dzcpsrxd1f15flxljaplxzjyiwbrzz5f";
  }) { config = { allowBroken = true; }; };
in
pkgs.haskellPackages.developPackage {
  root = ./.;

  source-overrides = {
    rainbow-hash = pkgs.fetchFromGitHub {
      owner = "tmciver";
      repo = "rainbow-hash";
      rev = "b6928339020e147a8075e07c979cabf270513de9";
      hash = "sha256-nlUOOCSHB7ttB4vsvtJMRiMq8ac4opm+L98VCXvNUH4=";
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
        rev = "cca28da32a9da6fb0c3109d2601cfc3e43172c7c";
        hash = "sha256-mlNq3UtbxEXanLprhG7UE5678z4qudbdHbQYfW/84AI=";
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
      ]);
}
