let
  # pkgs = import <nixpkgs> { config = { allowBroken = true; }; }; # pin the channel to ensure reproducibility!
  # pkgs = import (builtins.fetchGit {
  #   # Descriptive name to make the store path easier to identify
  #   name = "nixos-24.11-2025-02-04";
  #   url = "https://github.com/nixos/nixpkgs/";
  #   # Commit hash for nixos-unstable as of 2018-09-12
  #   # `git ls-remote https://github.com/nixos/nixpkgs nixos-unstable`
  #   ref = "refs/heads/nixos-24.11";
  #   rev = "fecfeb86328381268e29e998ddd3ebc70bbd7f7c";
  # }) {};
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
  # overrides = final: prev: let
  #   inherit (pkgs.haskell.lib) overrideCabal;
  # in {
  #   rdf4h = overrideCabal prev.rdf4h {
  #     version = "5.0.1";
  #     sha256 = "sha256-tmh5XWACl+TIp/1VoQe5gnssUsC8FMXqDWXiDmaRxmw=";
  #   };
  # };
  overrides = final: prev:
    let
      inherit (pkgs.haskell.lib) overrideCabal doJailbreak;

      hsparqlSrc = pkgs.fetchFromGitHub {
        owner = "tmciver";
        repo = "hsparql";
        rev = "cca28da32a9da6fb0c3109d2601cfc3e43172c7c";
        hash = "sha256-mlNq3UtbxEXanLprhG7UE5678z4qudbdHbQYfW/84AI=";
      };
      rainbowHashSrc = pkgs.fetchFromGitHub {
        owner = "tmciver";
        repo = "rainbow-hash";
        rev = "b6928339020e147a8075e07c979cabf270513de9";
        hash = "sha256-nlUOOCSHB7ttB4vsvtJMRiMq8ac4opm+L98VCXvNUH4=";
      };
    in {
      # tls = overrideCabal prev.tls {
      #   version = "1.6.0";
      #   sha256 = "sha256-0p0gr8HBuFKjw5sHbshTy1lqyIjPUh5UFERB3saJ5Jg=";
      # };
      # connection = doJailbreak prev.connection;
      rdf4h = doJailbreak prev.rdf4h;
      hsparql = pkgs.haskellPackages.callCabal2nix "hsparql" hsparqlSrc {};
      rainbow-hash = import "${rainbowHashSrc}/default.nix" {};
    };
      
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
      [ ghc
        cabal-install
        ghcid
        haskell-language-server
        hasktags
      ]);
}
