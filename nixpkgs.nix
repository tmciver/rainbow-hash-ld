builtins.fetchTarball {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2025-02-04";
  # Commit hash for nixos-unstable as of 2018-09-12
  url = "https://github.com/nixos/nixpkgs/archive/8c50a710ddca43d7a530fb805ad55bde8d0141c5.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "0am8xx09fx5yf2p0wb001v0jx1g5hrfb76h4r37xph378jgk7pcr";
}
