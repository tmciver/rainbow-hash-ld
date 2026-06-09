builtins.fetchTarball {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2025-02-04";
  # Commit hash for nixos-unstable as of 2018-09-12
  url = "https://github.com/nixos/nixpkgs/archive/32f313e49e42f715491e1ea7b306a87c16fe0388.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "1z4ga87qla5300qwib3dnjnkaywwh8y1qqsb8w2mrsrw78k9xmlw";
}
