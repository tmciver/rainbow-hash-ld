let
  pkgs = import (import ./nixpkgs.nix) { config = { allowBroken = true; }; };
in
pkgs.haskell.lib.justStaticExecutables (import ./default.nix { inherit pkgs; })
