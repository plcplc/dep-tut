/* Run with 'nix-shell -p "import ./shell.nix"' */
/* This version is redundant as it just uses <nixpkgs>: You might as well just
   'nix-shell -p "import ./dev-env.nix"'. */
(import <nixpkgs> {}).callPackage ./dev-env.nix {}
