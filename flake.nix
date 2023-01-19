{
  description = "Dep Tut";

  inputs = {
    flake-utils = {
      url = github:numtide/flake-utils;
    };

    nixpkgs = {
      url = github:NixOS/nixpkgs/nixos-22.11;
    };
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:
    flake-utils.lib.eachDefaultSystem (system:
    {
      devShells.default =
        let
          pkgs = import nixpkgs { inherit system; };
          ghcName = "ghc943";
          jailbreak = pkgs.haskell.lib.doJailBreak;
          #hs = pkgs.haskellPackages.override {
          #    overrides = hsPkgNew : hsPkgOld : rec {
          #      "hspec-contrib" = jailbreak hsPkgOld.hspec-contrib;
          #    };
          #  };
        in
        pkgs.mkShell {
          buildInputs = [
            pkgs.zlib
            pkgs.zlib.dev
            pkgs.stdenv
            pkgs.jq
            pkgs.cabal2nix

            pkgs.haskell.compiler.${ghcName}
            pkgs.haskell.packages.${ghcName}.cabal-install
            #hs.${ghcName}.ghcid
            pkgs.haskell.packages.${ghcName}.haskell-language-server
            # pkgs.haskell.packages.${ghcName}.hlint
            # pkgs.haskell.packages.${ghcName}.hoogle
            #hs.${ghcName}.hspec-discover
            #pkgs.haskell.packages.${ghcName}.ormolu
          ];
        };
    });
}
