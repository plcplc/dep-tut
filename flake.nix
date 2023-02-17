{
  description = "Dep Tut";

  inputs = {
    flake-utils = {
      url = github:numtide/flake-utils;
    };

    nixpkgs = {
      # url = github:NixOS/nixpkgs/nixos-22.11;
      url = github:NixOS/nixpkgs/nixos-unstable;
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
          ghcName = "ghc944";
          doDistribute = pkgs.haskell.lib.doDistribute;
          dontCheck = pkgs.haskell.lib.dontCheck;
          jailbreak = pkgs.haskell.lib.doJailBreak;
          hs = pkgs.haskell.packages.${ghcName}.override {
              overrides = hsPkgNew : hsPkgOld : rec {
                # "hspec-contrib" = jailbreak hsPkgOld.hspec-contrib;
                ormolu = doDistribute hsPkgOld.ormolu_0_5_3_0;
                ghcid = dontCheck hsPkgOld.ghcid;

              };
            };
        in
        pkgs.mkShell {
          buildInputs = [
            pkgs.zlib
            pkgs.zlib.dev
            pkgs.stdenv
            pkgs.jq
            pkgs.cabal2nix

            hs.cabal-install
            hs.ghcid
            hs.haskell-language-server
            hs.ormolu
            #pkgs.haskell.compiler.${ghcName}
            #pkgs.haskell.packages.${ghcName}.cabal-install
            #pkgs.haskell.packages.${ghcName}.ghcid
            #pkgs.haskell.packages.${ghcName}.haskell-language-server
            #pkgs.haskell.packages.${ghcName}.ormolu
            # pkgs.haskell.packages.${ghcName}.hlint
            # pkgs.haskell.packages.${ghcName}.hoogle
            #hs.${ghcName}.hspec-discover
          ];
        };
    });
}
