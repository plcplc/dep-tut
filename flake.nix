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
          ghcName = "ghc966";
          doDistribute = pkgs.haskell.lib.doDistribute;
          dontCheck = pkgs.haskell.lib.dontCheck;
          jailbreak = pkgs.haskell.lib.doJailbreak;
          hs = pkgs.haskell.packages.${ghcName}.override {
              overrides = hsPkgNew : hsPkgOld : rec {
                # "hspec-contrib" = jailbreak hsPkgOld.hspec-contrib;
                # ormolu = doDistribute hsPkgOld.ormolu_0_5_3_0;
                ghcid = dontCheck hsPkgOld.ghcid;
                /*
                tar = jailbreak hsPkgOld.tar;
                ed25519 = jailbreak hsPkgOld.ed25519;
                unliftio-core = jailbreak hsPkgOld.unliftio-core;
                ghc-trace-events = jailbreak hsPkgOld.ghc-trace-events;
                hie-compat = jailbreak hsPkgOld.hie-compat;
                # Actually include tier-2 plugins:
                # see https://github.com/maralorn/nixpkgs/blob/master/pkgs/development/haskell-modules/configuration-ghc-9.4.x.nix
                # and https://haskell-language-server.readthedocs.io/en/latest/support/plugin-support.html#current-plugin-support-tiers
                # haskell-language-server = hsPkgOld.haskell-language-server.override {
                #   hls-refactor-plugin = hsPkgNew.hls-refactor-plugin;
                #   hls-eval-plugin = hsPkgNew.hls-eval-plugin;
                #   hls-ormolu-plugin = hsPkgNew.hls-ormolu-plugin;
                #   # hls-rename-plugin = hsPkgNew.hls-rename-plugin;
                # };
                */
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
