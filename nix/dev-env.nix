{ pkgs, haskell, stdenv } :
  let
  compiler = "ghc923";
  hsPkgs = import ./haskellPackages.nix { inherit pkgs compiler;};
  in
  pkgs.mkShell
  {
    buildInputs = [
      (hsPkgs.ghcWithHoogle (h : with h;
      (haskell.lib.getHaskellBuildInputs h.dep-tut)
      ++ [h.haskell-language-server]
      ))
      hsPkgs.ghcid
      hsPkgs.cabal2nix
      pkgs.git
      hsPkgs.cabal-install
    ];

    shellHook = ''
    # For making haddocks visible to HIE:
    export HIE_HOOGLE_DATABASE="$(cat $(which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p')"
    '';
  }
