# This expression just exposes the final derivation of our package.
# Sometimes this file would be called 'release.nix'.
{pkgs ? import <nixpkgs> {}, compiler} :
  let
    hsPkgs = import ./haskellPackages.nix { inherit pkgs compiler; }; 
  in
  hsPkgs.dep-tut
