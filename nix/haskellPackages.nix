{ pkgs, compiler } :
let
  haskell = pkgs.haskell;
  lib = haskell.lib;
in haskell.packages."${compiler}".extend (
    self : super :
         {
           dep-tut = self.callCabal2nix "dep-tut" ../. {};
       })
