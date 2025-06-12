{ pkgs, ... }:
let compiler = "ghc96";
in {
  packages = with pkgs.haskell.packages."${compiler}"; [
    fourmolu
    cabal-fmt
    implicit-hie
    ghcid
    cabal2nix
    pkgs.ghciwatch
    pkgs.terraform
  ];

  languages.haskell.enable = true;
  env.AWS_PROFILE = "idris";
}
