{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    let
      system = flake-utils.lib.system.x86_64-linux;
      pkgs = nixpkgs.legacyPackages.${system};
      compiler = "ghc96";
      overlays = [ (final: prev: { inherit chronobid; }) ];
      chronobid =
        pkgs.haskell.packages.${compiler}.callCabal2nix "" ./chronobid { };
    in {
      packages.${system}.default = chronobid;
      nixosConfigurations.bitnomial = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [ ./configuration.nix { nixpkgs.overlays = overlays; } ];
      };
    };
}
