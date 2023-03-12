{
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        runtimeDependencies = with pkgs; [
          glow
          jq
        ];
        overlays = [
          (self: super: {
            xdg-ninja = super.xdg-ninja.overrideAttrs (old: {
              version = "git";
              src = ./.;
            });
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; };
      in rec {
        packages = flake-utils.lib.flattenTree {
          xdg-ninja = pkgs.xdg-ninja;
        };
        defaultPackage = packages.xdg-ninja;
        apps = {
          xdg-ninja = flake-utils.lib.mkApp { drv = packages.xdg-ninja; };
        };
        defaultApp = apps.xdg-ninja;
      }
    );
}
