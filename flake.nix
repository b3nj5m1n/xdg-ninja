{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system:
      let
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
        packages = flake-utils.lib.flattenTree rec {
          default = xdg-ninja;
          # The shell script and configurations, uses derivation from offical nixpkgs
          xdg-ninja = pkgs.stdenvNoCC.mkDerivation {
            pname = "xdg-ninja";
            version = "0.1.0";

            src = ./.;

            nativeBuildInputs = with pkgs; [ makeWrapper ];

            installPhase = ''
              runHook preInstall

              DESTDIR="$out" PREFIX= make install

              wrapProgram "$out/bin/xdg-ninja" \
                --prefix PATH : "${pkgs.lib.makeBinPath (with pkgs; [ glow jq findutils ])}"

              runHook postInstall
            '';
          };
          # Pre-built binary of xdgnj tool downloaded from github
          xdgnj-bin = pkgs.stdenvNoCC.mkDerivation {
            name = "xdgnj-bin";
            version = "0.2.0.1-alpha";
            src = pkgs.fetchurl {
              url = "https://github.com/b3nj5m1n/xdg-ninja/releases/download/v0.2.0.1/xdgnj";
              sha256 = "y1BSqKQWbhCyg2sRgMsv8ivmylSUJj6XZ8o+/2oT5ns=";
            };
            dontUnpack = true;
            installPhase = ''
              mkdir -p "$out/bin"
              install -Dm755 $src "$out/bin/xdgnj"
            '';
            meta.description = "Pre-built binary of the xdgnj tool for creating and editing configuration files for xdg-ninja.";
          };
        };
        apps = rec {
          default = xdg-ninja;
          xdg-ninja = flake-utils.lib.mkApp { drv = packages.xdg-ninja; exePath = "/bin/xdg-ninja"; };
          xdgnj-bin = flake-utils.lib.mkApp { drv = packages.xdgnj-bin; exePath = "/bin/xdgnj"; };
        };
      }
    );
}
