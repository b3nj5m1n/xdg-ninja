{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        packages = flake-utils.lib.flattenTree rec {
          xdg-ninja = pkgs.stdenvNoCC.mkDerivation {
            name = "xdg-ninja";
            src = ./.;
            nativeBuildInputs = [pkgs.makeWrapper];
            installPhase = ''
              runHook preInstall
              install -Dm755 xdg-ninja.sh "$out/share/xdg-ninja/xdg-ninja.sh"
              install -Dm644 programs/* -t "$out/share/xdg-ninja/programs"
              mkdir -p "$out/bin"
              ln -s "$out/share/xdg-ninja/xdg-ninja.sh" "$out/bin/xdg-ninja"
              wrapProgram "$out/bin/xdg-ninja" --prefix PATH : "${pkgs.lib.makeBinPath [pkgs.glow pkgs.jq]}"
              runHook postInstall
            '';
          };
          default = xdg-ninja;
        };
        defaultApp = packages.default;
      }
    );
}
