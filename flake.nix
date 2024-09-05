{
  description = "WikiMusic API flake";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            wikimusicApiProject = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc96";
              shell.tools = {
                cabal = { };
                hlint = { };
                haskell-language-server = { };
              };
              shell.buildInputs = with pkgs; [
                gnumake
                simple-http-server
                minify
                haskell-language-server
                watchexec
                cachix
                ormolu
                nixfmt
                ghcid
                statix
                deadnix
                jq
                awscli2
              ];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.wikimusicApiProject.flake { };
      in flake // {
        legacyPackages = pkgs;
        packages.default = flake.packages."wikimusic-api:exe:wikimusic-api-exe";
        packages.test = flake.packages."wikimusic-api:test:spec";
      });

}
