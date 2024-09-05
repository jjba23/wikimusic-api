.PHONY: all test clean

fmt:
	find . -name '*.hs' -type f -exec ormolu --mode inplace {} \;
	find . -name '*.nix' -exec nixfmt {} \;
	-statix check
	-deadnix -f
# dev: fmt
# 	ghcid --warnings --restart=./wikimusic-api.cabal --test "WikiMusic.Boot.boot"
dev: fmt
	nix run -L .#
run-production:
	nix run -L . -- "./resources/config/run-production.toml"
test:
	nix run -L .#test
push-cache:
	nix path-info --recursive | cachix push wikimusic-api
	nix flake --extra-experimental-features 'nix-command flakes' \
		--accept-flake-config archive --json | jq --raw-output '.path, (.inputs | to_entries [] .value.path)' | cachix push wikimusic-api
push-cache-arm:
	nix --system aarch64-linux path-info --recursive | cachix push wikimusic-api
	nix --system aarch64-linux flake --extra-experimental-features 'nix-command flakes' \
		--accept-flake-config archive --json | jq --raw-output '.path, (.inputs | to_entries [] .value.path)' | cachix push wikimusic-api

grab-db-backup:
	bash ./resources/scripts/grab-db-backup.bash
cabal-release:
	cabal sdist -o .

build:
	nix build -L --extra-experimental-features 'nix-command flakes' --accept-flake-config --print-build-logs --system x86_64-linux
build-arm:
	nix build -L --extra-experimental-features 'nix-command flakes' --accept-flake-config --print-build-logs --system aarch64-linux
