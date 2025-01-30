#!/usr/bin/env bash
CHARON_DIR=./lib/charon
CHARON_BRANCH="$(git -C "$CHARON_DIR" rev-parse --abbrev-ref HEAD)"
CHARON_COMMIT="$(git -C "$CHARON_DIR" rev-parse HEAD)"
echo 'Taking the commit from your local charon directory. The charon branch is `'"$CHARON_BRANCH"'`'
nix flake lock --extra-experimental-features nix-command --extra-experimental-features flakes --override-input charon "github:aeneasverif/charon/$CHARON_COMMIT"
