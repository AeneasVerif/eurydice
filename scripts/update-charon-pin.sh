#!/usr/bin/env bash
if ! which jq 2> /dev/null 1>&2; then
    echo 'Error: command `jq` not found; please install it.'
    exit 1
fi

CHARON_DIR=./lib/charon/..
CHARON_BRANCH="$(git -C "$CHARON_DIR" rev-parse --abbrev-ref HEAD)"
CHARON_COMMIT="$(git -C "$CHARON_DIR" rev-parse HEAD)"
echo 'Taking the commit from your local charon directory. The charon branch is `'"$CHARON_BRANCH"'`'
nix flake lock --extra-experimental-features nix-command --extra-experimental-features flakes --override-input charon "github:aeneasverif/charon/$CHARON_COMMIT"
