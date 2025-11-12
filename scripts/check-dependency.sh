#!/usr/bin/env bash
# Eurydice has 3 dependencies that must be handled by hand: karamel, charon, and libcrux.
# For each, we require that `./$PROJECT` contain a clone of the corresponding repository.
# For users/devs of Eurydice only, the repos can be cloned directly there and
# this script will ensure the right commit is checked out. For devs that also
# touch those linked projects, each `./$PROJECT` can be instead a symlink to a
# local clone, in which case having the wrong commit checked out will only be a
# flagged as a warning.
#
# This script must be called like:
#   ./scripts/check-dependency.sh <PROJECT>
# or
#   ./scripts/check-dependency.sh <PROJECT> --force

if [[ "$IN_CI" == "1" ]]; then
    # Don't check commit hashes etc in CI.
    exit 0
fi

PROJECT="$1"
FORCE=
if [[ "$2" == "--force" ]]; then
    FORCE=1
fi

if !which jq 2> /dev/null 1>&2; then
    echo 'Error: `jq` not found. Please install the `jq` command-line utility.'
    exit 1
fi

rebuild() {
    if which nix 2> /dev/null 1>&2; then
      local has_nix=true
    else
      local has_nix=false
    fi
    case "$1" in
        karamel)
            if ! $has_nix; then
              opam install --deps-only .
            fi
            make lib/AutoConfig.ml
        ;;
        charon)
            if $has_nix; then
                # No need to install dependencies via opam if we're within nix
                nix develop --command bash -c "make test"
            elif which rustup 2> /dev/null 1>&2; then
                opam install --deps-only .
                make test
            else
                echo 'Error: Neither `rustup` nor `nix` appears to be installed. Install one or the other in order to build `charon`.'
                exit 1
            fi
            # If we rebuilt charon, most likely the llbc files in eurydice are stale.
            (cd .. && make clean-llbc)
        ;;
        libcrux)
            # Nothing to do
        ;;
    esac
}

PINNED_COMMIT="$(jq -r ".nodes.$PROJECT.locked.rev" flake.lock)"
REPO_URL="$(jq -r '.nodes.'$PROJECT'.original | "https://github.com/\(.owner)/\(.repo)"' flake.lock)"
if [ -e "./$PROJECT" ]; then
    # If ./$PROJECT is a symlink, we assume it's a working copy so we won't enforce commit hashes.
    IS_SYMLINK=
    if [ -L "./$PROJECT" ]; then
        # echo 'Warning: `./'"$PROJECT"'` is a symlink; we assume it is a working copy and will not enforce commit hashes.'
        IS_SYMLINK=1
    fi

    cd "$PROJECT"

    ACTUAL_COMMIT="$(git rev-parse HEAD)"
    if [[ "$ACTUAL_COMMIT" != "$PINNED_COMMIT" ]]; then
        if [[ "$FORCE" == "1" ]]; then
            git fetch origin && git checkout "$PINNED_COMMIT" && rebuild
            exit 0
        elif [[ "$IS_SYMLINK" == "1" ]]; then
            echo 'Warning: `'"$PROJECT"'` commit ('"$ACTUAL_COMMIT"') is not the pinned commit ('"$PINNED_COMMIT"').'
            exit 0
        else
            echo 'Error: `'$PROJECT'` commit is not the pinned commit. Update the '$PROJECT' repository to the commit specified in `flake.lock`:'
            echo '  $ cd '$PROJECT' && git fetch origin && git checkout '"$PINNED_COMMIT"' && make'
            echo 'To do this automatically, run `make setup-'$PROJECT'`.'
            exit 1
        fi
    fi

    if [[ "$FORCE" == "1" ]]; then
        rebuild
        exit 0
    fi
else
    if [[ "$FORCE" == "1" ]]; then
        git clone "$REPO_URL"
        cd "$PROJECT" && git checkout "$PINNED_COMMIT" && rebuild $PROJECT
        exit 0
    else
        echo 'Error: `'$PROJECT'` not found. Please clone the '$PROJECT' repository into `./'$PROJECT'`,'\
            'or make a symlink to an existing clone of '$PROJECT':'
        echo '  $ git clone '$REPO_URL''
        echo '  $ cd '$PROJECT' && git checkout '"$PINNED_COMMIT"' && make'
        echo 'To do this automatically, run `make setup-'$PROJECT'`.'
        exit 1
    fi
fi
