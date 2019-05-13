#!/bin/bash

set -euxo pipefail

# Regenerate .travis.yml and the project files, then check that none of them are different to what's in HEAD, and that there aren't any untracked files.
cabal v2-run --project-file=cabal/"$PROJECT".project meta -- gen-travis

test -z "$(git status --porcelain)"

cabal v2-run --project-file=cabal/"$PROJECT".project meta -- check-defrost

mkdir -p "$HOME"/.local/bin

HPACKVER=0.31.2

curl -L https://github.com/sol/hpack/releases/download/"$HPACKVER"/hpack_linux.gz \
  | gunzip \
  > $HOME/.local/bin/hpack

chmod +x "$HOME"/.local/bin/hpack

# Run hpack on packages, and see if we're left with a clean working tree. Note that we have to use xargs because find swallows return codes and we want to detect things that make hpack fail (e.g., hand-modification of .cabal files, bad YAML). No need to check for untracked files here - if we had a missing .cabal file, the build wouldn't even get down here!
# TODO: when https://github.com/sol/hpack/pull/346 is merged/released, we can do that instead of asking git! Still have to use xargs, though.
find packages/ -type f -path 'packages/*/package.yaml' -print0 \
  | xargs -0 -n1 hpack

git diff-index --exit-code HEAD
