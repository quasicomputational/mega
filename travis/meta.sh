#!/bin/bash

set -euxo pipefail

cabal v2-run --project-file=cabal/"$PROJECT".project meta -- check-hash

mkdir -p "$HOME"/.local/bin

HPACKVER=0.31.2

curl -L https://github.com/sol/hpack/releases/download/"$HPACKVER"/hpack_linux.gz \
  | gunzip \
  > $HOME/.local/bin/hpack

chmod +x "$HOME"/.local/bin/hpack

# Run hpack on packages, and see if we're left with a clean working tree.
# TODO: when https://github.com/sol/hpack/pull/346 is merged/released, we can do that instead of asking git!
find packages/ -type f -path 'packages/*/package.yaml' -exec "$HOME"/.local/bin/hpack \{\} \;

git diff-index --exit-code HEAD
