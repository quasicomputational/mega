#!/bin/bash

set -euxo pipefail

curl -sL https://github.com/sol/hpack/releases/download/"$HPACKVER"/hpack_linux.gz | gunzip > ~/.local/bin/hpack

chmod +x ~/.local/bin/hpack

find packages/ -path 'packages/*/package.yaml' -exec "$HOME/.local/bin/hpack" '{}' ';'
