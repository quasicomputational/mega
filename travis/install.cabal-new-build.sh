#!/bin/bash

set -euxo pipefail

CABALOPTS=$(./travis/options.cabal-new-build.sh)

cabal new-build all --only-dependencies $CABALOPTS
