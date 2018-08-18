#!/bin/bash

set -euxo pipefail

CABALOPTS=$(./travis/options.cabal-new-build.sh)

cabal new-update $CABALOPTS

cabal new-build all --only-dependencies $CABALOPTS