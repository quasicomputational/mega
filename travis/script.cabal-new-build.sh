#!/bin/bash

set -euxo pipefail

CABALOPTS=$(./travis/options.cabal-new-build.sh)

cabal new-build all --enable-tests $CABALOPTS

cabal new-test all --enable-tests $CABALOPTS
