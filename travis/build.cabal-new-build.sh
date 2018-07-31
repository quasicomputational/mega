#!/bin/bash

set -euxo pipefail

CABALOPTS=$(./travis/options.cabal-new-build.sh)

cabal new-build all $CABALOPTS

cabal new-test all $CABALOPTS

#TODO: run the benchmarks? Haddock?
