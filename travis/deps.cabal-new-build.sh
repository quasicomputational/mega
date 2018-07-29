#!/bin/bash

set -euxo pipefail

CABALOPTS=$(./travis/options.cabal-new-build.sh "$1")

cabal new-update $CABALOPTS

# Always succeed: this is only priming the cache, and these are batched up per GHC version (i.e., for multiple projects). If this fails for one project, then we don't want to cause the other projects to fail; further, that failure will be reported by the build stage.
# TODO: failure masking? I guess we'll get visibility if builds start taking too long...
cabal new-build all --only-dependencies $CABALOPTS || true
