#!/bin/bash

set -euxo pipefail

# Same logic as in deps.cabal-new-build.sh: this is only a preparatory cache-filling step, so it's okay to fail (and in fact we have to be polite and swallow the failure or else we'll block the next stage!)
stack --no-terminal build --test --bench --only-dependencies --fast $@ || true
