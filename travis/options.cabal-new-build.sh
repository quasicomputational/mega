#!/bin/bash

set -euxo pipefail

printf -- '--project-file=cabal/ghc-%s.project\n' "$GHCSERIES"
printf -- '--enable-tests --enable-benchmarks\n'
