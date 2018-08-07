#!/bin/bash

set -euxo pipefail

printf -- '--project-file=cabal/%s.project\n' "$PROJECT"
printf -- '--enable-tests --enable-benchmarks --disable-optimisation\n'
