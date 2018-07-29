#!/bin/bash

set -euxo pipefail

printf -- '--project-file=cabal/%s.project\n' "$1"
printf -- '--enable-tests --enable-benchmarks\n'
