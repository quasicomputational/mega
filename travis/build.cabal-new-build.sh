#!/bin/bash

set -euxo pipefail

cabal new-build all --project-file=cabal/"$PROJECT".project

cabal new-test all --project-file=cabal/"$PROJECT".project

#TODO: run the benchmarks? Haddock?
