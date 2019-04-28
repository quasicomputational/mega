#!/bin/bash

set -euxo pipefail

cabal v2-build all --project-file=cabal/"$PROJECT".project

cabal v2-test all --project-file=cabal/"$PROJECT".project

#TODO: run the benchmarks? Haddock?
