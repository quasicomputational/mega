#!/bin/bash

set -euxo pipefail

cabal v2-run --project-file=cabal/"$PROJECT".project meta -- check-hash

cabal v2-run --project-file=cabal/"$PROJECT".project meta -- check-stale-cabal
