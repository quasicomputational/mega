#!/bin/bash

set -euxo pipefail

cabal new-run --project-file=cabal/"$PROJECT".project meta -- check-hash

cabal new-run --project-file=cabal/"$PROJECT".project meta -- check-stale-cabal
