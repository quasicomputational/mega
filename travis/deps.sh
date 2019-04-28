#!/bin/bash

set -euxo pipefail

cabal v2-update --project-file=cabal/"$PROJECT".project

cabal v2-build all --only-dependencies --project-file=cabal/"$PROJECT".project
