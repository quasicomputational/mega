#!/bin/bash

set -euxo pipefail

cabal new-update --project-file=cabal/"$PROJECT".project

cabal new-build all --only-dependencies --project-file=cabal/"$PROJECT".project
