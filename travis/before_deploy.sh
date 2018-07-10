#!/bin/bash

set -euxo pipefail

# Note: specialised for stack builds. TODO: When cabal new-install supports binaries from local packages (targetting 2.4!), investigate that instead.
# TODO: bandwidth is expensive; -split-sections when we can

# Example tag name: release/q4c12-twofinger-0.1
# We wish to transform that to q4c12-twofinger
PACKAGE=$( printf '%s' "$TRAVIS_TAG" \
         | sed 's#^release/##' \
         | rev \
         | cut -d- -f2- \
         | rev \
         )

mkdir bin/

stack install "$PACKAGE" --local-bin-path bin/

ARCH=$(uname -m)

find bin/ -type f -exec mv {} {}."$TRAVIS_OS_NAME-$ARCH" \;
