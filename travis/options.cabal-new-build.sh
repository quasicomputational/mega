#!/bin/bash

set -euxo pipefail

printf -- '--project-file=cabal/ghc-%s.project' "$GHCSERIES"
