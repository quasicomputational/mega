#!/bin/bash

set -euxo pipefail

stack --no-terminal build --test --bench --haddock --no-haddock-deps --fast $@
