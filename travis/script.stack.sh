#!/bin/bash

set -euxo pipefail

stack --no-terminal test --haddock --no-haddock-deps --fast $@
