#!/bin/bash

set -euxo pipefail

stack --no-terminal build --test --bench --only-dependencies --fast $@
