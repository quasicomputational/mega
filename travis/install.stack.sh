#!/bin/bash

set -euxo pipefail

stack --no-terminal test --only-dependencies --fast $@
