#!/bin/bash

set -euxo pipefail

exec ./travis/script.stack.sh --ghc-options=-Werror
