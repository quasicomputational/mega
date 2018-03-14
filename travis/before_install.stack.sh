#!/bin/bash

set -euxo pipefail

curl -sL https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

stack setup
