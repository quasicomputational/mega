#!/bin/bash

set -euxo pipefail

CABALOPTS=$(./travis/options.cabal-new-build.sh)

#TODO: remove the condition once we can use cabal-install 2.2 and hence new-update in all configurations
case "$GHCSERIES" in
  head)
    cabal new-update $CABALOPTS
    ;;
  *)
    cabal update
    ;;  
esac
