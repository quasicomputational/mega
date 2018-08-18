#!/bin/bash

set -euxo pipefail

# Note: assumes no packages have been installed to the global database post-installation.
#TODO: assumes only one ghc-pkg in scope; will break if we ever introduce another

NON_UPGRADEABLE=$(
    ghc-pkg list --global --simple-output \
  | tr ' ' '\n' \
  | perl -plne 's/-(\d+)(\.\d+)*//g' \
  | tr '\n' ',' \
  | sed 's/,$//' \
  )

# This is implemented with an ugly hack while we're stuck on cabal-install 2.2. Once we can upgrade Travis to using 2.4 (i.e., as soon as it's out and in the PPA), we can use --project-file and operate directly on the freeze file in its natural habitat.

TMPDIR=$( mktemp -d )

cp "cabal/$PROJECT.project.freeze" "$TMPDIR/cabal.project.freeze"

cd "$TMPDIR"

cabal outdated --new-freeze-file --ignore "$NON_UPGRADEABLE" --exit-code
