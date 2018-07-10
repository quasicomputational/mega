#!/bin/bash

set -euxo pipefail

# If there is a tag, then we only want to run if we're the release. But if there's not a tag we should run unconditionally.

CMD=$1
shift

if [ -z "${TRAVIS_TAG+x}" -o "${RELEASE:-false}" = true ]
then
    "$CMD" "$@"
fi
