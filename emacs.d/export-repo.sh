#!/bin/bash

if [ -z "$1" ]; then
    echo "Usage: $0 <destination>" >&2
    exit 1
fi

set -o errexit
set -o nounset

BOOTSTRAP_FILE="$(dirname $0)/export-repo.el"

test -f "$BOOTSTRAP_FILE"

emacs -Q --batch -l  "$BOOTSTRAP_FILE" --eval "(export-repo \"$1\")"
