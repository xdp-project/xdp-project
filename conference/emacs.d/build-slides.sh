#!/bin/bash

if [ -z "$1" ]; then
    echo "Usage: $0 <filename>" >&2
    exit 1
fi

set -o errexit
set -o nounset

BOOTSTRAP_FILE="$(dirname $0)/bootstrap-build.el"

test -f "$BOOTSTRAP_FILE"

emacs -Q --batch -l  "$BOOTSTRAP_FILE" --eval "(export-slides-file \"$1\")"
