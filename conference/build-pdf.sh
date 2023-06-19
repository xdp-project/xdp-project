#!/bin/sh

if [ -z "$1" ] || [ -z "$2" ]; then
    echo "Usage: $0 <filename> <decktape command>" >&2
    exit 1
fi

set -o errexit
set -o nounset

FILE="$1"
DECKTAPE="$2"

width=$(grep -o "^width:[[:space:]]*[0-9]\+" "$FILE" | sed 's/[^0-9]//g')
height=$(grep -o "^height:[[:space:]]*[0-9]\+" "$FILE" | sed 's/[^0-9]//g')

if [ -z "$width" ] || [ -z "$height" ]; then
    echo "Couldn't find width and height definitions in $FILE" >&2
    exit 1
fi

$DECKTAPE -s "${width}x${height}" -p 300 "$FILE" "${FILE/.html/.pdf}"
