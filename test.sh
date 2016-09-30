#!/bin/bash
set -o nounset -o errexit -o pipefail

dumps=$(find 'tests/' -name '*.dump')
found_diff=false

for expected in $dumps; do
    src=$(echo "$expected" | sed 's/\.dump$//')
    echo -n "Testing $src... "
    actual=$(mktemp)
    stack exec patat -- --dump --force "$src" >"$actual"

    if [[ $@ == "--fix" ]]; then
        cp "$actual" "$expected"
        echo 'Fixed'
    else
        if [[ "$(cat "$expected")" == "$(cat "$actual")" ]]; then
            echo 'OK'
        else
            echo 'files differ'
            diff "$actual" "$expected" || true
            found_diff=true
        fi
    fi
done

if [[ "$found_diff" = true ]]; then
    exit 1
fi
