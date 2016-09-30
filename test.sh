#!/bin/bash
set -o nounset -o errexit -o pipefail

dumps=$(find 'tests/' -name '*.dump')
stuff_went_wrong=false

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
            stuff_went_wrong=true
        fi
    fi
done

if [[ "$stuff_went_wrong" = true ]]; then
    exit 1
fi
