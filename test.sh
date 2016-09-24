#!/bin/bash
set -o nounset -o errexit -o pipefail

dumps=$(find 'tests/' -name '*.dump')

for dump in $dumps; do
    src=$(echo "$dump" | sed 's/\.dump$//')
    echo -n "Testing $src... "
    expected=$(stack exec patat -- --dump --force "$src")

    if [[ $@ == "--fix" ]]; then
        echo "$expected" >"$dump"
        echo 'Fixed'
    else
        actual=$(cat "$dump")
        if [[ "$expected" == "$actual" ]]; then
            echo 'OK'
        else
            echo 'files differ'
            exit 1
        fi
    fi
done
