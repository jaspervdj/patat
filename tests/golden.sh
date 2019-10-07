#!/bin/bash
set -o nounset -o errexit -o pipefail

srcs=$(find tests/golden -type f ! -name '*.dump')
stuff_went_wrong=false

for src in $srcs; do
    expected="$src.dump"
    echo -n "Testing $src... "
    actual=$(mktemp)
    HOME=/dev/null patat --dump --force "$src" >"$actual"

    if [[ $@ == "--fix" ]]; then
        cp "$actual" "$expected"
        echo 'Fixed'
    elif [[ ! -f "$expected" ]]; then
        echo "missing file: $expected"
        stuff_went_wrong=true
    elif [[ "$(cat "$expected")" == "$(cat "$actual")" ]]; then
        echo 'OK'
    else
        echo 'files differ'
        diff "$actual" "$expected" || true
        stuff_went_wrong=true
    fi
done

if [[ "$stuff_went_wrong" = true ]]; then
    exit 1
fi
