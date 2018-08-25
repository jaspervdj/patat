#!/bin/bash
set -o nounset -o errexit -o pipefail

PATAT_CONF=~/.patat.yaml
PATAT_CONF_BACKUP=~/.patat.yaml.testing

function restore() {
    mv $PATAT_CONF_BACKUP $PATAT_CONF
}

if [[ -f "$PATAT_CONF" ]]; then
    mv $PATAT_CONF $PATAT_CONF_BACKUP
    trap restore EXIT
fi

srcs=$(find tests -type f ! -name '*.dump')
stuff_went_wrong=false

for src in $srcs; do
    expected="$src.dump"
    echo -n "Testing $src... "
    actual=$(mktemp)
    patat --dump --force "$src" >"$actual"

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
