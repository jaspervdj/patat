#!/usr/bin/env bash
set -o nounset -o errexit -o pipefail

mkdir -p cropped
rm -f cropped/*
for i in inputs/*; do
    convert \
        -alpha remove \
        -crop '1280x1000+122+138' "$i" "cropped/$(basename "$i")"
done

rm -f patat.gif
convert \
    -repage '0x0' \
    -layers coalesce \
    -layers optimize-plus \
    +repage \
    -delay 150x100 \
    -loop 0 \
    cropped/*.png patat.gif
