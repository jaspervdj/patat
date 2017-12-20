#!/bin/bash
set -o nounset -o errexit -o pipefail

echo $1
shift
echo $@

CIRCLE_PROJECT_USERNAME=jaspervdj
CIRCLE_PROJECT_REPONAME=patat
CIRCLE_TAG="v0.6.0.0"

USER="$CIRCLE_PROJECT_USERNAME"
REPO="$CIRCLE_PROJECT_REPONAME"
TAG="$CIRCLE_TAG"
ARCHIVE=patat.zip

TMP="$(mktemp)"

echo $USER/$REPO

echo "Creating release..."
curl \
    -H "Authorization: token $GITHUB_TOKEN" \
    -H 'Accept: application/vnd.github.v3+json' \
    -H "Content-Type: application/json" \
    --data '@-' \
    -o "$TMP" \
    "https://api.github.com/repos/$USER/$REPO/releases" <<HERE
  {
    "tag_name": "$TAG"
  }
HERE

RELEASE="$(sed -n 's/^.*\/releases\/\([0-9][0-9]*\)".*$/\1/p' "$TMP")"
echo "Release: $RELEASE"

curl \
    -H "Authorization: token $GITHUB_TOKEN" \
    -H 'Accept: application/vnd.github.v3+json' \
    -H 'Content-Type: application/zip' \
    --data-binary "@$ARCHIVE" \
    "https://uploads.github.com/repos/$USER/$REPO/releases/$RELEASE/assets?name=$ARCHIVE"
