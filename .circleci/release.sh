#!/bin/bash
set -o nounset -o errexit -o pipefail

TAG="$1"
SUFFIX="$2"

echo "Tag: $TAG"
echo "Suffix: $SUFFIX"

if [[ -z "$TAG" ]]; then
    echo "Setting test tag..."
    TAG="v0.0.666"
fi

GHR_VERSION="v0.5.4"

# Install ghr
wget "https://github.com/tcnksm/ghr/releases/download/${GHR_VERSION}/ghr_${GHR_VERSION}_linux_386.zip"
unzip ghr_${GHR_VERSION}_linux_386.zip

# Create binary tarball
mkdir -p "patat-$TAG-$SUFFIX"
cp "$(which patat)" "patat-$TAG-$SUFFIX"
cp "README.md" "patat-$TAG-$SUFFIX"
tar -czf "patat-$TAG-$SUFFIX.tar.gz" "patat-$TAG-$SUFFIX"
rm -r "patat-$TAG-$SUFFIX"

# TODO: Actually upload
