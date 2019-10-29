#!/bin/bash
set -o nounset -o errexit -o pipefail

TAG="$1"
SUFFIX="linux-$(uname -m)"
USER="jaspervdj"
REPOSITORY="$(basename -- *.cabal ".cabal")"
BINARY="$REPOSITORY"

echo "Tag: $TAG"
echo "Suffix: $SUFFIX"
echo "Repository: $REPOSITORY"

$BINARY --version

if [[ -z "$TAG" ]]; then
    echo "Not a tagged build, skipping release..."
    exit 0
fi

# Install ghr
GHR_VERSION="v0.13.0"
curl --silent -L -O \
    "https://github.com/tcnksm/ghr/releases/download/${GHR_VERSION}/ghr_${GHR_VERSION}_linux_386.tar.gz"
tar xf ghr_${GHR_VERSION}_linux_386.tar.gz
mv ghr_${GHR_VERSION}_linux_386/ghr .

# Install upx
UPX_VERSION="3.94"
curl --silent -L -O \
    "https://github.com/upx/upx/releases/download/v${UPX_VERSION}/upx-${UPX_VERSION}-amd64_linux.tar.xz"
tar xf upx-${UPX_VERSION}-amd64_linux.tar.xz
mv upx-${UPX_VERSION}-amd64_linux/upx .

# Create tarball
PACKAGE="$REPOSITORY-$TAG-$SUFFIX"
mkdir -p "$PACKAGE"
cp "$(which "$BINARY")" "$PACKAGE"
./upx -q "$PACKAGE/$BINARY"
cp README.* "$PACKAGE"
cp CHANGELOG.* "$PACKAGE"
cp extra/patat.1 "$PACKAGE"
tar -czf "$PACKAGE.tar.gz" "$PACKAGE"
rm -r "$PACKAGE"

# Actually upload
./ghr -u "$USER" -r "$REPOSITORY" "$TAG" "$PACKAGE.tar.gz"
