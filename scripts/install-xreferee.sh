#!/usr/bin/env bash
# Installs the pinned xreferee release for `just xref`.
# https://github.com/brandonchinn178/xreferee
set -euo pipefail

VERSION=1.1.1
BIN_DIR=${XREFEREE_BIN_DIR:-$HOME/.local/bin}
RELEASES=https://github.com/brandonchinn178/xreferee/releases

case "$(uname -s)/$(uname -m)" in
  Linux/x86_64) asset=linux-x86_64 sha=9d9293f1fd20e113603d809414588e3aad1ce631ce5368371057e1dc178bc1b5 ;;
  Linux/aarch64 | Linux/arm64) asset=linux-arm64 sha=b0eef35279ef7db7540fe4f9111f092e48a5f94897acdb3cab0c80c2b10700d6 ;;
  Darwin/x86_64) asset=osx-x86_64 sha=7ef824d70dd89606f4ed7d3b5fc119be53be5e99cca47debe660cb321b4007fe ;;
  Darwin/arm64) asset=osx-arm64 sha=454b4527d4102d95b8ad8b59a32aec6e16e2825e4d3e9b8785860cbf2cd83679 ;;
  *)
    echo "no xreferee $VERSION release for $(uname -s)/$(uname -m); see $RELEASES" >&2
    exit 1
    ;;
esac

url="$RELEASES/download/v$VERSION/xreferee-$VERSION-$asset"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

curl -fsSL -o "$tmp/xreferee" "$url"
echo "$sha  $tmp/xreferee" | sha256sum --check --status ||
  {
    echo "checksum mismatch for $url" >&2
    exit 1
  }

mkdir -p "$BIN_DIR"
install -m 0755 "$tmp/xreferee" "$BIN_DIR/xreferee"
echo "installed: $BIN_DIR/xreferee (v$VERSION)"

command -v xreferee >/dev/null 2>&1 || echo "note: $BIN_DIR is not on PATH" >&2
