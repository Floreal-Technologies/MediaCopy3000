#!/usr/bin/env bash
set -euo pipefail

VERSION=2.11.0
BIN_DIR=${WEEDER_BIN_DIR:-$HOME/.local/bin}
PROJECT_DIR=$(cd "$(dirname "$0")/.." && pwd)

compiler=$(sed -n 's/^with-compiler: *//p' "$PROJECT_DIR/cabal.project")

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"

cabal install "weeder-$VERSION" \
  --with-compiler="$compiler" \
  --installdir="$BIN_DIR" \
  --install-method=copy \
  --overwrite-policy=always
echo "installed: $BIN_DIR/weeder (v$VERSION, $compiler)"

command -v weeder >/dev/null 2>&1 || echo "note: $BIN_DIR is not on PATH" >&2
