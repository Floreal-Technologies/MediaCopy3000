#!/usr/bin/env bash
set -euo pipefail

SVG=assets/eu.choutri.MediaCopy3000.svg
OUT=packaging/windows/mediacopy3000.ico
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

for size in 16 32 48 64 128 256; do
  inkscape --export-type=png --export-filename="${WORK}/i${size}.png" \
    -w "$size" -h "$size" "$SVG" >/dev/null
done

mkdir -p "$(dirname "$OUT")"
magick "${WORK}/i16.png" "${WORK}/i32.png" "${WORK}/i48.png" \
  "${WORK}/i64.png" "${WORK}/i128.png" "${WORK}/i256.png" "$OUT"

echo "wrote $OUT"
magick identify "$OUT"
