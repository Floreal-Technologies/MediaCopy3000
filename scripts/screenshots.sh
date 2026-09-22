#!/usr/bin/env bash
set -euo pipefail

out=${1:-manual/en/images}
mkdir -p "$out"
out=$(cd "$out" && pwd)

binary=$(cabal list-bin mediacopy3000)

icons=$(mktemp -d)
trap 'rm -rf "$icons"' EXIT
mkdir -p "$icons/icons/hicolor/scalable/apps"
cp assets/eu.choutri.MediaCopy3000.svg "$icons/icons/hicolor/scalable/apps/"
export XDG_DATA_DIRS="$icons:${XDG_DATA_DIRS:-/usr/local/share:/usr/share}"

mapfile -t scenes < <("$binary" --list-scenes)
if [ "${#scenes[@]}" -eq 0 ]; then
  echo "error: $binary --list-scenes printed nothing" >&2
  exit 1
fi

failed=0
for scene in "${scenes[@]}"; do
  target="$out/$scene.png"
  rm -f "$target"
  MC3K_ENV=dev MC3K_DEMO="$scene" MC3K_SHOT="$target" "$binary" || true
  if [ -s "$target" ]; then
    printf '%-20s %s\n' "$scene" "$(identify -format '%wx%h' "$target" 2>/dev/null || echo written)"
  else
    printf '%-20s FAILED\n' "$scene"
    failed=1
  fi
done

exit "$failed"
