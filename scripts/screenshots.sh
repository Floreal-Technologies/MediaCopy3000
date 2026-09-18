#!/usr/bin/env bash
# Takes one picture of each demo scene for the pages in manual/en/.
#
# Each scene runs the application in its own process. The process seeds the model with fixture data,
# renders it, saves a PNG of its own window and exits. A job reads no file and writes no file,
# because the scenes hold literal paths.
#
# If MediaCopy 3000 runs, close it first. A second process with the same application id gives its
# start-up to the first one, and no picture comes out.
set -euo pipefail

out=${1:-manual/en/images}
mkdir -p "$out"
out=$(cd "$out" && pwd)

binary=$(cabal list-bin mediacopy3000)

# The about page draws the application icon from an icon theme. A run from the source tree has no
# place in one, so these lines give it a theme that holds only that icon.
icons=$(mktemp -d)
trap 'rm -rf "$icons"' EXIT
mkdir -p "$icons/icons/hicolor/scalable/apps"
cp assets/eu.choutri.MediaCopy3000.svg "$icons/icons/hicolor/scalable/apps/"
export XDG_DATA_DIRS="$icons:${XDG_DATA_DIRS:-/usr/local/share:/usr/share}"

# The binary is the one list of scene names; `--list-scenes` prints them in order.
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
