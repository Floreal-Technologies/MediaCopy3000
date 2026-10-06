#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

case "$(uname -s)" in
  Linux) PLUGINS="${XDG_DATA_HOME:-$HOME/.local/share}/mediacopy3000/plugins" ;;
  Darwin) PLUGINS="$HOME/Library/Application Support/MediaCopy3000/Plugins" ;;
  *)
    echo "no install step for $(uname -s); see plugins/credits/README.md" >&2
    exit 1
    ;;
esac

DIR="$PLUGINS/tech.floreal.credits"

cabal build -v0 exe:credits
BIN="$(cabal list-bin exe:credits | tail -1)"

mkdir -p "$DIR/bin"
ln -sfn "$ROOT/plugins/credits/plugin.json" "$DIR/plugin.json"
ln -sfn "$BIN" "$DIR/bin/credits"
echo "installed: $DIR"
