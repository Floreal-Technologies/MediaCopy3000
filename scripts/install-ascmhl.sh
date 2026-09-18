#!/usr/bin/env bash
set -euo pipefail

if ! command -v uv >/dev/null 2>&1; then
  echo "uv is required: https://docs.astral.sh/uv/" >&2
  exit 1
fi

uv tool install --force ascmhl
echo "installed: $(command -v ascmhl) and $(command -v ascmhl-debug)"
