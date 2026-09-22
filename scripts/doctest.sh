#!/usr/bin/env bash
set -euo pipefail

targets=(ascmhl:lib:ascmhl mediacopy3000:lib:domain mediacopy3000:lib:interface)

shim=$PWD/dist-newstyle/doctest-as-ghc
mkdir -p dist-newstyle
cat >"$shim" <<'SHIM'
#!/usr/bin/env bash
for arg; do
  if [ "$arg" = --interactive ]; then exec doctest "$@"; fi
done
exec ghc-9.14.1 "$@"
SHIM
chmod +x "$shim"

for target in "${targets[@]}"; do
  cabal repl --with-compiler="$shim" "$target"
done
