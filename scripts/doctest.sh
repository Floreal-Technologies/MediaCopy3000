#!/usr/bin/env bash
# Runs the doctests in the pure libraries' haddocks.
#
# Needs a doctest that links the GHC 9.14.1 API:
#   cabal install doctest-0.25.0 --with-compiler=ghc-9.14.1 --overwrite-policy=always
#
# `cabal repl` drives it, so every source directory, default extension and package flag stays in
# the .cabal files, where the compiler already reads them.
set -euo pipefail

targets=(ascmhl:lib:ascmhl mediacopy3000:lib:domain mediacopy3000:lib:interface)

# cabal calls its compiler twice: once to build a library the target depends on, once to start the
# repl. doctest answers only the second and dies on `--make`, so this shim sends the first on to
# ghc. Its path stays the same between runs, because cabal reconfigures and rebuilds every
# dependency when the compiler it was given moves.
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
