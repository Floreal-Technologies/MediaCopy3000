procs := num_cpus()

export XDG_DATA_DIRS := if os() == "macos" {
        `brew --prefix` + "/share:" + env("XDG_DATA_DIRS", "/usr/local/share:/usr/share")
    } else {
        env("XDG_DATA_DIRS", "/usr/local/share:/usr/share")
    }

[private]
default:
    @just --list --unsorted

# Start the application
run:
    cabal run mediacopy3000

# Install the dependencies
deps:
    cabal build --only-dependencies all

# Build the project
build:
    cabal build all

# Rebuild and start the application on each code change
watch:
    ghcid -c "cabal repl lib:mediacopy3000" -T "MediaCopy.Gtk.Runtime.start Nothing"

# Remove the compilation artefacts
clean:
    cabal clean

# Start a REPL
repl:
    cabal repl

# Run the test suite
test:
    cabal test all

# Install the ASC MHL reference tool for the conformance suite
deps-conformance:
    ./scripts/install-ascmhl.sh

# Run the conformance suite against the ASC MHL reference tool
conformance:
    cabal test conformance-test --test-show-details=direct

# Write plugin-protocol/schema/protocol-1.schema.json again from the Haskell types
schema:
    cabal build plugin-protocol-test
    cabal test plugin-protocol-test --test-options='-p Schema' || true
    cp plugin-protocol/schema/actual-protocol-1.schema.json plugin-protocol/schema/protocol-1.schema.json

# Run the doctests in the pure libraries' haddocks
doctest:
    @cabal repl --with-repl=doctest ascmhl
    @cabal repl --with-repl=doctest plugin-protocol
    @cabal repl --with-repl=doctest mediacopy3000:domain
    @cabal repl --with-repl=doctest mediacopy3000:application
    @cabal repl --with-repl=doctest mediacopy3000:interface
    @cabal repl --with-repl=doctest mediacopy3000-credits

# Install the cross-reference checker for `just xref`
deps-xref:
    ./scripts/install-xreferee.sh

# Check the cross references between the code and the documentation
xref:
    xreferee -I manual/en/cross-references.md

# Install the dead code finder for `just weeder`
deps-weeder:
    ./scripts/install-weeder.sh

# Find dead code (Weeder)
weeder:
    cabal build all --builddir=dist-newstyle/weeder
    weeder --require-hs-files --hie-directory dist-newstyle/weeder

# Find the modules that slow down the build
chokepoints top="15" *args:
    cabal build all
    ghcup run --ghc 9.14.1 -- ./scripts/chokepoints.py --top {{top}} {{args}}

# Take the screenshots for the manual (closes a running instance first)
screenshots:
    ./scripts/screenshots.sh

# Measure the render cost of the messages of one job
bench-render files="500":
    cabal run -v0 render-bench -- {{files}}

# Run the code linter (HLint) with automatic refactoring
lint:
    find app bench src ascmhl plugin-protocol tests tests-conformance -name "*.hs" | xargs -P {{procs}} -I {} hlint --refactor-options="-i" --refactor {}

# Run the code formatters (cabal-gild, fourmolu)
style:
    cabal-gild mediacopy3000.cabal
    cabal-gild ascmhl/ascmhl.cabal
    cabal-gild plugin-protocol/plugin-protocol.cabal
    fourmolu -q --mode inplace app src ascmhl plugin-protocol tests tests-conformance

# Start the manual's development server (VitePress)
docs:
    npm run docs:dev

# Build the manual into manual/.vitepress/dist
docs-build:
    npm run docs:build

# Build native packages with fpm (deb/rpm/pacman on Linux, osxpkg on macOS)
package:
    ./scripts/package.sh -v head

# Build and install the Credits plug-in
install-credits:
    ./scripts/install-credits-plugin.sh
