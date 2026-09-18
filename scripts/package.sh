#!/usr/bin/env bash
set -euo pipefail

#
# Usage: scripts/package.sh -v <version>|head [format...]
#   -v: required. A version such as 1.2.3, or "head".
#   formats: deb rpm pacman flatpak tarball (Linux), osxpkg (macOS)
#   With no format, the script builds every fpm format for the host OS.
#   The script builds flatpak only on request. flatpak needs
#   flatpak-builder and the GNOME runtime.
#
# The output goes to dist-package/out/.
#
# Every tool call, tar and the others, must work under GNU and macOS.

usage() {
  echo "usage: scripts/package.sh -v <version>|head [format...]"
  echo "  -v   mandatory; version label for the package file name:"
  echo "       a numeric version (e.g. 1.2.3) or \"head\""
  echo "  formats: deb rpm pacman flatpak tarball (Linux), osxpkg (macOS)"
}

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

OS="$(uname -s)"

VERSION_LABEL=""
while getopts ":v:h" opt; do
  case "$opt" in
    v) VERSION_LABEL="$OPTARG" ;;
    h)
      usage
      exit 0
      ;;
    :)
      echo "error: option -$OPTARG requires an argument" >&2
      usage >&2
      exit 1
      ;;
    \?)
      echo "error: unknown option -$OPTARG" >&2
      usage >&2
      exit 1
      ;;
  esac
done
shift $((OPTIND - 1))

if [ -z "$VERSION_LABEL" ]; then
  echo "error: -v is mandatory" >&2
  usage >&2
  exit 1
fi

if [ "$VERSION_LABEL" != "head" ] \
  && ! [[ "$VERSION_LABEL" =~ ^[0-9]+(\.[0-9]+)*$ ]]; then
  echo "error: -v expects a numeric version (e.g. 1.2.3) or \"head\", got '$VERSION_LABEL'" >&2
  exit 1
fi

if [ "$#" -gt 0 ]; then
  FORMATS=("$@")
elif [ "$OS" = "Darwin" ]; then
  FORMATS=(osxpkg)
else
  FORMATS=(deb rpm pacman)
fi

for fmt in "${FORMATS[@]}"; do
  case "$OS,$fmt" in
    Darwin,osxpkg | Linux,deb | Linux,rpm | Linux,pacman | Linux,flatpak | Linux,tarball) ;;
    *)
      echo "error: cannot build '$fmt' on $OS" >&2
      exit 1
      ;;
  esac
done

if [ "$OS" = "Darwin" ]; then
  PREFIX=/usr/local
else
  PREFIX=/usr
fi

CABAL_VERSION="$(awk '/^version:/ {print $2}' mediacopy3000.cabal)"
if TAG="$(git describe --tags --exact-match 2>/dev/null)"; then
  VERSION="${TAG#v}"
else
  VERSION="${CABAL_VERSION}.git$(git rev-parse --short HEAD)"
fi

ARCH="$(uname -m)"

# Minimum supported OS per package format.
min_os_for() {
  case "$1" in
    deb) echo "ubuntu-$(. /etc/os-release && echo "${VERSION_ID%%.*}")" ;;
    rpm) echo fedora-44 ;;
    pacman) echo archlinux ;;
    osxpkg) echo macos-15 ;;
  esac
}

echo "==> Building mediacopy3000 ${VERSION}"
CONFIGURE_FLAGS=(--project-file=cabal.release.project --datadir="${PREFIX}/share" --datasubdir=mediacopy3000)
cabal build exe:mediacopy3000 "${CONFIGURE_FLAGS[@]}"
BIN="$(cabal list-bin mediacopy3000 "${CONFIGURE_FLAGS[@]}" | tail -1)"

echo "==> Staging filesystem root"
STAGING="dist-package/root"
rm -rf dist-package
mkdir -p dist-package/out

install_file() {
  local mode="$1" src="$2" dst="$3"
  mkdir -p "$(dirname "$dst")"
  install -m "$mode" "$src" "$dst"
}

# The palettes keep their place under the data directory, because the run reads the tree by its
# shape: <family>/<light|dark>/<name>.css, and the family's own family.json beside them.
install_themes() {
  local data_dir="$1" file
  while IFS= read -r file; do
    install_file 644 "$file" "${data_dir}/${file}"
  done < <(find assets/themes -type f | sort)
}

macos_bundle() {
  local brew_prefix contents loaders_src loaders_dst loader iconset size
  brew_prefix="$(brew --prefix)"
  contents="${ROOT}/${STAGING}/Applications/MediaCopy3000.app/Contents"

  install_file 755 "$BIN" "${contents}/MacOS/mediacopy3000-bin"
  strip "${contents}/MacOS/mediacopy3000-bin"
  install_file 755 packaging/macos/launcher.sh "${contents}/MacOS/mediacopy3000"
  sed "s/@VERSION@/${VERSION}/g" packaging/macos/Info.plist > "${contents}/Info.plist"

  install_file 644 assets/styles.css \
    "${contents}/Resources/share/mediacopy3000/assets/styles.css"
  install_themes "${contents}/Resources/share/mediacopy3000"

  mkdir -p "${contents}/Resources/share/glib-2.0/schemas"
  "${brew_prefix}/bin/glib-compile-schemas" \
    --targetdir="${contents}/Resources/share/glib-2.0/schemas" \
    "${brew_prefix}/share/glib-2.0/schemas"

  loaders_src="${brew_prefix}/lib/gdk-pixbuf-2.0/2.10.0/loaders"
  loaders_dst="${contents}/Resources/lib/gdk-pixbuf-2.0/2.10.0/loaders"
  mkdir -p "$loaders_dst"
  cp "${loaders_src}"/* "${loaders_dst}/"

  "${brew_prefix}/bin/gdk-pixbuf-query-loaders" "${loaders_src}"/* \
    | sed "s|${brew_prefix}/lib|/Applications/MediaCopy3000.app/Contents/Resources/lib|g" \
    > "${contents}/Resources/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache"

  local bundler_args
  bundler_args=(
    -od -b
    -x "${contents}/MacOS/mediacopy3000-bin"
    -d "${contents}/Frameworks"
    -p '@executable_path/../Frameworks/'
    -s "${brew_prefix}/lib"
  )
  for loader in "${loaders_dst}"/*; do
    bundler_args+=(-x "$loader")
  done
  dylibbundler "${bundler_args[@]}"

  codesign --force -s - \
    "${contents}/Frameworks/"*.dylib \
    "${loaders_dst}"/* \
    "${contents}/MacOS/mediacopy3000-bin"

  iconset="${ROOT}/dist-package/mediacopy3000.iconset"
  mkdir -p "$iconset"
  for size in 16 32 128 256 512; do
    rsvg-convert -w "$size" -h "$size" \
      assets/eu.choutri.MediaCopy3000.svg -o "${iconset}/icon_${size}x${size}.png"
    rsvg-convert -w "$((size * 2))" -h "$((size * 2))" \
      assets/eu.choutri.MediaCopy3000.svg -o "${iconset}/icon_${size}x${size}@2x.png"
  done
  iconutil -c icns -o "${contents}/Resources/mediacopy3000.icns" "$iconset"
}

if [ "$OS" = "Darwin" ]; then
  macos_bundle
else
  install_file 755 "$BIN" "${STAGING}${PREFIX}/bin/mediacopy3000"
  strip "${STAGING}${PREFIX}/bin/mediacopy3000"
  install_file 644 assets/styles.css "${STAGING}${PREFIX}/share/mediacopy3000/assets/styles.css"
  install_themes "${STAGING}${PREFIX}/share/mediacopy3000"
  install_file 644 assets/eu.choutri.MediaCopy3000.desktop \
    "${STAGING}${PREFIX}/share/applications/eu.choutri.MediaCopy3000.desktop"
  install_file 644 assets/eu.choutri.MediaCopy3000.svg \
    "${STAGING}${PREFIX}/share/icons/hicolor/scalable/apps/eu.choutri.MediaCopy3000.svg"
  install_file 644 assets/eu.choutri.MediaCopy3000.metainfo.xml \
    "${STAGING}${PREFIX}/share/metainfo/eu.choutri.MediaCopy3000.metainfo.xml"
fi

FLATPAK_MANIFEST=packaging/flatpak/eu.choutri.MediaCopy3000.yml

FPM_COMMON=(
  -s dir
  -n mediacopy3000
  -v "$VERSION"
  --license "GPL-3.0-only"
  --maintainer "Feriel Choutri de Tarlé <feriel@choutri.eu>"
  --description "Verified media offload and ASC MHL verification for videographers"
  --url "https://github.com/tchoutri/MediaCopy3000"
  -a native
  -C "$STAGING"
  -f
)

build_osxpkg() {
  local pkg_name="$1"
  local components="dist-package/osxpkg-components.plist"
  echo "==> pkgbuild ($pkg_name)"
  pkgbuild --analyze --root "$STAGING" "$components"
  /usr/libexec/PlistBuddy -c "Set :0:BundleIsRelocatable false" "$components"
  pkgbuild \
    --root "$STAGING" \
    --identifier eu.choutri.MediaCopy3000 \
    --version "$VERSION" \
    --install-location / \
    --component-plist "$components" \
    "dist-package/out/${pkg_name}"
}

check_flatpak_glibc() {
  local runtime version binary_glibc runtime_glibc
  runtime="$(sed -n 's/^runtime: *//p' "$FLATPAK_MANIFEST")"
  version="$(sed -n 's/^runtime-version: *//p' "$FLATPAK_MANIFEST" | tr -d "'")"
  binary_glibc="$(objdump -T "${STAGING}${PREFIX}/bin/mediacopy3000" | grep -o 'GLIBC_[0-9.]*' | sed 's/^GLIBC_//' | sort -uV | tail -1)"
  runtime_glibc="$(flatpak run --user --command=ldd "${runtime}//${version}" --version | awk 'NR == 1 { print $NF }')"
  if ! printf '%s\n%s\n' "$binary_glibc" "$runtime_glibc" | sort -CV; then
    echo "error: binary needs glibc ${binary_glibc} but ${runtime}//${version} ships ${runtime_glibc}; build on an older host" >&2
    exit 1
  fi
  echo "==> glibc check: binary ${binary_glibc} <= runtime ${runtime_glibc}"
}

build_flatpak() {
  local pkg_name="$1"
  echo "==> flatpak-builder ($pkg_name)"
  flatpak-builder --force-clean --user --install-deps-from=flathub \
    --repo=dist-package/flatpak-repo \
    dist-package/flatpak-build \
    "$FLATPAK_MANIFEST"
  check_flatpak_glibc
  flatpak build-bundle dist-package/flatpak-repo \
    "dist-package/out/${pkg_name}" eu.choutri.MediaCopy3000
}

for fmt in "${FORMATS[@]}"; do
  EXTRA=()
  case "$fmt" in
    flatpak)
      build_flatpak "mediacopy3000-${VERSION_LABEL}-${ARCH}.flatpak"
      continue
      ;;
    tarball)
      PKG_NAME="mediacopy3000-${VERSION_LABEL}-linux-${ARCH}.tar.gz"
      echo "==> tar ($PKG_NAME)"
      tar -C "${STAGING}${PREFIX}" -czf "dist-package/out/${PKG_NAME}" .
      continue
      ;;
    osxpkg)
      build_osxpkg "mediacopy3000-${VERSION_LABEL}-$(min_os_for "$fmt")-${ARCH}.pkg"
      continue
      ;;
    deb) EXTRA=(-d libgtk-4-1 -d 'libadwaita-1-0 (>= 1.7)'); EXT=deb ;;
    rpm) EXTRA=(-d gtk4 -d 'libadwaita >= 1.7'); EXT=rpm ;;
    pacman) EXTRA=(-d gtk4 -d 'libadwaita>=1.7'); EXT=pkg.tar.zst ;;
  esac
  PKG_NAME="mediacopy3000-${VERSION_LABEL}-$(min_os_for "$fmt")-${ARCH}.${EXT}"
  echo "==> fpm -t $fmt ($PKG_NAME)"
  fpm "${FPM_COMMON[@]}" -t "$fmt" -p "dist-package/out/${PKG_NAME}" "${EXTRA[@]}" .
done

echo "==> Packages:"
ls -l dist-package/out/
