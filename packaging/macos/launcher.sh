#!/bin/sh
set -eu

DIR="$(CDPATH='' cd -P "$(dirname "$0")" && pwd)"
CONTENTS="$(CDPATH='' cd -P "$DIR/.." && pwd)"
RESOURCES="${CONTENTS}/Resources"

XDG_DATA_DIRS="${RESOURCES}/share"
GSETTINGS_SCHEMA_DIR="${RESOURCES}/share/glib-2.0/schemas"
GDK_PIXBUF_MODULE_FILE="${RESOURCES}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache"
mediacopy3000_datadir="${RESOURCES}/share/mediacopy3000"
export XDG_DATA_DIRS GSETTINGS_SCHEMA_DIR GDK_PIXBUF_MODULE_FILE mediacopy3000_datadir

exec "$DIR/mediacopy3000-bin" "$@"
