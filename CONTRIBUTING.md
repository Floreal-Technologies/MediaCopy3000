# Contributing to MediaCopy 3000

## Building

### What you need

- GHC 9.14.1 and `cabal`
- GTK 4.20, libadwaita 1.6 and Pango 1.56 or later, with their development files
- xxHash and GObject Introspection, with their development files
- [`just`](https://just.systems/) for the recipes
- Node.js 22 and `npm` for the manual
- [`uv`](https://docs.astral.sh/uv/) for the conformance tests

### Windows

On native Windows, the GTK stack comes from MSYS2. Install the `ucrt64` packages `pkgconf`,
`gobject-introspection`, `gtk4`, `libadwaita`, `xxhash` and `ntldd` from the
`mingw-w64-ucrt-x86_64` prefix.

If cabal does not find the libraries, add this to `cabal.project.local`:

```
package *
  extra-include-dirs: C:\ghcup\msys64\ucrt64\include
  extra-lib-dirs: C:\ghcup\msys64\ucrt64\lib
```

### Windows with WSL2

WSL2 runs a Linux distribution inside Windows. In WSL2, you build and run the Linux version of the
application. WSLg shows its window on the Windows desktop. Use Windows 11 or later.

1. Install a distribution that has GTK 4.20, libadwaita 1.6 and Pango 1.56 or later, for example
   Ubuntu 26.04 or Fedora 43.
2. Install GHC 9.14.1 and `cabal` with GHCup, from inside the distribution.
3. Install the development packages. On Ubuntu, use the `apt install` list in the job `tests` of
   `.github/workflows/ci.yml`.
4. Make sure that the versions are GTK 4.20, libadwaita 1.6 and Pango 1.56 or later:

   ```
   pkg-config --modversion gtk4 libadwaita-1 pango
   ```

5. Clone the repository into the Linux file system, for example `~/mediacopy3000`. Do not clone it
   under `/mnt/c`.

`cabal.project.local` must not contain the MSYS2 directories from the section above. Those
directories are for a native Windows build only.

In WSL2, `scripts/package.sh` makes Linux packages. To make the Windows installer, use
`scripts/package.ps1` in a native Windows build.

### macOS

On macOS, install the dependencies with Homebrew:

```
brew install gtk4 libadwaita xxhash gobject-introspection pkgconfig
```

### Development mode

```
MC3K_ENV=dev cabal run mediacopy3000
```

With `MC3K_ENV=dev`, the application reads `assets/styles.css` from the working directory and loads
it again every time the file changes. Otherwise, the application reads the installed copy: first
the `data-files` directory (`mediacopy3000_datadir` variable), and then `share/mediacopy3000` under
the installation prefix.

### Recipes

We use a [`justfile`](https://just.systems/) to interact with the project.
Install the [`just`](https://just.systems/man/en/installation.html) command-line tool
and type `just` in the project repository to see all the available commands.

## Architecture

The repository holds four packages:

- `ascmhl`, which is the format of the manifests;
- `plugin-protocol`, which is the format between the application and its plug-ins;
- `mediacopy3000-credits`, which is the Credits plug-in;
- `mediacopy3000`, which is the application.

### ascmhl

`ascmhl` is a stand-alone library that implements the ASC Media Hash List file format.

### plugin-protocol

`plugin-protocol` holds the types of the messages between the application and its plug-ins. The
JSON schema `plugin-protocol/schema/protocol-1.schema.json` comes from these types. To write it
again after a change, run `just schema`.

### mediacopy3000-credits

`mediacopy3000-credits`, in `plugins/credits/`, is a plug-in that puts the crew into each manifest.
To build and install it, run `just install-credits`.

### mediacopy3000

#### The runtime loop

The interface is Model-View-Update, Elm architecture-style. `update` is in `MediaCopy.Model`:

```
Message ──▶ (update :: Message -> Model -> (Model, List AppEffect))
                                           │           │
                                           ▼           ▼
                                        render     runEffect (IO)
```

#### Building the interface

Read [`STYLE_GUIDE.md`](STYLE_GUIDE.md) before you change the interface. It has the rules for GTK
widgets, signal handlers, widget state, lists and localisation. Its section "MediaCopy 3000" has
the rules for this application only.

##### Layers

The interface has three layers. They keep GTK code out of the model and the view models.

The model holds the application state. The view models come from the model: they represent the
visual elements and hold the display logic. The widgets paint the view models into GTK widgets,
with as little logic as possible.

| Layer       | Task                                        |
|-------------|---------------------------------------------|
| Model       | State, message handling, Update             |
| View models | Turn the state into interface elements      |
| Widgets     | Build GTK widgets and paint the view models |

##### The life of a Message

1. A button activates its GAction, from `MediaCopy.Gtk.Actions`.
2. The action calls `dispatch (RunCommand c)`.
3. `MediaCopy.Runtime` runs `update`, then calls `render` in `MediaCopy.Gtk.View`.
4. That `render` calls the render function of each widget.
5. The runtime runs the effects.

`UiMessage` comes from the user. Other `Message` constructors come from workers, the clock and the
desktop.

There are two `dispatch` functions:

- The runtime `dispatch` in `MediaCopy.Runtime` that takes a `Message`.
- Widgets and actions get a `dispatch` callback that takes a `UiMessage`.
  It wraps the message in `Ui` and calls the runtime `dispatch`.

Both run on the GLib main loop only. Worker threads use the field `post` of the
record `Shell`, in `MediaCopy.Runtime`.

##### A widget

A widget module has two parts:

- `newX` builds the widget tree, connects the handlers, and gives the root and a render function.
  In the example below, the render function is the `update` field of `FileRow`.
- The render function paints a view model on the existing widgets.

Example:

```haskell
import Data.GI.Base (AttrOp ((:=)))
import Data.GI.Base qualified as GI
import Floreal.Commons.Gtk.Common qualified as Common
import GI.Gtk (Box (..), Orientation (..))
import GI.Gtk qualified as Gtk

data FileRow = FileRow
  { row :: Box
  , update :: FileRowView -> IO ()
  }

newFileRow :: IO FileRow
newFileRow = do
  name <- Common.newLabel "" [#xalign := 0, #hexpand := True] []
  row <- GI.new Box [#orientation := OrientationHorizontal]
  Gtk.boxAppend row name
  cell <- Common.newCell $ \view -> do
    GI.set name [#label := view.name]
    Common.toggleClass row "error" view.failed
  pure FileRow {row, update = Common.renderCell cell}
```

To add a widget:

1. Write the view model in a module under `MediaCopy.Interface.View`.
2. Write the widget in a module under `MediaCopy.Gtk.Widgets`.
3. In `buildWidgets`, in `MediaCopy.Gtk.View`, call `newX`. Call its render function from
   `render`.

##### Skip the repaint

`render` runs after each message, and the clock sends one `Tick` each second.

A repaint that is not necessary can move the cursor, lose the selection and fire signals again.
Use these tools to skip it:

| The data goes                    | Use                              |
|----------------------------------|----------------------------------|
| From the model to the widget     | `Cell` (`newCell`, `renderCell`) |
| Both ways (the user can edit it) | `Control` with `bind`            |
| Into rows                        | `KeyedList` or `newRowsCell`     |

`renderCell` skips the paint when the new view model is equal to the last one. It uses the `Eq`
instance of the view model. If you write that instance yourself, you choose which parts of the view
model cause a repaint.

##### Text

To add translated text:

1. Add the message to `locales/en.ftl` and `locales/fr.ftl`. If `fr.ftl` does not have it, the
   build fails.
2. Add a function in `MediaCopy.Interface.Wording` that reads it.
3. Call that function from the view model.

#### Commands

`MediaCopy.Interface.Command` is the catalogue of commands. Commands have
a kebab-case id and a Fluent label (`command-<id>` in `locales/*.ftl`).
`MediaCopy.Gtk.Actions` binds each command to a GAction, an accelerator and a
shortcuts-window section. The header buttons, the detail-pane buttons, the main
menu, the shortcuts window and the command palette all read this catalogue.
`MediaCopy.Interface.Palette` matches and ranks the commands for the palette.

A command reaches the model as the message `RunCommand`. The `update` function
runs it only when the command is enabled.

To add a command:

1. Add the constructor and its id in `MediaCopy.Interface.Command`.
2. Add the label `command-<id>` to `locales/en.ftl` and to `locales/fr.ftl`.
3. Add one row in `commandSpec`, in `MediaCopy.Model`.
4. Add one branch in `runCommand`, in `MediaCopy.Model`.

#### The two phases of a job

##### Planning

The planning phase reads the disk and computes a `Plan` (`MediaCopy.Domain.Plan`).
`MediaCopy.Engine.Planner` gathers the facts from the disk, and `MediaCopy.Domain.Preflight` turns
the facts into the plan.

##### Execution

Once the user approves the plan, `MediaCopy.Engine` runs it. The plan names its steps, its targets,
its hash format and its findings.

A queued job is planned again when its turn comes. `MediaCopy.Domain.Plan.equivalent` compares the
approved plan with the new plan. If they are equivalent, the job starts. If not, the job goes to
`NeedsReview`, the next job in the queue starts, and the user must review the new plan.

## Packaging

### Packages

```
./scripts/package.sh -v head [deb|rpm|pacman|flatpak|tarball|osxpkg]
```

The `-v` flag sets the version label. Use `head` for a development build, or a version number such
as `1.2.3`. The format argument is optional. Without it, the script builds the native formats of
the host operating system. `flatpak` is built only when you name it.

The Flatpak build compiles nothing inside the sandbox. The script builds the application on the
host, then stages it under `dist-package/root/usr`. The manifest
`packaging/flatpak/tech.floreal.MediaCopy3000.yaml` copies that tree into `/app`. It names the
`org.gnome.Platform` runtime, version 51. `flatpak-builder` pulls the runtime from Flathub, so the
first build needs a network.

The script then compares the glibc version of the binary against the version in the runtime. If the
binary needs a newer glibc, the script stops and tells you to build on an older host.

### The Windows installer

`scripts/package.sh` is a POSIX shell script, so Windows has its own:

```
./scripts/package.ps1 -Version 1.2.3
```

The script stages `mediacopy3000.exe` with the libraries and the data files it needs. `ntldd` finds
the libraries. The script then builds an MSI with WiX 7, which it installs as a .NET tool. You need
the .NET SDK for this step.

The installer writes to the local application data of the user. It asks for no administrator, so a
person can install the application on a machine they do not own.

The icon comes from `scripts/make-windows-icon.sh`. Run it again only when
`assets/tech.floreal.MediaCopy3000.svg` changes.

The installer is not signed. SmartScreen shows a warning the first time a person starts the
application. To get past the warning, select "More info", then "Run anyway".
