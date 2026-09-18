# Contributing to MediaCopy 3000

## Architecture

Here are described the different parts:
The glossary in this manual's index holds the words the code uses for its own concepts, you should read it first.

The repository holds two packages: `ascmhl`, which is the format of the manifests,
and `mediacopy3000`, which is the application.

| Component | Where | What it holds |
|---|---|---|
| `lib:ascmhl` | `ascmhl/src` | The ASC MHL types, reader, writer, folder layout and error set. |
| `lib:domain` | `src/domain` | Domain types (a job, a plan, a finding, the preflight decision). |
| `lib:application` | `src/application` | The engine that runs a plan |
| `lib:interface` | `src/interface` | The model the window shows, the report, and the appearance. |
| `lib:mediacopy3000` | `src/gtk` | The widgets, the runtime, the theme. |
| `lib:demo` | `src/demo` | The scenes for the screenshots, and the fixtures the model tests share. |
| `exe:mediacopy3000` | `app/Main.hs` | Starts the runtime. |

The dependency arrows go like this:

* `ascmhl` ← `domain` ← `application`;
* `domain` ← `interface`.

This separation of concerns allow us to keep a maximum of things outside of GTK (and so we are able to test them independently).

### The loop

The interface is a model, messages and commands (Elm architecture-style):

```
Msg ──▶ update :: Msg -> Model -> (Model, List Cmd)
                     │              │
                     ▼              ▼
                   render        runCmd (IO)
```

- `MediaCopy.Model` holds `Model`, `Msg`, `Cmd` and `update`. It is pure.
- `MediaCopy.Gtk.Runtime` owns the model, runs the commands, and turns their results back into
  messages.
- `MediaCopy.Gtk.View` renders a whole model. Each widget owns a cell that leaves itself alone when
  its value has not changed.

### The two phases of a job

#### Planning

The planning phase reads and computes a `JobPlan`.
`MediaCopy.Engine.Plan` gathers the facts from the disk,
 and `MediaCopy.Domain.Preflight` turns the facts into the plan.

#### Execution
Once the user approves the plan, `MediaCopy.Engine` runs it. The plan names its
steps, its targets, its hash format and its findings.

A queued job is planned again when its turn comes. `planEquivalent` compares the two plans. A plan
that moved sends its job to `NeedsReview` and goes back for review.

## Build and package

### What you need

- GHC 9.14.1 and `cabal`
- GTK 4 and libadwaita 1.7 or later, with their development files
- [`just`](https://just.systems/), for the recipes below
- Node.js 22 and `npm`, for `just docs` and `just docs-build`
- [`uv`](https://docs.astral.sh/uv/), for `just deps-conformance`

On Windows, the GTK stack comes from MSYS2. Install the `ucrt64` packages
`pkgconf`, `gobject-introspection`, `gtk4`, `libadwaita` and `ntldd` from the
`mingw-w64-ucrt-x86_64` prefix.

Put them in the MSYS2 that GHCup installs, at `C:\ghcup\msys64`. A second MSYS2 beside it gives
errors that are hard to read, which the Cabal manual warns about. Cabal finds no library on
Windows until `cabal.project.local` names the directories:

```
package *
  extra-include-dirs: C:\ghcup\msys64\ucrt64\include
  extra-lib-dirs: C:\ghcup\msys64\ucrt64\lib
```

CI builds and tests the application on Windows. The job `tests-windows` runs `core-test` and
`ascmhl-test`. The job `package-windows` builds the installer on each push. Both are in
`.github/workflows/ci.yml`.

### Development mode

```
MC3K_ENV=dev cabal run mediacopy3000
```

With `MC3K_ENV=dev`, the application reads `assets/styles.css` from the source
tree and loads it again every time the file changes. Without it, the application
reads the installed copy of the same file.

### Packages

```
scripts/package.sh -v head [deb|rpm|pacman|flatpak|tarball|osxpkg]
```

The `-v` flag sets the version label. Use `head` for a development build, or a version number such
as `1.2.3`. The format argument is optional. Without it, the script builds the native formats of the
host operating system. `flatpak` is built only when you name it.

The Flatpak build compiles nothing inside the sandbox. The script builds the application on the
host, then stages it under `dist-package/root/usr`. The manifest
`packaging/flatpak/eu.choutri.MediaCopy3000.yml` copies that tree into `/app`. It names the
`org.gnome.Platform` runtime, version 49. `flatpak-builder` pulls the runtime from Flathub, so
the first build needs a network.

The script then compares the glibc version of the binary against the version in the runtime. If
the binary needs a newer glibc, the script stops and tells you to build on an older host.

### The Windows installer

`scripts/package.sh` is a POSIX shell script, so Windows has its own:

```
./scripts/package.ps1 -Version 1.2.3
```

The script stages `mediacopy3000.exe` with the libraries and the data files it needs. `ntldd` finds
the libraries. The script then builds an MSI with WiX 7, which it installs as a .NET tool.

The installer writes to the local application data of the user. It asks for no administrator, so a
person can install the application on a machine they do not own.

The icon comes from `scripts/make-windows-icon.sh`. Run it again only when
`assets/eu.choutri.MediaCopy3000.svg` changes.

The installer is not signed. SmartScreen shows a warning the first time a person starts the
application. To get past the warning, select "More info", then "Run anyway".

## Screenshots

Every picture in this manual comes out of the application itself. Run `just screenshots` after an
interface change, and the whole set is current again.

```
just screenshots
```

The script writes one PNG for each scene into `manual/en/images/`. It prints the size of each file, and
it ends with a non-zero status when a scene wrote nothing.

### Why it exists

The screens past the empty window cannot be reached by a script. Every job starts at a native folder
picker, and this desktop has no way to drive one. The model also keeps nothing on disk, so nothing
can be seeded from outside the process.

So the application seeds itself. A scene is a list of messages folded through the production
`update`, which means every state a picture shows is a state the application can reach.

### How a run works

```
MC3K_ENV=dev MC3K_DEMO=<scene> MC3K_SHOT=<file.png> mediacopy3000
```

1. `app/Main.hs` reads the two variables and looks the scene up in `MediaCopy.Demo`.
2. The runtime renders the scene's frames in place of the empty model, one after the other.
3. The runtime activates the scene's GAction, if it has one. `about` is the one that does.
4. The runtime opens the expanders and scrolls to the end, if the scene asks for it.
5. `MediaCopy.Gtk.Screenshot` renders the window into a texture and writes the PNG.
6. The window closes and the process ends.

The picture is taken inside the process, so it holds no cursor, no shadow and no part of the desktop
behind it. A libadwaita dialog draws inside its own window, which is why a sheet, an alert and the
about page all arrive in the same texture.

An unknown scene name stops the process and prints the names it knows.

### The parts

| File | What it holds |
|---|---|
| `src/demo/MediaCopy/Demo.hs` | The scenes and the messages that build them |
| `src/demo/MediaCopy/Demo/Fixtures.hs` | The media source, the jobs, the plans and the history. The model tests read them too. |
| `src/gtk/MediaCopy/Gtk/Screenshot.hs` | `Startup`, the seam a seeded run comes in through. Window to PNG, open the expanders, scroll to the end |
| `scripts/screenshots.sh` | One process per scene, and an icon theme for the about page |

`Startup` is empty for a normal run, and every part of this path is then dead. A plain
`cabal run mediacopy3000` starts the empty application, as it always has.

### Adding a scene

1. Add the messages and the `Scene` to `scenes` in `src/demo/MediaCopy/Demo.hs`.
2. Put new fixture data in `src/demo/MediaCopy/Demo/Fixtures.hs`.
3. Run `just screenshots` and look at the picture.
4. Put the picture in the page that needs it.

The script does not hold a list of scenes. It asks the binary with `--list-scenes`, so a scene
added to `scenes` is photographed on the next run and nothing else needs an edit.

Some rules the fixtures obey:

- Every scene is stamped with one fixed instant, so two runs make the same picture.
- A seeded window is not a pointer target and shows no focus ring. The desktop's pointer can rest
  over the window, and a hovered row would differ from one run to the next.
- The shot waits 3.5 seconds after the last frame. An overlay scrollbar shows itself when its
  content is laid out and fades out over the three seconds that follow.
- The script sets `GSK_RENDERER=cairo`. The GPU renderer draws the rounded corners of the window
  a little differently on each run. The cairo renderer draws the same pixels every time.

Two runs of `just screenshots` then write the same bytes for every picture but one. The spinner on
`plan-planning.png` turns whatever the settings say, so that picture differs in the spinner alone.
- A scene that must show a copy rate needs three frames. The interface builds the row on the first
  render, takes its first sample on the second, and has two samples to divide on the third.
- The paths are literals. No scene reads or writes a disk.
- The planner makes every plan. A fixture gives it facts. No fixture writes a plan by hand.

### If the window comes out empty

The application has one application id. A second process with the same id hands its start-up to the
one already running, and no picture comes out. Close any running MediaCopy 3000 first.
