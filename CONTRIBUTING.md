# Contributing to MediaCopy 3000

## Building

### What you need

- GHC 9.14.1 and `cabal`
- GTK 4.20, libadwaita 1.6 and Pango 1.56 or later, with their development files
- [`just`](https://just.systems/) for the recipes
- Node.js 22 and `npm` for the manual
- [`uv`](https://docs.astral.sh/uv/), for the conformance tests

## Windows

On native Windows, the GTK stack comes from MSYS2. Install the `ucrt64` packages
`pkgconf`, `gobject-introspection`, `gtk4`, `libadwaita` and `ntldd` from the
`mingw-w64-ucrt-x86_64` prefix.

If cabal has trouble finding libraries, help it by including this in your `cabal.project.local` file:

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

## macOS

To build MC3K on macOS, `xxhash` and `gobject-introspection` need to be installed from an external package manager.
```
   brew install xxhash gobject-introspection
````
`

### Development mode

```
MC3K_ENV=dev cabal run mediacopy3000
```

With `MC3K_ENV=dev`, the application reads `assets/styles.css` from the
working directory and loads it again every time the file changes. Otherwise,
the application reads the installed copy: first the `data-files` directory,
(`mediacopy3000_datadir` variable), and then `share/mediacopy3000` under the
installation prefix.

## Architecture

The repository holds four packages:

* `ascmhl`, which is the format of the manifests;
* `plugin-protocol`, which is the format between the application and its plug-ins;
* `mediacopy3000`, which is the application;
* `mediacopy3000-credits`, which is the Credits plug-in.

### ascmhl

`ascmhl` is a stand-alone library that implements the ASC Media Hash List file format.

### mediacopy3000

#### The Runtime loop

The interface is Model-View-Update, Elm architecture-style:

```
Message ──▶ update :: Message -> Model -> (Model, List AppEffect)
                                           │           │
                                           ▼           ▼
                                        render     runEffect (IO)
```

### Commands

`MediaCopy.Interface.Command` is the catalogue of commands. Commands have
a kebab-case id and a Fluent label (`command-<id>` in `locales/*.ftl`).
`MediaCopy.Gtk.Actions` binds each command to a GAction, an accelerator and a
shortcuts-window section. The header buttons, the detail-pane buttons, the main
menu, the shortcuts window and the command palette all read this catalogue.
`MediaCopy.Interface.Palette` matches and ranks the commands for the palette.

A command reaches the model as the message `RunCommand`. `update` runs it only when the command is
enabled.

To add a command:

1. Add the constructor and its id in `Command.hs`.
2. Add the two Fluent keys.
3. Add one row in `commandSpec` in `Model.hs`.
4. Add one branch in `runCommand` in `Model.hs`.

### The two phases of a job

#### Planning

The planning phase reads and computes a `JobPlan`.
`MediaCopy.Engine.Planner` gathers the facts from the disk,
 and `MediaCopy.Domain.Preflight` turns the facts into the plan.

#### Execution
Once the user approves the plan, `MediaCopy.Engine` runs it. The plan names its
steps, its targets, its hash format and its findings.

A queued job is planned again when its turn comes. `planEquivalent` compares the two plans. A plan
that moved sends its job to `NeedsReview` and goes back for review.

## Packaging

### Packages

```
scripts/package.sh -v head [deb|rpm|pacman|flatpak|tarball|osxpkg]
```

The `-v` flag sets the version label. Use `head` for a development build, or a version number such
as `1.2.3`. The format argument is optional. Without it, the script builds the native formats of the
host operating system. `flatpak` is built only when you name it.

The Flatpak build compiles nothing inside the sandbox. The script builds the application on the
host, then stages it under `dist-package/root/usr`. The manifest
`packaging/flatpak/tech.floreal.MediaCopy3000.yaml` copies that tree into `/app`. It names the
`org.gnome.Platform` runtime, version 51. `flatpak-builder` pulls the runtime from Flathub, so
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
`assets/tech.floreal.MediaCopy3000.svg` changes.

The installer is not signed. SmartScreen shows a warning the first time a person starts the
application. To get past the warning, select "More info", then "Run anyway".
