# MediaCopy 3000

<div align="center">
  <p>
    MediaCopy 3000 is a media offloading application for videographers and photographers.
  </p>

  <p>
    <img width="40%" alt="The offloading dialog" src="https://raw.githubusercontent.com/Floreal-Technologies/MediaCopy3000/refs/heads/main/manual/en/images/offload-dialog.png">
    <img width="40%" alt="The execution plan" src="https://raw.githubusercontent.com/Floreal-Technologies/MediaCopy3000/refs/heads/main/manual/en/images/themes/queue-dracula-light-alucard.png">
  </p>

  <p>
    <img width="40%" alt="The queue view" src="https://raw.githubusercontent.com/Floreal-Technologies/MediaCopy3000/refs/heads/main/manual/en/images/themes/queue-catppuccin-dark-frappé.png">
    <img width="40%" alt="The queue view" src="https://raw.githubusercontent.com/Floreal-Technologies/MediaCopy3000/refs/heads/main/manual/en/images/themes/queue-catppuccin-light-latte.png">
  </p>
</div>

## Features

* Integrity verification through the ASC Media Hash List (MHL) manifest format
* Sealing of a media source in place, so a later copy has original hashes to check against
* Plan review before execution, so that all actions are accounted for.

## Development

[`CONTRIBUTING.md`](CONTRIBUTING.md) holds the architecture, the build and the packaging, and how
the pictures in the manual are made.

`just` lists every recipe. `just test` and `just conformance` run the two test suites, `just lint`
and `just style` run HLint and the formatters, and `just xref` checks that the cross references
between the code and `manual/` still hold.

## Packages

The repository holds two packages:

* `ascmhl/`:  the ASC MHL format
* `mediacopy3000`: The application domain, the engine, the GTK interface.

## Packaging

Build a package with `scripts/package.sh`:

```
scripts/package.sh -v head [deb|rpm|pacman|flatpak|tarball|osxpkg]
```

The `-v` flag sets the version label.
Use `head` for a development build, or a version number such as `1.2.3`.
The format argument is optional.
When you omit it, the script builds every package format for the host operating system.

## Acknowledgements

* [Guerilla.Studio](https://guerilla.studio) for the idea
* [Pomfort GmbH](https://pomfort.com) for the domain knowledge
