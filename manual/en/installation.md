# Installation

## Latest Version (v0.1.0)

### Windows and macOS

| Your computer | Package |
|---|---|
| Windows 10 or 11, 64-bit Intel or AMD | [`mediacopy3000-0.1.0-x64.msi`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-x64.msi) |
| macOS 15 or later, Apple silicon (M1, M2, M3, M4) | [`mediacopy3000-0.1.0-macos-15-arm64.pkg`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-macos-15-arm64.pkg) |

### Linux

| Distribution | Intel or AMD (x86_64) | ARM (aarch64) |
|---|---|---|
| Ubuntu 24.04 and Debian | [`.deb`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-ubuntu-24-x86_64.deb) | [`.deb`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-ubuntu-24-aarch64.deb) |
| Ubuntu 26.04 | [`.deb`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-ubuntu-26-x86_64.deb) | [`.deb`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-ubuntu-26-aarch64.deb) |
| Fedora 44 and later | [`.rpm`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-fedora-44-x86_64.rpm) | [`.rpm`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-fedora-44-aarch64.rpm) |
| Arch Linux | [`.pkg.tar.zst`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-archlinux-x86_64.pkg.tar.zst) | [`.pkg.tar.zst`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-archlinux-aarch64.pkg.tar.zst) |
| Any Linux with Flatpak | [`.flatpak`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-x86_64.flatpak) | [`.flatpak`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-aarch64.flatpak) |
| Any other Linux | [`.tar.gz`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-linux-x86_64.tar.gz) | [`.tar.gz`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-linux-aarch64.tar.gz) |

These links give the packages of release 0.1.0. [All releases](https://github.com/Floreal-Technologies/MediaCopy3000/releases) has the older versions, and
[`mediacopy3000-head`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/tag/mediacopy3000-head) has the build of the last commit.

A package file is named `mediacopy3000-<version>-<system>-<architecture>.<format>`. The version is a
number such as `0.1.0`, or the word `head`.

## Install it

### Debian and Ubuntu

```
sudo apt install ./mediacopy3000-1.2.3-ubuntu-24-x86_64.deb
```

### Fedora

```
sudo dnf install ./mediacopy3000-1.2.3-fedora-44-x86_64.rpm
```

### Arch Linux

```
sudo pacman -U ./mediacopy3000-1.2.3-archlinux-x86_64.pkg.tar.zst
```

### Flatpak

```
flatpak install --user ./mediacopy3000-1.2.3-x86_64.flatpak
```

The bundle needs the `org.gnome.Platform` runtime, version 49. Flatpak gets the
runtime from Flathub when your system does not have it.

### The Linux archive

The archive holds the file tree that goes under `/usr`. Unpack it there:

```
sudo tar -C /usr -xzf mediacopy3000-1.2.3-linux-x86_64.tar.gz
```

The archive names no dependency. Install GTK 4 and libadwaita 1.7 or later with the package manager
of your system. The `.deb`, `.rpm` and `.pkg.tar.zst` packages name both for you.

### macOS

```
sudo installer -pkg mediacopy3000-1.2.3-macos-15-arm64.pkg -target /
```

The package puts `MediaCopy3000.app` in `/Applications`. The application carries its own copy of
GTK and libadwaita, so the system needs nothing else.

This package has no Apple developer signature, and macOS will refuse it on the first start.
To start it, open **System Settings** > **Privacy & Security**, then choose **Open Anyway**.

### Windows

Double-click the `.msi` file. The installer works for the account that runs it, and it asks for no
administrator rights. The files go to `%LOCALAPPDATA%\Programs\MediaCopy 3000`.

## The `head` tag

The release with the tag `mediacopy3000-head` is the build of the last commit on the `main` branch.
It is marked as a pre-release. Each commit on `main` replaces it: the packages, the tag, and the
date all move to the new commit.

The same word is the version label of the build script. `scripts/package.sh -v head` builds the
`head` packages on your own computer, and `scripts/package.sh -v 1.2.3` builds the packages of a
release.
