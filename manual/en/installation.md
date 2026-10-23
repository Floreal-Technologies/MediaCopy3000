# Installation

## Latest Version (v0.1.0)

### Windows and macOS

| Operating System | Package |
|---|---|
| Windows 10 or 11, 64-bit Intel or AMD | [`mediacopy3000-0.1.0-x64.msi`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-x64.msi) |
| macOS 15 or later, Apple silicon (M1, M2, M3, M4) | [`mediacopy3000-0.1.0-macos-15-arm64.pkg`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-macos-15-arm64.pkg) |

### Linux

| Distribution | Intel or AMD (x86_64) | ARM (aarch64) |
|---|---|---|
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

### Ubuntu

```
sudo apt install ./mediacopy3000-1.2.3-ubuntu-26-x86_64.deb
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

The bundle needs the `org.gnome.Platform` runtime. Flatpak gets the runtime from
Flathub when your system does not have it.

### macOS

This package has no Apple developer signature, and macOS will refuse it on the first start.
To start it, open **System Settings** > **Privacy & Security**, then choose **Open Anyway**.

### Windows

Double-click the `.msi` file and proceed with the installation wizard.
The files go to `%LOCALAPPDATA%\Programs\MediaCopy 3000`.

## The `head` tag

The release with the tag `mediacopy3000-head` is the build of the last commit on the `main` branch.
It is marked as a pre-release. Each commit on `main` replaces it: the packages, the tag, and the
date all move to the new commit.

The same word is the version label of the build script. `scripts/package.sh -v head` builds the
`head` packages on your own computer, and `scripts/package.sh -v 1.2.3` builds the packages of a
release.
