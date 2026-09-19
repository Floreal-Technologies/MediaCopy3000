# Installation

## Dernière Version (v0.1.0)

### Windows et macOS

| Votre ordinateur | Paquet |
|---|---|
| Windows 10 ou 11, Intel ou AMD 64 bits | [`mediacopy3000-0.1.0-x64.msi`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-x64.msi) |
| macOS 15 ou ultérieur, Apple Silicon (M1, M2, M3, M4) | [`mediacopy3000-0.1.0-macos-15-arm64.pkg`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-macos-15-arm64.pkg) |

### Linux

| Distribution | Intel ou AMD (x86_64) | ARM (aarch64) |
|---|---|---|
| Ubuntu 24.04 et Debian | [`.deb`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-ubuntu-24-x86_64.deb) | [`.deb`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-ubuntu-24-aarch64.deb) |
| Ubuntu 26.04 | [`.deb`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-ubuntu-26-x86_64.deb) | [`.deb`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-ubuntu-26-aarch64.deb) |
| Fedora 44 et ultérieur | [`.rpm`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-fedora-44-x86_64.rpm) | [`.rpm`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-fedora-44-aarch64.rpm) |
| Arch Linux | [`.pkg.tar.zst`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-archlinux-x86_64.pkg.tar.zst) | [`.pkg.tar.zst`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-archlinux-aarch64.pkg.tar.zst) |
| Toute distribution Linux avec Flatpak | [`.flatpak`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-x86_64.flatpak) | [`.flatpak`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-aarch64.flatpak) |
| Toute autre distribution Linux | [`.tar.gz`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-linux-x86_64.tar.gz) | [`.tar.gz`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/download/v0.1.0/mediacopy3000-0.1.0-linux-aarch64.tar.gz) |

Ces liens donnent les paquets de la version 0.1.0. [Toutes les versions](https://github.com/Floreal-Technologies/MediaCopy3000/releases) contient les versions
antérieures, et [`mediacopy3000-head`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/tag/mediacopy3000-head) contient la construction du
dernier commit.

Un fichier de paquet est nommé `mediacopy3000-<version>-<système>-<architecture>.<format>`. La
version est un numéro tel que `0.1.0`, ou bien le mot `head`.

## Installer le paquet

### Debian et Ubuntu

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

Ce paquet nécessite l'environnement d'exécution `org.gnome.Platform`, version
49. Flatpak le récupère depuis Flathub lorsque votre système ne le possède pas.

### L'archive Linux

L'archive contient l'arborescence de fichiers destinée à `/usr`. Décompressez-la à cet endroit :

```
sudo tar -C /usr -xzf mediacopy3000-1.2.3-linux-x86_64.tar.gz
```

L'archive ne déclare aucune dépendance. Installez GTK 4 et libadwaita 1.7 ou ultérieur avec le
gestionnaire de paquets de votre système. Les paquets `.deb`, `.rpm` et `.pkg.tar.zst` déclarent
ces deux dépendances pour vous.

### macOS

```
sudo installer -pkg mediacopy3000-1.2.3-macos-15-arm64.pkg -target /
```

Le paquet place `MediaCopy3000.app` dans `/Applications`. L'application embarque
sa propre copie de GTK et de libadwaita : le système n'a besoin de rien d'autre.

Ce paquet ne porte pas de signature de développeur Apple, et macOS le refusera
au premier démarrage. Pour le démarrer, ouvrez **Réglages Système** >
**Confidentialité et sécurité**, puis choisissez **Ouvrir quand même**.

### Windows

Double-cliquez sur le fichier `.msi`. L'installation vaut pour le compte qui la
lance, et ne demande aucun droit d'administrateur. Les fichiers sont placés dans
`%LOCALAPPDATA%\Programs\MediaCopy 3000`.

## L'étiquette `head`

La publication portant l'étiquette `mediacopy3000-head` est la publication du
dernier commit de la branche `main`. Elle est marquée comme pré-version. Chaque
commit sur `main` la remplace : les paquets, l'étiquette et la date pointent
alors sur le nouveau commit.

Ce même mot sert d'étiquette de version au script de construction.
`scripts/package.sh -v head` construit les paquets `head` sur votre propre
machine, et `scripts/package.sh -v 1.2.3` construit les paquets d'une version
publiée.
