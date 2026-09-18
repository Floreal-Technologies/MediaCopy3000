# Installation

MediaCopy 3000 est un logiciel gratuit.

## Où récupérer un paquet

| Ce que vous cherchez | Page |
|---|---|
| La dernière version publiée | [Dernière version](https://github.com/Floreal-Technologies/MediaCopy3000/releases/latest) |
| Une version antérieure | [Toutes les versions](https://github.com/Floreal-Technologies/MediaCopy3000/releases) |
| La construction du dernier commit | [`mediacopy3000-head`](https://github.com/Floreal-Technologies/MediaCopy3000/releases/tag/mediacopy3000-head) |

## Quel fichier choisir

Un fichier de paquet est nommé `mediacopy3000-<version>-<système>-<architecture>.<format>`. La
version est un numéro tel que `1.2.3`, ou bien le mot `head`.

| Fichier | Système |
|---|---|
| `…-macos-15-<arch>.pkg` | macOS 15 et ultérieur |
| `…-x64.msi` | Windows sur un ordinateur Intel ou AMD 64 bits |
| `…-ubuntu-24-<arch>.deb` | Ubuntu 24.04 et Debian |
| `…-ubuntu-26-<arch>.deb` | Ubuntu 26.04 |
| `…-fedora-44-<arch>.rpm` | Fedora 44 et ultérieur |
| `…-archlinux-<arch>.pkg.tar.zst` | Arch Linux |
| `…-<arch>.flatpak` | Toute distribution Linux avec Flatpak |
| `…-linux-<arch>.tar.gz` | Toute autre distribution Linux |


L'architecture est `x86_64` pour un ordinateur Intel ou AMD. Elle est `aarch64`
sous Linux et `arm64` sous macOS pour un ordinateur ARM, tel qu'un Mac équipé
d'une puce Apple Silicon (M1, M2, M3, M4, etc).

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
