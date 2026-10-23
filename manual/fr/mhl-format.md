# Le format ASC MHL

ASC MHL est le format *Media Hash List* (liste d’empreintes de médias) de l’American Society of Cinematographers. MediaCopy 3000
lit et écrit la version 2.0 de ce format. Cette page traite de la partie du format utilisée dans
cette application. La spécification officielle fait autorité.

## La structure des dossiers

Chaque destination reçoit une copie du dossier de la source multimédia. À l’intérieur de cette copie se trouve un dossier `ascmhl` :

```
<destination>/CARD_A001/…les fichiers…
<destination>/CARD_A001/ascmhl/ascmhl_chain.xml
<destination>/CARD_A001/ascmhl/0001_CARD_A001_2026-09-12_140300.mhl
<destination>/CARD_A001/ascmhl/0002_CARD_A001_2026-09-13_091500.mhl
```

Le nom du fichier manifeste contient le numéro de génération, le nom du dossier, la date et l’heure.

Le dossier `ascmhl` est inclus dans la copie. Il contient les générations de la source ainsi que
la génération propre au déchargement en cours.

## La chaîne

Le fichier `ascmhl_chain.xml` énumère toutes les générations dans l’ordre. Pour chacune d’elles, il contient une empreinte `c4`
du fichier manifeste lui-même.

Le format `c4` est imposé par la spécification pour cet usage précis ; il n’est donc jamais modifiable ni variable,
quel que soit le format d’empreinte utilisé pour les fichiers eux-mêmes.

## Un manifeste

Un manifeste décrit l’arborescence dans laquelle il se trouve. Pour chaque fichier, il enregistre le chemin, la taille, l’empreinte
et l’action effectuée sur le fichier lors de l’opération :

| Action | Signification |
|---|---|
| `original` | Il s’agit des premières empreintes calculées pour ce fichier. |
| `verified` | L’empreinte correspondait à un enregistrement existant. |
| `failed` | L’empreinte ne correspondait pas à l’enregistrement. |

MediaCopy 3000 écrit ces trois valeurs, et aucune autre. Son lecteur accepte aussi `new`, que d’anciens outils écrivaient, et le lit comme `verified`.

Un manifeste consigne également les empreintes des répertoires : une empreinte de contenu et une empreinte de structure pour chaque répertoire, ainsi qu’une paire pour la racine. L’annexe G de la spécification décrit comment ces valeurs sont agrégées à partir des éléments enfants. La paire racine constitue le `roothash` de l’arborescence.

Les entrées suivent l’ordre indiqué dans la section 6.5 de la spécification. Pour chaque répertoire, le manifeste consigne ses sous-répertoires, puis ses propres fichiers, et enfin le répertoire lui-même. La racine ne possède pas d’entrée propre.

## Processus d’une génération

Chaque génération indique l’opération effectuée :

| Processus | Écrit par |
|---|---|
| `in-place` | Un scellement et une vérification |
| `transfer` | Le côté destination d’un déchargement |
| `flatten` | Non écrit par cette application |

Le format ne prévoit pas de processus `verify` spécifique. C’est pourquoi une génération de vérification est consignée sous le type `in-place`.

## Formats d’empreinte

| Format | MediaCopy 3000 |
|---|---|
| `xxh64` | Écrit pour chaque nouvelle tâche et lu |
| `md5` | Lu ; continue d’être écrit pour un dossier déjà enregistré avec ce format |
| `sha1` | Lu ; continue d’être écrit pour un dossier déjà enregistré avec ce format |
| `c4` | Utilisé uniquement pour la chaîne |

Une tâche utilise un format unique pour tous les fichiers qu’elle traite. Ce format est déterminé par les enregistrements servant de référence pour la tâche ; il n’est jamais choisi manuellement. Si les enregistrements contiennent plusieurs formats, la tâche ne peut pas démarrer en raison du blocage `le format d’empreinte ne peut pas être fixé`.

## Lecture d’un manifeste sans cette application

Les fichiers sont au format XML et sont conçus pour perdurer au-delà de tout outil spécifique. D’autres outils ASC MHL sont capables de lire les fichiers générés par MediaCopy 3000. La suite de tests de conformité présente dans ce dépôt vérifie cette compatibilité en la comparant à l’outil de référence ASC, `ascmhl`.
