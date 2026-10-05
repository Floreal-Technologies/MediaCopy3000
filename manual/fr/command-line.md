# La ligne de commande

MediaCopy 3000 est une application avec interface graphique. Une commande spécifique s'exécute sans cette interface.

## Afficher un plan

```
mediacopy3000 plan offload SOURCE DEST [DEST…] [--resume | --replace] [--seal-first] [OPTIONS D’EXTENSION]
mediacopy3000 plan verify DOSSIER [OPTIONS D’EXTENSION]
mediacopy3000 plan seal DOSSIER [OPTIONS D’EXTENSION]
```

La commande lit les dossiers, affiche le plan et se termine. Elle n'écrit aucune donnée. `mediacopy3000 plan --help`
affiche les formats de plan, et `mediacopy3000 --help` affiche les commandes disponibles.

Les autres arguments sont transmis à la boîte à outils (toolkit), ce qui correspond à la manière dont le fichier de lancement du bureau ouvre la fenêtre.

| Code de sortie | Signification |
|---|---|
| `0` | Le plan est prêt. |
| `1` | Un obstacle bloque l'opération. Le plan l'indique. |
| `2` | Les arguments sont incorrects ou le plan n'a pas pu être généré. Le message s'affiche sur la sortie d'erreur. |

`--resume` et `--replace` déterminent l'action à entreprendre si une
destination contient déjà une copie partielle. La page [Transfert d'une source
multimédia](offload.md) explique ces deux options. `--seal-first` scelle la
source multimédia avant la copie et interrompt le processus si le scellement
révèle un problème.

## Options d’extension

La commande de plan démarre les [extensions](plugins.md) activées, comme la fenêtre.

| Option | Effet |
|---|---|
| `--no-plugins` | Fait le plan sans aucune extension. |
| `--plugin-field ID.CLÉ=VALEUR` | Donne le champ de tâche `CLÉ` à l’extension `ID`. Répétez l’option pour chaque champ. |

Un blocage d’une extension donne aussi le code de sortie `1`. La commande écrit une ligne sur la
sortie d’erreur pour chaque extension non valide ou non activée, et pour chaque ligne qu’une
extension écrit sur sa propre sortie d’erreur.
