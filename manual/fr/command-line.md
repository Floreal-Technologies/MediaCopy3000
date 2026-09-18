# La ligne de commande

MediaCopy 3000 est une application avec interface graphique. Une commande spécifique s'exécute sans cette interface.

## Afficher un plan

```
mediacopy3000 plan offload SOURCE DEST [DEST…] [--resume | --replace] [--seal-first]
mediacopy3000 plan verify DOSSIER
mediacopy3000 plan seal DOSSIER
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
