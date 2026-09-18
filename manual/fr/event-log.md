# Le journal des événements

Chaque tâche génère un journal lors de son exécution : un fichier par tâche, une ligne par événement. Ce journal est conservé une fois la tâche terminée et l'application fermée.

## Emplacement

| Système | Dossier |
|---|---|
| Linux, macOS | `~/.local/state/mediacopy3000/jobs/` |
| Windows | `%LOCALAPPDATA%\mediacopy3000\jobs\` |

Sous Linux, la variable `XDG_STATE_HOME` permet de modifier l'emplacement de ce dossier.

Le nom du fichier combine l'heure de création de la tâche, son numéro, le nom
de son dossier et son type : `2026-09-15_101500-1-CARD_A001-offload.log`. Le
nombre suivant l'heure correspond au numéro de la tâche pour cette session de
l'application. Le rapport enregistré indique le nom du fichier sur la ligne «
Log ».

## Contenu

Le fichier débute par une ligne de titre, suivie du plan que vous avez approuvé, tel qu'il apparaît dans le rapport. Ensuite, une ligne par événement, avec l'heure en UTC :

```
2026-09-15T10:15:00.123Z planned 7 files, 18600000000 bytes to read
2026-09-15T10:15:00.201Z A/1.mxf Copying
2026-09-15T10:15:00.302Z progress 4194304
2026-09-15T10:15:04.881Z A/1.mxf Saving to disk
2026-09-15T10:15:05.117Z A/1.mxf Naming the copy
2026-09-15T10:15:05.203Z A/1.mxf Verifying
2026-09-15T10:15:07.910Z A/1.mxf Verified
2026-09-15T10:16:35.114Z writing the manifest
2026-09-15T10:16:40.007Z manifest /Volumes/Shuttle-01/CARD_A001/ascmhl/0002_CARD_A001_2026-09-15_101640.mhl
2026-09-15T10:16:40.010Z finished, all ok
```

Les états des fichiers correspondent aux états de la liste des fichiers. La page
[Pendant l'exécution d'une tâche](running-a-job.md) les répertorie. Une ligne
`progress` apparaît environ dix fois par seconde pendant la copie. ## Quand
l'envoyer

Un rapport d'incident est plus utile lorsqu'il est accompagné du rapport
enregistré et de ce journal. Ensemble, ils contiennent le plan, le résultat et
l'ordre dans lequel les événements se sont produits.
