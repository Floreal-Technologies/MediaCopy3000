# Rapports

Un rapport est un fichier texte simple qui consigne les actions effectuées par
une tâche. Il est destiné à la personne qui devra expliquer, plusieurs semaines
plus tard, ce qui est arrivé à une source multimédia.

## Enregistrer un rapport

1. Sélectionnez une tâche terminée.
2. Cliquez sur **Enregistrer le rapport…** ou appuyez sur <kbd>Ctrl</kbd>+<kbd>S</kbd>.
3. Choisissez l'emplacement du fichier.

Le nom suggéré est `<nom du dossier>-report.txt`.

Ce bouton ne fonctionne que pour une tâche terminée, ayant échoué ou ayant été annulée. Une tâche en cours ne produit aucun résultat à consigner.

## Contenu du rapport

```
MediaCopy 3000 report
Job: offload
Source: /media/CARD_A001
Destination: /Volumes/Shuttle-01/2026-09-12
Destination: /Volumes/Archive-A/2026-09-12
Created: 2026-09-12T14:03:00+00:00
Plan
  files: 7
  bytes: 17260697327
  originals: the seal this job takes first
  seal first: 7 files, 17260697327 bytes, on failure: stop before copying
  generation: /Volumes/Shuttle-01/2026-09-12 (3, 0003_CARD_A001_2026-09-12_140300.mhl)
  generation: /Volumes/Archive-A/2026-09-12 (1, 0001_CARD_A001_2026-09-12_140300.mhl)
  target: /Volumes/Shuttle-01/2026-09-12 (empty)
  target: /Volumes/Archive-A/2026-09-12 (will be created)
  warning: folder is already sealed – generation 2
Result: finished, all files verified
Files: 7 total, 7 verified, 0 hash mismatch/io error, 0 missing, 0 new, 0 replaced
Bytes: 17260697327
Manifest: /Volumes/Shuttle-01/2026-09-12/CARD_A001/ascmhl/0003_CARD_A001_2026-09-12_140300.mhl
Manifest: /Volumes/Archive-A/2026-09-12/CARD_A001/ascmhl/0001_CARD_A001_2026-09-12_140300.mhl
Originals: the media source's own history, 7 files
Log: /home/you/.local/state/mediacopy3000/jobs/2026-09-12_140300-1-CARD_A001-offload.log
```

| Section | Utilité |
|---|---|
| `Job`, `Source`, `Destination`, `Folder` | Tâche concernée et dossiers impliqués |
| `Created` | Date de création de la tâche |
| `Plan` | Le plan que vous avez approuvé, avec ses conclusions, mot pour mot. Un transfert vers une destination partielle ajoute la mention `existing copy: resume` (reprise) ou `replace` (remplacement). |
| `Result` | Issue de la tâche |
| `Files`, `Bytes` | Compteurs issus du volet de détails |
| `Manifest` | Tous les manifestes générés par la tâche |
| `Originals` | Éléments de référence utilisés pour la vérification |
| `Log` | Journal des événements de la tâche. La page [Journal des événements](event-log.md) fournit des explications à ce sujet. |

Une tâche ayant rencontré des échecs comporte une section `Failures:`. Une ligne est indiquée pour chaque fichier problématique :

```
Failures:
FAILED  A001C002_260912_R1AB.mov  hash mismatch expected xxh64 4f9a1c3b2d7e8051 actual xxh64 8c21b40fa9e3d517
FAILED  A001C004_260912_R1AB.mov  io error input/output error
MISSING Sidecar/A001C001.wav
```

Une opération de vérification ou de scellement (*seal*) inclut également une section `History:`, avec une ligne pour chaque génération.

## Pourquoi le plan y figure

Le bloc `Plan` est l'élément qui transforme le rapport en une piste d'audit.
Il indique ce que l'application a signalé avant que vous ne donniez votre
approbation, y compris tous les avertissements que vous avez choisi d'accepter.
Le rapport et la fiche du plan ne peuvent pas diverger, car ils se basent sur le
même plan.

## Le rapport n'est pas l'enregistrement de référence

C'est le manifeste qui constitue l'enregistrement de référence. Il accompagne
les fichiers et perdure au-delà de cette application. Le rapport n'est qu'une
note relative à une exécution spécifique.
