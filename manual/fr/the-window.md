# La fenêtre principale

![La fenêtre avant la première tâche](../en/images/empty.png)

La fenêtre princpale a trois parties: la barre d'en-tête, la liste des tâches et le paneau.

## La barre d'en-tête

Les trois boutons situés à gauche servent pour trois types de tâche : 

| Bouton | Tâche |
|---|---|
| **New Offload…** | Un déchargement. La page [Décharger des médias](offload.md) l'explique. |
| **Verify Folder…** | Une vérification. La page [Vérifier un dossier](verify.md) l'explique. |
| **Seal Media…** | Un scellement. la page [Sceller une source de médias](seal.md) l'explique. |

Le bouton de menu est à droite, qui s'ouvre avec la touche <kbd>F10</kbd>.

## La liste des tâches

![La liste avec plusieurs tâches](../en/images/queue.png)

La liste sur la gauche contiens les tâches, une par rangée, leur phase et la barre de progression.

| The row says | What it means |
|---|---|
| `Queued` | La tâche est en attente de traitement. Une tâche est executée à la fois. |
| `Needs review` | La tâche a été reprogrammée et le plan a changé. Il attent patiemment une nouvelle approbation. |
| `Copying · 42 % · 310 MB/s` | Un déchargement est en cours, avec sa progression et son taux de transfer  |
| `Writing the manifest · 25 s` | La tâche a a lu ou écrit tous les fichiers, et est en train d'écrire le manifeste. Le temps écoulé est celui passé à attendre le disque. |
| `Saving to disk · 14 s` | La tâche n'a transféré aucune donnée depuis quelques secondes. Les termes indiquent l'état du fichier en cours de traitement, et le nombre correspond à la durée écoulée. La page [Exécution d'une tâche](running-a-job.md) fournit des explications à ce sujet. |
| `Verifying…` | Une tâche de vérification. |
| `Sealing…` | Une tâche de scellement. |
| `Finished · 7 files · all OK` | Tous les fichiers sont OK. |
| `Finished · 2 failures` | Certains fichiers ne sont pas OK. |
| `Failed` | La tâche s'est arrêtée avant son terme. Un message au bas de la fenêtre en indique la raison, et le rapport la conserve. |
| `Cancelled` | La tâche a été annulée. |

Cliquez sur une ligne pour sélectionner la tâche. <kbd>Ctrl</kbd>+<kbd>Page ⇟</kbd>
(Page suivante) et <kbd>Ctrl</kbd>+<kbd>Page ⇞</kbd> (Page précédente) permettent de
déplacer la sélection.
La page [Raccourcis clavier](keyboard.md) liste toutes les touches.

## Le volet Détails

La vue centrale affiche la tâche sélectionnée.
La page [Exécution d'une tâche](running-a-job.md) explique chaque partie du volet.
