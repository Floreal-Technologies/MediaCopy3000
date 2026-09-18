# Vérifier un dossier

Une tâche de vérification relit un dossier et compare chaque fichier à l'historique du dossier lui-même. Elle écrit
une nouvelle génération qui consigne le résultat.

![Un dossier vérifié avec succès](../en/images/verify-history.png)

## Lancer une vérification

1. Cliquez sur **Verify Folder…** (Vérifier le dossier…) ou appuyez sur <kbd>Ctrl</kbd>+<kbd>O</kbd>.
2. Sélectionnez le dossier.
3. Lisez le plan, puis cliquez sur **Start** (Démarrer).

Choisissez le dossier qui contient le dossier `ascmhl`. Pour une destination issue d'un transfert (offload),
il s'agit du dossier portant le nom de la source multimédia, et non du dossier de destination qui le contient.

## Prérequis

Une tâche de vérification nécessite un historique. Un dossier dépourvu de dossier `ascmhl` déclenche l'erreur bloquante
`folder has no history` (le dossier n'a pas d'historique) et la tâche ne peut pas démarrer. Scellez plutôt le dossier. La page
[Sceller une source multimédia](seal.md) traite ce sujet.

## Format de hachage utilisé

Une tâche de vérification ne choisit pas son format de hachage. Elle utilise le format du manifeste de référence ;
ainsi, un dossier enregistré en `md5` est relu en `md5`. MediaCopy 3000 prend en charge les formats `md5`, `sha1` et
`xxh64`.

## Signification du résultat

| Résultat | Ce qu'il prouve |
|---|---|
| `Finished · N files · all OK` | Tous les fichiers listés dans le manifeste sont présents et leur empreinte numérique correspond à celle indiquée dans le manifeste. |
| `Finished · N failures` | Un fichier ne correspond pas, n'a pas pu être lu ou est absent. La liste détaille chaque cas. |

Une vérification réussie prouve que les fichiers correspondent aux
manifestes. Elle ne prouve pas que la copie était complète au moment où elle
a été effectuée. Un fichier qui n'a jamais été copié ne figure pas dans le
manifeste ; il n'y a donc rien que la vérification puisse omettre. La page
[Limites](limits.md) apporte plus de précisions à ce sujet.

## Écriture des données

Une tâche de vérification ajoute une génération à l'historique du dossier.
Cette génération consigne le processus `in-place` (sur place), car aucun
fichier n'a été déplacé. La spécification ne prévoit pas de processus `verify`
(vérification) distinct. Les échecs sont également consignés. Une génération
qui enregistre ses échecs fait partie de l'histoire au même titre que n'importe
quelle autre, et le compteur de l'historique affiche leur nombre en rouge.
