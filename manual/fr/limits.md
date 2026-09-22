# Limitations

Cette page indique ce que cette version ne fait pas et ce qu'une vérification réussie ne prouve pas. Lisez-la avant
de vous fier à cette application pour une opération importante.

## Ce qu'une vérification réussie ne prouve pas

Une vérification réussie prouve que les fichiers correspondent aux manifestes. Elle ne prouve pas qu'une
copie antérieure était complète. Une copie interrompue avant la lecture d'un fichier ne contient pas de hachage pour ce fichier dans
le manifeste ; par conséquent, une vérification ultérieure ne détectera aucune absence.

## Chemins d'accès longs sous Windows

Windows limite la longueur d'un chemin d'accès à 260 caractères pour les appels effectués par cette application. Une
source multimédia comportant des dossiers imbriqués en profondeur et des noms de fichiers longs peut dépasser cette limite ;
le traitement du fichier échoue alors en générant une erreur qui le désigne nommément. Placez la destination
près de la racine du lecteur pour rester en dessous de cette limite.

Linux et macOS ne présentent pas cette limitation.

## Volume refusant la lecture sans mise en cache (unbuffered read)

Sous Windows, MediaCopy 3000 relit chaque copie en désactivant le cache. Un volume refusant ce
contournement est lu via le cache, comme d'habitude, quel que soit le système.

Sous macOS, ce contournement est une règle appliquée au descripteur de lecture. Des pages encore en mémoire
suite à l'écriture peuvent répondre à la demande de lecture. Cette version n'a pas été testée sur Mac.

## Le hachage de chaîne est écrit, mais non vérifié

La chaîne contient un hachage `c4` pour chaque manifeste, permettant ainsi de déterminer si un manifeste a été modifié
après son écriture. MediaCopy 3000 écrit ce hachage et le renseigne pour les entrées plus anciennes. Cependant,
elle ne compare pas le manifeste à ce hachage lors de la lecture de l'historique.

## Un échec du scellement peut se cacher derrière une bonne copie

Une tâche qui effectue une opération de scellement (seal) avant la copie parcourt deux fois les mêmes fichiers.
Un fichier qui échoue lors de l'un des deux passages est compté une seule fois dans le compteur d'échecs.
La liste des fichiers affiche le dernier état de chaque fichier. Si le scellement ne peut pas lire un fichier et que la copie le lit ensuite correctement, la tâche signale un échec que la liste des fichiers ne montre pas.
Le manifeste du scellement ne contient aucune entrée pour ce fichier.

## Une action inconnue de ce lecteur est classée comme `original`

Le schéma ASC MHL rend l'attribut `action` facultatif. Un manifeste qui omet cette information est interprété comme `original`. Un manifeste contenant une instruction d'action inconnue de ce lecteur est également interprété comme `original` et réécrit sous cette forme. Une action inconnue est ainsi convertie en `original` plutôt que rejetée.

## Non inclus dans cette version

| Fonctionnalité absente | Ce que cela implique pour vous |
|---|---|
| Détection automatique des périphériques | Vous devez sélectionner manuellement le dossier source. Rien ne s'affiche lors de l'insertion d'une carte ou du branchement d'un disque. |
| Un seul format de hachage par tâche | Une tâche ne peut pas générer deux formats simultanément. |
| Une seule tâche à la fois | Les tâches sont mises en file d'attente ; elles ne s'exécutent pas en parallèle. |
| Absence de champs pour l'auteur | Le manifeste enregistre l'ordinateur, l'outil et sa version, mais aucune information sur une personne. |
| Thème non mémorisé | À chaque lancement, le logiciel revient aux paramètres « Suivre le bureau », « Système (clair) » ou « Système (sombre) ». |
| Ajout de données ≠ Reprise | Une destination contenant déjà une génération terminée est refusée. Transférez les données vers un nouveau dossier ou vérifiez la destination. |

## Copie partielle depuis une autre carte

Lorsque le support source ne possède aucun historique, le programme ne peut pas distinguer deux cartes ayant des fichiers de mêmes noms et tailles. La tâche détecte la différence lors du hachage de chaque fichier réutilisé et copie à nouveau le fichier. Ce dernier est alors marqué comme « Remplacé suite à une discordance ».

## Où signaler un problème

Signalez tout problème dans le dépôt du projet. Un rapport est d'autant plus utile s'il inclut le rapport de tâche enregistré ainsi que le journal des événements. La page [Journal des événements](event-log.md) indique où trouver ce journal.
