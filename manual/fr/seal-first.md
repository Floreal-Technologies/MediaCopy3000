# Sceller d'abord la source multimédia

Le groupe **Avant la copie** de la fiche de plan contient deux options : Sceller
**d'abord la source multimédia** et **Copier malgré tout si le scellement
**détecte un problème**.

Une opération de scellement calcule les empreintes numériques (hash) de la
source multimédia telle qu'elle se trouve et enregistre chaque fichier comme
`original`. L'opération de transfert compare ensuite chaque copie à cet
enregistrement. La tâche marque comme `vérifiée` toute copie conforme. Si
la source multimédia ne correspond plus à son scellement, la tâche marque le
fichier comme `échec`.

![Les deux options sur la fiche de plan](../en/images/plan-ready.png)

## Sceller d'abord la source multimédia

| Option | Action de la tâche |
|---|---|
| Activée | Lit toute la source multimédia et enregistre ses empreintes. Puis effectue la copie. |
| Désactivée (et la source contient un historique) | Compare les copies aux générations déjà présentes. |
| Désactivée (et la source ne contient aucun historique) | Copie sans point de comparaison. La tâche enregistre chaque fichier comme `original`. |

L'activation de cette option implique une lecture complète de la source multimédia. Le sous-titre de la ligne indique le volume de données (en octets) ainsi que la génération qui sera inscrite par le scellement.

Activez-la pour une source multimédia provenant directement d'une caméra et ne contenant aucun historique. Laissez-la désactivée pour une source déjà scellée, car un second scellement n'apporterait aucune information nouvelle. Dans ce cas, le plan affiche un avertissement indiquant que le dossier est déjà scellé.

Toute modification de l'une ou l'autre de ces options entraîne un recalcul du plan pour la tâche en cours. Vous ne pouvez pas valider un plan correspondant à un réglage que vous avez abandonné.

## Copier malgré tout si le scellement détecte un problème

Cette seconde option n'apparaît sur la fiche que lorsque la première est activée. Elle définit le comportement de la tâche si l'étape de scellement elle-même enregistre un échec (par exemple, un fichier que la source multimédia ne peut plus lire).

| Option | Action de la tâche |
|---|---|
| Désactivée | Arrêt avant la copie. La tâche n'écrit aucune donnée vers les destinations. |
| Activée | Copie malgré tout. L'échec reste consigné dans l'enregistrement. |

L'option « désactivé » (off) est le choix sûr, et c'est celui qui est
sélectionné par défaut. La tâche échoue alors avec le message : `the seal failed
for N of M files; nothing was copied` (le scellement a échoué pour N fichiers
sur M ; rien n'a été copié).

Activez cette option lorsque la source est endommagée et qu'une copie partielle
vaut mieux que pas de copie du tout. Le manifeste consigne les fichiers ayant
échoué ; l'historique permet donc de savoir ce qui s'est passé.
