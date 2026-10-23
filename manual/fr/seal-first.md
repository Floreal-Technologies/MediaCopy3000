# Sceller d’abord la source multimédia

Le groupe **Before Copying** de la fiche du plan contient deux options : **Seal the
media source first** et **Copy anyway if the seal finds a problem**.

Un scellement calcule les empreintes (*hash*) de la source multimédia telle
qu’elle se trouve et enregistre chaque fichier comme `original`. Le
déchargement compare ensuite chaque copie à cet enregistrement. La tâche
enregistre comme `verified` toute copie conforme. Si la source multimédia ne
correspond plus à son sceau, la tâche enregistre le fichier comme `failed`.

![Les deux options sur la fiche du plan](../en/images/plan-ready.png)

## Sceller d’abord la source multimédia

| Option | Action de la tâche |
|---|---|
| Activée | Lit toute la source multimédia et enregistre ses empreintes, puis effectue la copie. |
| Désactivée (et la source contient un historique) | Compare les copies aux générations déjà présentes. |
| Désactivée (et la source ne contient aucun historique) | Copie sans point de comparaison. La tâche enregistre chaque fichier comme `original`. |

L’activation de l’option **Seal the media source first** implique une lecture complète de la source multimédia. Le sous-titre de la ligne indique le volume de données (en octets) ainsi que la génération que le scellement va écrire.

Activez-la pour une source multimédia qui vient directement d’une caméra et ne contient aucun historique. Laissez-la désactivée pour une source déjà scellée, car un second scellement n’apporterait aucune information nouvelle. Dans ce cas, le plan affiche un avertissement indiquant que le dossier est déjà scellé.

Toute modification de l’une ou l’autre de ces options entraîne un nouveau calcul du plan pour la tâche en cours. Vous ne pouvez pas valider un plan qui correspond à un réglage que vous avez abandonné.

## Copier malgré tout si le scellement détecte un problème

L’option **Copy anyway if the seal finds a problem** n’apparaît sur la fiche que lorsque la première est activée. Elle définit le comportement de la tâche si le scellement lui-même enregistre un échec (par exemple, un fichier que la source multimédia ne peut plus lire).

| Option | Action de la tâche |
|---|---|
| Désactivée | Arrêt avant la copie. La tâche n’écrit aucune donnée vers les destinations. |
| Activée | Copie malgré tout. L’échec reste consigné dans l’enregistrement. |

L’option désactivée est le choix sûr, et c’est le réglage par défaut. La tâche
échoue alors avec le message
`the seal failed for N of M files; nothing was copied`.

Activez cette option lorsque la source est endommagée et qu’une copie partielle
vaut mieux que pas de copie du tout. Le manifeste consigne les fichiers en
échec ; l’historique permet donc de savoir ce qui s’est passé.
