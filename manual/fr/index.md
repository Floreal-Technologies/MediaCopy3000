# Manuel

MediaCopy 3000 (MC3K) est une application de transfers de medias, concentrée sur
**l'intégrité** et la **complétion** des transfers des volumes de stockage.

![L'application avec un plan prêt](../en/images/plan-ready.png)

![L'application avec toutes les tâches finies](../en/images/job-finished.png)

## Glossaire

Voici les termes techniques utilisés dans le manuel :

<dl>
  <dt>Source de médias</dt>
  <dd>La source des médias concernés par une tâche: une carte SD,
      un SSD, ou n'importe-quel dossier. MediaCopy 3000 n'y écrit que pour la sceller,
      et écrire les sommes de contrôle dans le dossier <code>ascmhl</code>.
  </dd>
  <dt>Destination</dt>
  <dd>Le dossier de déchargement des médias.</dd>
  <dt>Manifeste</dt>
  <dd>Un fichier <code>.mhl</code> qui possède une somme de contrôle pour chaque fichier dans son arborescence.</dd>
  <dt>Génération</dt>
  <dd>Un manifeste dans l'historique d'un dossier, numéroté à partir de 1.</dd>
  <dt>Chaine</dt>
  <dd>Le fichier qui contiens toutes les générations et leur somme de contrôle.</dd>
  <dt>Sceau</dt>
  <dd>L'enregistrement des sommes de contrôle d'une source de médias.</dd>
  <dt>Déchargement</dt>
  <dd>
    La copie depuis une source de médias vers une ou plusieurs destinations, ainsi que
    la vérification de chaque copie. Chaque destination porte en elle l'historique de la source.
  </dd>
  <dt>Vérification</dt>
  <dd>La lecture et vérification d'un dossier et de son historique.</dd>
  <dt>Le plan</dt>
  <dd>Ce qu'une tâche a prévu de faire.</dd>
  <dt>Constats</dt>
  <dd>Les bloqueurs ou avertissements que le plan aura trouvé avant l'exécution.</dd>
</dl>
