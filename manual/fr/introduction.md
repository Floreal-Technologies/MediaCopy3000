# Introduction

MediaCopy 3000 copie des médias et garde une trace des copies. L'application
est faite pour à la fois les ingénieurs vision (en anglais DIT, _Digital
Imaging Technician_) sur les plateaux de tournage, et les hobbyistes qui veulent
transférer leurs données de manière sécurisée.

MediaCopy 3000 s'interface avec le standard
[ASC Media Hash List](mhl-format.md).
Un manifeste dans ce format guarantie deux choses au sujet d'une copie :
**L'intégrité** (les octets n'ont pas changé) et la **complétude** (tous les
fichiers sont présents). Chaque tâche de transfer écris un manifeste et l'ajoute
à la chaîne du dossier de destination. Cette chaîne constitute l'historique de
vos fichiers.

![La file d'attente des tâches](../en/images/queue.png)
