## Results

result-all-verified = terminé, tous les fichiers sont vérifiés
result-all-sealed = terminé, tous les fichiers sont scellés
result-with-failures = terminé avec { $count ->
     [one] { $count } échec
    *[other] { $count } échecs
}

## Toasts

toast-no-history = pas d’historique ASC Media Hash List (ascmhl/) dans { $folder }

## Durations

duration-seconds = { $seconds } s
duration-minutes = { $minutes } min { $seconds } s
duration-hours = { $hours } h { $minutes } min

## Activity

quiet-doing = { $doing } · { $elapsed }
running-copying = Copie
running-verifying = Vérification
running-sealing = Scellement
doing-writing-manifest = Écriture du manifeste

## File status

file-status-pending = En attente
file-status-hashing = Calcul de l’empreinte
file-status-copying = Copie
file-status-flushing = Écriture sur le disque
file-status-publishing = Nommage de la copie
file-status-verifying = Vérification
file-status-verified = Vérifié
file-status-hash-mismatch = Empreinte différente
file-status-missing = Manquant
file-status-new = Nouveau (absent du manifeste)
file-status-io-error = Erreur d’E/S : { $message }
file-status-replaced = Remplacé après une différence

## Job kinds

job-kind-offload = déchargement
job-kind-verify = vérification
job-kind-seal = scellement

## Plan

write-mode-copy = copie
write-mode-overwrite = écrasement
write-mode-reuse = réutilisation
process-transfer = transfert
process-in-place = sur place
process-flatten = aplatissement
target-empty = vide
target-not-empty = non vide
target-absent = sera créé
target-partial = copie partielle

## Findings

finding-source-missing = source introuvable
finding-source-empty = la source est vide
finding-destination-partial = la destination contient une copie partielle
finding-destination-foreign = la destination contient des fichiers absents de la source
finding-destination-other-source = la destination a été copiée depuis une autre source
finding-destination-unavailable = la destination est illisible
finding-destination-history-unreadable = l’historique de la destination est illisible
finding-insufficient-space = espace insuffisant
finding-format-unsettled = le format d’empreinte ne peut pas être fixé
finding-chain-names-no-manifest = la chaîne ne nomme aucun manifeste
finding-chain-unreadable = la chaîne est illisible
finding-manifest-unreadable = un manifeste nommé par la chaîne est illisible
finding-no-seal = le dossier n’a pas d’historique
finding-already-sealed = le dossier est déjà scellé

## Theme

theme-section-system = Système
theme-system-light = Système clair
theme-system-dark = Système sombre
theme-base-follow-desktop = Suivre le bureau
theme-base-always-light = Toujours clair
theme-base-always-dark = Toujours sombre

## Job toasts & dialogs

toast-job-finished = { $label } : { $result }
toast-job-failed = { $label } : échec – { $message }
dialog-save-plan = Enregistrer le plan
dialog-save-report = Enregistrer le rapport

## Plug-ins

plugin-finding = { $plugin } : { $title }
plugin-unavailable = extension indisponible
plugin-field-missing = le champ { $field } n’a pas de valeur valide
plugin-bad-output = l’extension a envoyé une réponse invalide
toast-plugin = { $label } : { $message }
