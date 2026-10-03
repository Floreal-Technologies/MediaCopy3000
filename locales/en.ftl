## Results

result-all-verified = finished, all files verified
result-all-sealed = finished, all files sealed
result-with-failures = finished with { $count ->
     [one] { $count } failure
     *[other] { $count } failures
}

## Toasts

toast-no-history = no ASC Media Hash List history (ascmhl/) in { $folder }

## Durations

duration-seconds = { $seconds } s
duration-minutes = { $minutes } min { $seconds } s
duration-hours = { $hours } h { $minutes } min

## Activity

quiet-doing = { $doing } · { $elapsed }
running-copying = Copying
running-verifying = Verifying
running-sealing = Sealing
doing-writing-manifest = Writing the manifest

## File status

file-status-pending = Pending
file-status-hashing = Hashing
file-status-copying = Copying
file-status-flushing = Saving to disk
file-status-publishing = Naming the copy
file-status-verifying = Verifying
file-status-verified = Verified
file-status-hash-mismatch = Hash mismatch
file-status-missing = Missing
file-status-new = New (not in manifest)
file-status-io-error = I/O error: { $message }
file-status-replaced = Replaced after mismatch

## Job kinds

job-kind-offload = offload
job-kind-verify = verify
job-kind-seal = seal

## Plan

write-mode-copy = copy
write-mode-overwrite = overwrite
write-mode-reuse = reuse
process-transfer = transfer
process-in-place = in-place
process-flatten = flatten
target-empty = empty
target-not-empty = not empty
target-absent = will be created
target-partial = partial copy

## Findings

finding-source-missing = source not found
finding-source-empty = source is empty
finding-destination-partial = destination holds a partial copy
finding-destination-foreign = destination holds files that are not on the media source
finding-destination-other-source = destination was copied from another media source
finding-destination-unavailable = destination cannot be read
finding-destination-history-unreadable = destination history cannot be read
finding-insufficient-space = not enough space
finding-format-unsettled = hash format cannot be settled
finding-chain-names-no-manifest = chain names no manifest
finding-chain-unreadable = chain cannot be read
finding-manifest-unreadable = a manifest the chain names cannot be read
finding-no-seal = folder has no history
finding-already-sealed = folder is already sealed

## Theme

theme-section-system = System
theme-system-light = System light
theme-system-dark = System dark
theme-base-follow-desktop = Follow desktop
theme-base-always-light = Always light
theme-base-always-dark = Always dark

## Job toasts & dialogs

toast-job-finished = { $label }: { $result }
toast-job-failed = { $label }: failed – { $message }
dialog-save-plan = Save Plan
dialog-save-report = Save Report
