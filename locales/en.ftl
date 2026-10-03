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
