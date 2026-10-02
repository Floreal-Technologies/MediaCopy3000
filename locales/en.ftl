## Results

result-all-verified = finished, all files verified
result-all-sealed = finished, all files sealed
result-with-failures = finished with { $count ->
     [one] { $count } failure
     *[other] { $count } failures
}

## Toasts

toast-no-history = no ASC Media Hash List history (ascmhl/) in { $folder }
