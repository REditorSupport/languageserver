Coverage runs on eight shards with two test workers per runner. The previous
alphabetical split took about 20, 33, 46, and 61 minutes on the same revision
([run 40174060773](https://github.com/REditorSupport/languageserver/actions/runs/40174060773)).

`coverage-shards.R` assigns the longest measured test files to the shard with
the lowest total duration. Exact, escaped test filters select every current
test file once. New files receive the median duration of existing files until
they are measured; removed files are ignored. Assignment is deterministic,
including when durations tie.

`coverage-timings.csv` seeds the plan with local coverage measurements using R
4.6.1 and two workers. The metadata suite uses a measurement without optional
Polars, matching the reference CI run. Local durations are weights, not
predictions of GitHub runner wall time. Each CI shard uploads a
`coverage-timings-shard-N` artifact containing
file durations measured during instrumented execution, including teardown.
The reporter streams parallel events so timings measure execution rather than
the buffered replay used by the default check reporter. Completed-file timings
are retained even when tests fail.

To refresh the weights from a successful CI run, download its timing artifacts
into a directory, then run this from the repository root (replace the directory):

```r
paths <- list.files("/tmp/coverage-timings", pattern = "\\.csv$",
    recursive = TRUE, full.names = TRUE)
measured <- do.call(rbind, lapply(paths, read.csv))
stopifnot(nrow(measured) > 0, all(is.finite(measured$seconds)),
    all(measured$seconds >= 0))
timings <- aggregate(seconds ~ file, measured, median)
current <- list.files("tests/testthat", pattern = "^test.*\\.[Rr]$")
stopifnot(setequal(timings$file, current))
timings <- timings[order(timings$file, method = "radix"), ]
write.csv(timings, ".github/coverage-timings.csv", row.names = FALSE)
```

Preview a shard with `Rscript .github/coverage-shards.R 8 1`. Each CI job checks
the complete partition before selecting its files. Validate the partition and
timing reporter locally with `Rscript .github/check-coverage-shards.R`.
The full local check requires the package and test dependencies to be installed.
Commit refreshed weights so jobs in a run use the same plan.

Additional shards cannot divide an individual test file. If an uploaded timing
shows one file dominating a shard, split that file into independent test files
before increasing the shard count further. The original completion suite,
which started 41 servers, is split into basic completion, arguments, resolve,
and metadata suites so those groups can execute on different shards.
