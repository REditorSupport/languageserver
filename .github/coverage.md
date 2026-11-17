Coverage tests use covr >= 4.0.0 on four standard Ubuntu runners, with two test
workers per runner. Each test starts a language server with its own worker
pools, so testthat does not occupy all four runner cores. Four shards reduce
repeated package setup while retaining parallelism; the timing and resource
artifacts below support further tuning after observing CI results.

[covr 4.0.0](https://covr.r-lib.org/news/index.html#covr-400) instruments and
byte-compiles the package once after installation, counts expressions in C,
and streams compact subprocess counts against shared counter metadata. Its
native trace merger replaces our previous trace pre-merge workaround.
[covr #744](https://github.com/r-lib/covr/issues/744) investigated
languageserver's unusually expensive coverage runs. The release's 1 second /
144 MB result measures trace merging, not the full test suite; it does not
establish that one unsharded runner is sufficient.

Each shard saves its coverage object as `coverage.rds`. A final job merges the
expression counts with covr's public `c()` method before exporting one
Cobertura report and uploading it to Codecov. This matters because 4.0.0 only
marks a line covered when all its expressions ran: if two shards exercise
different branches on the same line, both individual XML reports can show
zero hits even though their combined coverage covers the line. The merge job
only installs covr and xml2, and checks this case with
`Rscript .github/check-coverage-merge.R`.

Coverage requires R >= 4.1.0 because of covr's new requirement. covr remains an
optional dependency; languageserver's runtime R requirement stays >= 4.0.0.
Experimental branch coverage and test recording are not enabled, preserving
covr's compact-count path. Keep existing graceful subprocess shutdown so
workers can save their traces, and investigate unexpected `covr_unsaved_trace`
warnings rather than overlooking missing worker coverage.

Prefer binary dependencies and use `R_KEEP_PKG_SOURCE=no` for dependency source
installations. Released 4.0.0 can repeatedly parse source-preserved R6 methods
([covr #768](https://github.com/r-lib/covr/issues/768)); the fix is after the
release. covr installs languageserver with source references independently, so
this setting does not remove its coverage source.

`coverage-shards.R` assigns the longest measured test files to the shard with
the lowest total duration. Exact, escaped test filters select every current
test file once. New files receive the median duration of existing files until
they are measured; removed files are ignored. Assignment is deterministic,
including when durations tie.

`coverage-timings.csv` uses measured file durations from successful
[covr 4.0.0 run 44010485204](https://github.com/REditorSupport/languageserver/actions/runs/44010485204),
with R 4.6.2, four-core Ubuntu runners, and two test workers per runner.
These durations are scheduling weights, not predictions of runner wall time.
Refresh them from successful CI runs rather than scaling by an assumed
speedup. Each CI shard uploads a
`coverage-timings-shard-N` artifact containing
file durations measured during instrumented execution, including teardown,
the coverage object and diagnostic Cobertura report, a version/time/coverage
summary, and GNU time resource
measurements. The maximum RSS is a peak-process measurement, not the sum of
all concurrently running worker processes. The summary also appears in the
GitHub Actions job summary.
The `combined-coverage` artifact holds the final report and its coverage
summary after all shards succeed.
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

Preview a shard with `Rscript .github/coverage-shards.R 4 1`. Each CI job checks
the complete partition for 1, 2, 4, and 8 shards before selecting its files.
Validate the partition and timing reporter locally with
`Rscript .github/check-coverage-shards.R`.
The full local check requires the package and test dependencies to be installed.
Commit refreshed weights so jobs in a run use the same plan.

Run coverage locally with `TESTTHAT_CPUS=2 Rscript .github/run-coverage.R`.
Set `RUNNER_TEMP` to an existing output directory and `TESTTHAT_TIMINGS` to a
CSV path to retain the report, installation, and file timings. Otherwise the
report and installation use R's temporary directory.

Additional shards cannot divide an individual test file. If an uploaded timing
shows one file dominating a shard, split that file into independent test files
before increasing the shard count further. The original completion suite,
which started 41 servers, is split into basic completion, arguments, resolve,
and metadata suites so those groups can execute on different shards.
