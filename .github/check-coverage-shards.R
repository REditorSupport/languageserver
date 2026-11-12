source(".github/coverage-shards.R")
source(".github/merge-covr-traces.R")

files <- paste0("test-", letters, ".R")
timings <- data.frame(file = files, seconds = seq_along(files)^2)
plan <- coverage_shards(files, timings, 8)
stopifnot(setequal(plan$file, files), !anyDuplicated(plan$file),
    setequal(plan$shard, 1:8),
    identical(plan, coverage_shards(rev(files), timings[26:1, ], 8)))

new_files <- c(files[-1], "test-new.R")
updated <- coverage_shards(new_files, timings, 8)
stopifnot(setequal(updated$file, new_files),
    updated$seconds[updated$file == "test-new.R"] == median(timings$seconds[-1]))
fallback <- coverage_shards(files, timings[FALSE, ], 8)
stopifnot(all(fallback$seconds == 1), setequal(fallback$shard, 1:8))

invalid <- timings
invalid$seconds[1] <- NA_real_
stopifnot(inherits(try(coverage_shards(files, invalid, 8), silent = TRUE), "try-error"),
    inherits(try(coverage_shards(files, timings[c(1, 1), ], 8), silent = TRUE), "try-error"),
    inherits(try(coverage_shards(files, timings, 0), silent = TRUE), "try-error"),
    inherits(try(coverage_shards(c("test-name.R", "test_name.R"), timings[FALSE, ], 2),
            silent = TRUE), "try-error"))

files <- c("test-completion.R", "test-completion-typing.R", "test-a+b.R", "test-a.b.R",
    "test_under.R", "testbare.R")
contexts <- sub("\\.R$", "", sub("^test[-_]", "", files))
stopifnot(identical(grepl(coverage_filter(files[c(1, 3, 5, 6)]), contexts),
        c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)))

files <- list.files("tests/testthat", pattern = "^test.*\\.[Rr]$")
timings <- read.csv(".github/coverage-timings.csv")
plan <- coverage_shards(files, timings, 8)
matches <- vapply(1:8, function(shard) {
    selected <- plan$file[plan$shard == shard]
    filter <- coverage_filter(selected)
    actual <- testthat:::find_test_scripts("tests/testthat", filter = filter,
        full.names = FALSE)
    stopifnot(setequal(actual, selected))
    grepl(filter,
        sub("\\.[Rr]$", "", sub("^test[-_]", "", files)))
}, logical(length(files)))
stopifnot(all(rowSums(matches) == 1L), all(colSums(matches) > 0L))
cat("All", length(files), "test files occur in exactly one of eight shards.\n")

desc_start_first <- read.dcf("DESCRIPTION", fields = "Config/testthat/start-first")[1L, 1L]
start_first <- trimws(strsplit(desc_start_first, ",", fixed = TRUE)[[1L]])
file_contexts <- sub("\\.[Rr]$", "", sub("^test[-_]", "", files))
stopifnot(length(start_first) > 0L, !anyDuplicated(start_first),
    all(start_first %in% file_contexts))
ordered <- testthat:::order_test_scripts(files, start_first)
ordered_contexts <- sub("\\.[Rr]$", "", sub("^test[-_]", "", head(ordered, length(start_first))))
stopifnot(identical(ordered_contexts, start_first))

# Verify streaming O(N) covr trace pre-merge matches covr:::merge_coverage().
make_trace_env <- function(entries) {
    env <- new.env(hash = TRUE, parent = emptyenv())
    for (nm in names(entries)) {
        env[[nm]] <- entries[[nm]]
    }
    env
}
make_counter <- function(val, fn = "fn", line = 1L, tests = NULL) {
    out <- list(
        value = as.numeric(val),
        srcref = structure(c(line, 1L, line, 10L, 1L, 10L, line, line), class = "srcref"),
        functions = fn
    )
    if (!is.null(tests)) {
        out$tests <- tests
    }
    out
}

stopifnot(identical(merge_covr_traces(tempfile("missing-trace-dir-")), character()))
empty_trace_dir <- tempfile("empty-trace-dir-")
dir.create(empty_trace_dir)
stopifnot(identical(merge_covr_traces(empty_trace_dir), character()))
unlink(empty_trace_dir, recursive = TRUE)

single_trace_dir <- tempfile("single-trace-dir-")
dir.create(single_trace_dir)
single_file <- file.path(single_trace_dir, "covr_trace_one")
saveRDS(make_trace_env(list("a.R:1:1:1:5:1:5:1:1" = make_counter(2, "fa", 1L))), single_file)
stopifnot(identical(merge_covr_traces(single_trace_dir), single_file),
    file.exists(single_file))
unlink(single_trace_dir, recursive = TRUE)

multi_trace_dir <- tempfile("multi-trace-dir-")
dir.create(multi_trace_dir)
f1 <- file.path(multi_trace_dir, "covr_trace_1")
f2 <- file.path(multi_trace_dir, "covr_trace_2")
f3 <- file.path(multi_trace_dir, "covr_trace_3")
e1 <- list(
    "a.R:1:1:1:5:1:5:1:1" = make_counter(3, "fa", 1L),
    "b.R:2:1:2:5:1:5:2:2" = make_counter(0, "fb", 2L)
)
e2 <- list(
    "a.R:1:1:1:5:1:5:1:1" = make_counter(4, "fa", 1L),
    "c.R:3:1:3:5:1:5:3:3" = make_counter(7, "fc", 3L)
)
e3 <- list(
    "b.R:2:1:2:5:1:5:2:2" = make_counter(5, "fb", 2L),
    "c.R:3:1:3:5:1:5:3:3" = make_counter(1, "fc", 3L),
    "d.R:4:1:4:5:1:5:4:4" = make_counter(0, "fd", 4L)
)
saveRDS(make_trace_env(e1), f1)
saveRDS(make_trace_env(e2), f2)
saveRDS(make_trace_env(e3), f3)

# Must skip cleanly when covr.record_tests is TRUE.
withr::with_options(list(covr.record_tests = TRUE), {
    stopifnot(identical(merge_covr_traces(multi_trace_dir), character()))
})
stopifnot(length(list.files(multi_trace_dir, pattern = "^covr_trace_[^/]+$")) == 3L)

expected_merged <- covr:::merge_coverage(
    list.files(multi_trace_dir, pattern = "^covr_trace_[^/]+$", full.names = TRUE)
)
merged_path <- merge_covr_traces(multi_trace_dir)
remaining_traces <- list.files(multi_trace_dir, pattern = "^covr_trace_[^/]+$", full.names = TRUE)
all_entries <- list.files(multi_trace_dir, all.files = TRUE, no.. = TRUE)
stopifnot(length(remaining_traces) == 1L, identical(remaining_traces, merged_path),
    length(all_entries) == 1L)
actual_merged <- covr:::merge_coverage(remaining_traces)
stopifnot(setequal(names(actual_merged), names(expected_merged)),
    identical(actual_merged[names(expected_merged)], expected_merged))
unlink(multi_trace_dir, recursive = TRUE)

recorded_tests_dir <- tempfile("recorded-tests-dir-")
dir.create(recorded_tests_dir)
rt1 <- file.path(recorded_tests_dir, "covr_trace_rt1")
rt2 <- file.path(recorded_tests_dir, "covr_trace_rt2")
rt2_entries <- list(
    "a.R:1:1:1:5:1:5:1:1" = make_counter(2, "fa", 1L,
        tests = list(tally = matrix(1L, nrow = 1L, ncol = 2L)))
)
saveRDS(make_trace_env(list("a.R:1:1:1:5:1:5:1:1" = make_counter(1, "fa", 1L))), rt1)
saveRDS(make_trace_env(rt2_entries), rt2)
stopifnot(length(merge_covr_traces(recorded_tests_dir)) == 2L,
    length(list.files(recorded_tests_dir, pattern = "^covr_trace_[^/]+$")) == 2L)
unlink(recorded_tests_dir, recursive = TRUE)
cat("Streaming covr trace pre-merge verified against covr:::merge_coverage().\n")

if ("--partition-only" %in% commandArgs(trailingOnly = TRUE)) quit(save = "no")

# Exercise the actual test entry point with parallel workers. Buffered event
# replay would produce near-zero timings and fail these checks.
library(testthat)
fixture <- tempfile("coverage-timing-fixture-")
dir.create(fixture)
writeLines(c("Package: coveragetimingfixture", "Version: 0.0.1",
        "Config/testthat/edition: 3"), file.path(fixture, "DESCRIPTION"))
writeLines('test_that("slow", { Sys.sleep(0.4); expect_true(TRUE) })',
    file.path(fixture, "test-slow.R"))
writeLines('test_that("fast", { Sys.sleep(0.2); withr::defer(Sys.sleep(0.2)); expect_true(TRUE) })',
    file.path(fixture, "test-fast.R"))
timing_env <- c(TESTTHAT_TIMINGS = file.path(fixture, "timings.csv"),
    TESTTHAT_CPUS = "2", TESTTHAT_PARALLEL = "TRUE", TESTTHAT_FILTER = "")
withr::with_envvar(timing_env, {
    env <- new.env(parent = globalenv())
    env$test_check <- function(package, ...) {
        test_dir(fixture, ..., package = "languageserver", load_package = "installed")
    }
    sys.source("tests/testthat.R", env)
    measured <- read.csv(Sys.getenv("TESTTHAT_TIMINGS"))
    stopifnot(nrow(measured) == 2L,
        measured$seconds[measured$file == "test-slow.R"] >= 0.4,
        measured$seconds[measured$file == "test-fast.R"] >= 0.4)
    writeLines('test_that("failure", { expect_true(FALSE) })', file.path(fixture, "test-fast.R"))
    failed <- inherits(try(sys.source("tests/testthat.R", env), silent = TRUE), "try-error")
    stopifnot(failed, nrow(read.csv(Sys.getenv("TESTTHAT_TIMINGS"))) == 2L)
})
unlink(fixture, recursive = TRUE)
cat("Parallel timings and test-failure propagation verified.\n")
