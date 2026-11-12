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

# (a) Corrupt 0-byte first trace (covr_trace_0000) + 2 valid traces, plus
# half-written gzipped RDS and garbage bytes in various positions.
corrupt_first_dir <- tempfile("corrupt-first-dir-")
dir.create(corrupt_first_dir)
cf0 <- file.path(corrupt_first_dir, "covr_trace_0000")
cf1 <- file.path(corrupt_first_dir, "covr_trace_1")
cf2 <- file.path(corrupt_first_dir, "covr_trace_2")
saveRDS(make_trace_env(e1), cf1)
saveRDS(make_trace_env(e2), cf2)
expected_cf <- covr:::merge_coverage(c(cf1, cf2))
invisible(file.create(cf0))
seen_msgs <- character()
cf_merged <- withCallingHandlers(
    merge_covr_traces(corrupt_first_dir),
    message = function(m) {
        seen_msgs <<- c(seen_msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
    }
)
cf_remaining <- list.files(corrupt_first_dir, pattern = "^covr_trace_[^/]+$", full.names = TRUE)
cf_actual <- covr:::merge_coverage(cf_remaining)
stopifnot(!file.exists(cf0),
    any(grepl("covr_trace_0000", seen_msgs, fixed = TRUE)),
    identical(cf_remaining, cf_merged),
    setequal(names(cf_actual), names(expected_cf)),
    identical(cf_actual[names(expected_cf)], expected_cf))
raw_valid <- readBin(cf_merged, "raw", file.info(cf_merged)$size)
unlink(corrupt_first_dir, recursive = TRUE)

# Half-written gzipped RDS in position 1 of 2 leaves the 1 valid trace intact.
one_of_two_dir <- tempfile("one-of-two-dir-")
dir.create(one_of_two_dir)
half_first <- file.path(one_of_two_dir, "covr_trace_000")
valid_second <- file.path(one_of_two_dir, "covr_trace_1")
writeBin(head(raw_valid, length(raw_valid) %/% 2L), half_first)
saveRDS(make_trace_env(e1), valid_second)
expected_single_valid <- covr:::merge_coverage(valid_second)
res_one_of_two <- suppressMessages(merge_covr_traces(one_of_two_dir, retries = 1L, delay = 0.01))
rem_one_of_two <- list.files(one_of_two_dir, pattern = "^covr_trace_[^/]+$", full.names = TRUE)
actual_one_of_two <- covr:::merge_coverage(rem_one_of_two)
stopifnot(!file.exists(half_first),
    identical(res_one_of_two, valid_second),
    identical(rem_one_of_two, valid_second),
    identical(actual_one_of_two[names(expected_single_valid)], expected_single_valid))
unlink(one_of_two_dir, recursive = TRUE)

# Middle half-written RDS + garbage trace among valid traces + recorded_tests with corrupt traces.
mixed_corrupt_dir <- tempfile("mixed-corrupt-dir-")
dir.create(mixed_corrupt_dir)
mc1 <- file.path(mixed_corrupt_dir, "covr_trace_1")
mc2_half <- file.path(mixed_corrupt_dir, "covr_trace_2")
mc3_garbage <- file.path(mixed_corrupt_dir, "covr_trace_3")
mc4 <- file.path(mixed_corrupt_dir, "covr_trace_4")
saveRDS(make_trace_env(e1), mc1)
saveRDS(make_trace_env(e3), mc4)
expected_mc <- covr:::merge_coverage(c(mc1, mc4))
writeBin(head(raw_valid, length(raw_valid) %/% 3L), mc2_half)
writeBin(charToRaw("not-a-valid-rds-payload"), mc3_garbage)
mc_merged <- suppressMessages(merge_covr_traces(mixed_corrupt_dir, retries = 1L, delay = 0.01))
mc_rem <- list.files(mixed_corrupt_dir, pattern = "^covr_trace_[^/]+$", full.names = TRUE)
mc_actual <- covr:::merge_coverage(mc_rem)
stopifnot(!file.exists(mc2_half), !file.exists(mc3_garbage),
    identical(mc_rem, mc_merged),
    setequal(names(mc_actual), names(expected_mc)),
    identical(mc_actual[names(expected_mc)], expected_mc))
unlink(mixed_corrupt_dir, recursive = TRUE)

rt_corrupt_dir <- tempfile("rt-corrupt-dir-")
dir.create(rt_corrupt_dir)
rtc_bad0 <- file.path(rt_corrupt_dir, "covr_trace_000")
rtc_1 <- file.path(rt_corrupt_dir, "covr_trace_rt1")
rtc_2 <- file.path(rt_corrupt_dir, "covr_trace_rt2")
rtc_bad9 <- file.path(rt_corrupt_dir, "covr_trace_zzz")
writeBin(head(raw_valid, 24L), rtc_bad0)
saveRDS(make_trace_env(list("a.R:1:1:1:5:1:5:1:1" = make_counter(1, "fa", 1L))), rtc_1)
saveRDS(make_trace_env(rt2_entries), rtc_2)
invisible(file.create(rtc_bad9))
rtc_res <- suppressMessages(merge_covr_traces(rt_corrupt_dir, retries = 1L, delay = 0.01))
rtc_rem <- list.files(rt_corrupt_dir, pattern = "^covr_trace_[^/]+$", full.names = TRUE)
rtc_merged_cov <- covr:::merge_coverage(rtc_rem)
stopifnot(!file.exists(rtc_bad0), !file.exists(rtc_bad9),
    identical(rtc_res, c(rtc_1, rtc_2)),
    identical(rtc_rem, c(rtc_1, rtc_2)),
    identical(rtc_merged_cov[["a.R:1:1:1:5:1:5:1:1"]]$value, 3))
unlink(rt_corrupt_dir, recursive = TRUE)

# (b) Truncated single trace file (1 of 1) and all-corrupt traces (2 of 2).
single_corrupt_dir <- tempfile("single-corrupt-dir-")
dir.create(single_corrupt_dir)
single_corrupt_file <- file.path(single_corrupt_dir, "covr_trace_only")
writeBin(head(raw_valid, length(raw_valid) %/% 2L), single_corrupt_file)
res_single_corrupt <- suppressMessages(merge_covr_traces(single_corrupt_dir))
stopifnot(identical(res_single_corrupt, character()),
    !file.exists(single_corrupt_file),
    length(list.files(single_corrupt_dir, pattern = "^covr_trace_[^/]+$")) == 0L,
    is.null(covr:::merge_coverage(
        list.files(single_corrupt_dir, pattern = "^covr_trace_[^/]+$", full.names = TRUE)
    )))
unlink(single_corrupt_dir, recursive = TRUE)

all_corrupt_dir <- tempfile("all-corrupt-dir-")
dir.create(all_corrupt_dir)
invisible(file.create(file.path(all_corrupt_dir, "covr_trace_a")))
writeBin(charToRaw("corrupt"), file.path(all_corrupt_dir, "covr_trace_b"))
res_all_corrupt <- suppressMessages(merge_covr_traces(all_corrupt_dir, retries = 1L, delay = 0.01))
stopifnot(identical(res_all_corrupt, character()),
    length(list.files(all_corrupt_dir, pattern = "^covr_trace_[^/]+$")) == 0L)
unlink(all_corrupt_dir, recursive = TRUE)

# (c) Delayed writer finishing an initially-incomplete trace within ~150ms.
delayed_dir <- tempfile("delayed-writer-dir-")
dir.create(delayed_dir)
fast_trace <- file.path(delayed_dir, "covr_trace_1_fast")
slow_trace <- file.path(delayed_dir, "covr_trace_2_slow")
saveRDS(make_trace_env(e1), fast_trace)
saveRDS(make_trace_env(e2), slow_trace)
expected_delayed <- covr:::merge_coverage(c(fast_trace, slow_trace))
slow_full_raw <- readBin(slow_trace, "raw", file.info(slow_trace)$size)
writeBin(head(slow_full_raw, length(slow_full_raw) %/% 2L), slow_trace)
ready_flag <- tempfile("delayed-writer-ready-")
bg_writer <- callr::r_bg(
    function(target_path, ready_path, payload) {
        file.create(ready_path)
        Sys.sleep(0.15)
        tmp_path <- file.path(dirname(target_path), ".writing_covr_trace")
        writeBin(payload, tmp_path)
        file.rename(tmp_path, target_path)
    },
    args = list(target_path = slow_trace, ready_path = ready_flag, payload = slow_full_raw)
)
while (!file.exists(ready_flag) && bg_writer$is_alive()) {
    Sys.sleep(0.01)
}
delayed_merged <- merge_covr_traces(delayed_dir)
bg_writer$wait()
unlink(ready_flag)
delayed_rem <- list.files(delayed_dir, pattern = "^covr_trace_[^/]+$", full.names = TRUE)
delayed_actual <- covr:::merge_coverage(delayed_rem)
stopifnot(identical(delayed_rem, delayed_merged),
    setequal(names(delayed_actual), names(expected_delayed)),
    identical(delayed_actual[names(expected_delayed)], expected_delayed))
unlink(delayed_dir, recursive = TRUE)

# Disappearing file mid-retry is skipped quietly without message or error.
vanish_file <- tempfile("covr_trace_vanish_")
invisible(file.create(vanish_file))
vanish_ready <- tempfile("vanish-ready-")
bg_vanish <- callr::r_bg(
    function(target_path, ready_path) {
        file.create(ready_path)
        Sys.sleep(0.05)
        unlink(target_path)
    },
    args = list(target_path = vanish_file, ready_path = vanish_ready)
)
while (!file.exists(vanish_ready) && bg_vanish$is_alive()) {
    Sys.sleep(0.01)
}
vanish_msgs <- character()
vanish_res <- withCallingHandlers(
    read_covr_trace(vanish_file, retries = 3L, delay = 0.1),
    message = function(m) {
        vanish_msgs <<- c(vanish_msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
    }
)
bg_vanish$wait()
unlink(vanish_ready)
stopifnot(is.null(vanish_res), length(vanish_msgs) == 0L, !file.exists(vanish_file))
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
