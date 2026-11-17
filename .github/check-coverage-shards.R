source(".github/coverage-shards.R")

files <- paste0("test-", letters, ".R")
timings <- data.frame(file = files, seconds = seq_along(files)^2)
new_files <- c(files[-1], "test-new.R")
shard_counts <- c(1L, 2L, 4L, 8L)
for (count in shard_counts) {
    plan <- coverage_shards(files, timings, count)
    stopifnot(setequal(plan$file, files), !anyDuplicated(plan$file),
        setequal(plan$shard, seq_len(count)),
        identical(plan, coverage_shards(rev(files), timings[26:1, ], count)))

    updated <- coverage_shards(new_files, timings, count)
    stopifnot(setequal(updated$file, new_files),
        updated$seconds[updated$file == "test-new.R"] == median(timings$seconds[-1]))
    fallback <- coverage_shards(files, timings[FALSE, ], count)
    stopifnot(all(fallback$seconds == 1), setequal(fallback$shard, seq_len(count)))
}

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
for (count in shard_counts) {
    plan <- coverage_shards(files, timings, count)
    matches <- vapply(seq_len(count), function(shard) {
        selected <- plan$file[plan$shard == shard]
        filter <- coverage_filter(selected)
        actual <- testthat:::find_test_scripts("tests/testthat", filter = filter,
            full.names = FALSE)
        stopifnot(setequal(actual, selected))
        grepl(filter,
            sub("\\.[Rr]$", "", sub("^test[-_]", "", files)))
    }, logical(length(files)))
    stopifnot(all(rowSums(matches) == 1L), all(colSums(matches) > 0L))
    cat("All", length(files), "test files occur exactly once across", count,
        if (count == 1L) "coverage shard.\n" else "coverage shards.\n")
}

desc_start_first <- read.dcf("DESCRIPTION", fields = "Config/testthat/start-first")[1L, 1L]
start_first <- trimws(strsplit(desc_start_first, ",", fixed = TRUE)[[1L]])
file_contexts <- sub("\\.[Rr]$", "", sub("^test[-_]", "", files))
stopifnot(length(start_first) > 0L, !anyDuplicated(start_first),
    all(start_first %in% file_contexts))
ordered <- testthat:::order_test_scripts(files, start_first)
ordered_contexts <- sub("\\.[Rr]$", "", sub("^test[-_]", "", head(ordered, length(start_first))))
stopifnot(identical(ordered_contexts, start_first))

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
