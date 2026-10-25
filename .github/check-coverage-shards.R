source(".github/coverage-shards.R")

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
