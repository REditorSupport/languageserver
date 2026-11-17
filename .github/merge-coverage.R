merge_coverage_shards <- function(paths, file) {
    stopifnot(length(paths) > 0L, !anyDuplicated(paths), all(file.exists(paths)))
    coverages <- lapply(paths, readRDS)
    stopifnot(all(vapply(coverages, inherits, logical(1), "coverage")))
    # Merge expression counts before calculating line coverage. Separate XML
    # reports lose coverage when different shards hit expressions on one line.
    cov <- do.call(c, coverages)
    covr::cobertura(cov, file = file)
    invisible(cov)
}

if (sys.nframe() == 0L) {
    stopifnot(utils::packageVersion("covr") >= "4.0.0")
    args <- commandArgs(trailingOnly = TRUE)
    stopifnot(length(args) == 2L)
    expected_count <- as.integer(args[[2L]])
    stopifnot(!is.na(expected_count), expected_count >= 1L)
    paths <- list.files(args[[1L]], pattern = "^coverage[.]rds$",
        recursive = TRUE, full.names = TRUE)
    stopifnot(length(paths) == expected_count)
    output_dir <- normalizePath(Sys.getenv("RUNNER_TEMP", tempdir()), winslash = "/")
    cov <- merge_coverage_shards(paths, file.path(output_dir, "cobertura.xml"))
    print(cov)
    summary <- c("### Combined coverage", "",
        paste("- Merged shards:", length(paths)),
        paste("- covr:", as.character(utils::packageVersion("covr"))),
        sprintf("- Line coverage: %.2f%%", covr::percent_coverage(cov)), "")
    writeLines(summary, file.path(output_dir, "coverage-summary.md"))
    summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY", "")
    if (nzchar(summary_path)) {
        cat(paste(summary, collapse = "\n"), file = summary_path, append = TRUE)
    }
}
