check_coverage_merge <- function() {
    source(".github/merge-coverage.R", local = TRUE)

    fixture_dir <- tempfile("coverage-merge-check-")
    dir.create(fixture_dir)
    on.exit(unlink(fixture_dir, recursive = TRUE), add = TRUE)
    fixture_file <- file.path(fixture_dir, "branch.R")
    writeLines("branch_probe <- function(x) if (x) 1L else 2L", fixture_file)
    fixture_env <- new.env(parent = baseenv())
    sys.source(fixture_file, envir = fixture_env, keep.source = TRUE,
        keep.parse.data = TRUE)

    # Each shard executes a different expression on the same source line.
    # covr 4.0.0 reports that line as uncovered in both individual reports.
    cov_true <- covr::function_coverage("branch_probe",
        fixture_env$branch_probe(TRUE), env = fixture_env)
    cov_false <- covr::function_coverage("branch_probe",
        fixture_env$branch_probe(FALSE), env = fixture_env)
    for (coverage in list(cov_true, cov_false)) {
        lines <- covr::tally_coverage(coverage, by = "line")
        stopifnot(nrow(lines) == 1L, lines$value == 0,
            covr::percent_coverage(coverage) == 0)
    }

    shard_dirs <- file.path(fixture_dir, c("shard-true", "shard-false"))
    for (dir in shard_dirs) dir.create(dir)
    paths <- file.path(shard_dirs, "coverage.rds")
    saveRDS(cov_true, paths[[1L]])
    saveRDS(cov_false, paths[[2L]])
    xml_file <- file.path(fixture_dir, "cobertura.xml")
    combined <- merge_coverage_shards(paths, xml_file)
    combined_lines <- covr::tally_coverage(combined, by = "line")
    stopifnot(nrow(combined_lines) == 1L, combined_lines$value == 1,
        covr::percent_coverage(combined) == 100)

    # Export only after merging expression counters, so hits survive sharding.
    xml <- xml2::read_xml(xml_file)
    xml_lines <- xml2::xml_find_all(xml, "//class/lines/line")
    stopifnot(length(xml_lines) == 1L,
        xml2::xml_attr(xml_lines, "number") == "1",
        xml2::xml_attr(xml_lines, "hits") == "1")

    # The aggregation job starts a fresh process without covr already loaded.
    cli_dir <- file.path(fixture_dir, "cli-output")
    dir.create(cli_dir)
    cli_output <- system2(file.path(R.home("bin"), "Rscript"),
        args = c("--vanilla", shQuote(normalizePath(".github/merge-coverage.R")),
            shQuote(fixture_dir), "2"),
        stdout = TRUE, stderr = TRUE,
        env = c(paste0("RUNNER_TEMP=", shQuote(cli_dir)), "GITHUB_STEP_SUMMARY="))
    cli_status <- attr(cli_output, "status")
    if (!is.null(cli_status) && cli_status != 0L) {
        stop(paste(cli_output, collapse = "\n"), call. = FALSE)
    }
    cli_xml <- xml2::read_xml(file.path(cli_dir, "cobertura.xml"))
    cli_lines <- xml2::xml_find_all(cli_xml, "//class/lines/line")
    stopifnot(length(cli_lines) == 1L,
        xml2::xml_attr(cli_lines, "number") == "1",
        xml2::xml_attr(cli_lines, "hits") == "1")
    cat("Coverage objects merge before Cobertura export without losing split-line hits.\n")
}

check_coverage_merge()
