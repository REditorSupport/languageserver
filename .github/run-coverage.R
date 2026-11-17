stopifnot(getRversion() >= "4.1.0", utils::packageVersion("covr") >= "4.0.0")

output_dir <- normalizePath(Sys.getenv("RUNNER_TEMP", tempdir()), winslash = "/")
cores <- parallel::detectCores()
cat(R.version.string, "\n")
cat("covr", as.character(utils::packageVersion("covr")), "\n")
cat("Detected cores:", cores, "\n")
cat("Test workers:", Sys.getenv("TESTTHAT_CPUS", "default"), "\n")

# covr 4.0 instruments the installed package once and streams compact counts
# from its workers. Use its native merger instead of rewriting trace files.
started <- proc.time()[[3L]]
cov <- covr::package_coverage(
    quiet = FALSE,
    clean = FALSE,
    install_path = file.path(output_dir, "package")
)
elapsed <- proc.time()[[3L]] - started
print(cov)
covr::cobertura(cov, file = file.path(output_dir, "cobertura.xml"))

summary <- c(
    paste("### Coverage shard", Sys.getenv("COVERAGE_SHARD", "local")),
    "",
    paste("- R:", as.character(getRversion())),
    paste("- covr:", as.character(utils::packageVersion("covr"))),
    paste("- Detected cores:", cores),
    paste("- Test workers:", Sys.getenv("TESTTHAT_CPUS", "default")),
    sprintf("- Coverage elapsed time: %.1f seconds", elapsed),
    sprintf("- Line coverage: %.2f%%", covr::percent_coverage(cov)),
    ""
)
writeLines(summary, file.path(output_dir, "coverage-summary.md"))
summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY", "")
if (nzchar(summary_path)) {
    cat(paste(summary, collapse = "\n"), file = summary_path, append = TRUE)
}
