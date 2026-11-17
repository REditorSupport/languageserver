# Binary mirrors may have dependency metadata for a release before its binary.
version <- tryCatch(utils::packageVersion("covr"), error = function(e) NULL)
if (is.null(version) || version < "4.0.0") {
    utils::install.packages("covr", repos = "https://cloud.r-project.org", type = "source")
}
stopifnot(utils::packageVersion("covr") >= "4.0.0")
cat("covr", as.character(utils::packageVersion("covr")), "\n")
