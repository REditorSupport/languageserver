# Run against an installed revision with its library selected by R_LIBS:
# Rscript --vanilla inst/benchmarks/default-values.R results.csv 10000 3
# Measures extraction from one wide default and completion across many formals.
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else "default-values-performance.csv"
size <- if (length(args) > 1L) as.integer(args[[2L]]) else 10000L
samples <- if (length(args) > 2L) as.integer(args[[3L]]) else 3L
stopifnot(!is.na(size), size > 0L, !is.na(samples), samples > 0L)

extract <- getFromNamespace("extract_default_values", "languageserver")
complete <- getFromNamespace("arg_value_completion", "languageserver")
labels <- sprintf("value-%05d", seq_len(size))
default <- as.call(c(list(as.name("c")), as.list(labels)))
defaults <- stats::setNames(rep(list(quote(c("first", "last"))), min(size, 2000L)),
    paste0("parameter", seq_len(min(size, 2000L))))
workspace <- list(guess_namespace = function(...) "example", get_formals = function(...) defaults)
stopifnot(identical(extract(default), labels))
items <- complete(NULL, workspace, NULL, NULL, "", "example")
stopifnot(length(items) == 2L * length(defaults),
    identical(vapply(items, `[[`, "", "label"), rep(c("first", "last"), length(defaults))))

measure <- function(operation, run, count) {
    times <- replicate(samples, {
        gc()
        system.time(invisible(run()))[["elapsed"]] * 1000
    })
    allocation <- NA_real_
    if (capabilities("profmem")) {
        path <- tempfile()
        on.exit(unlink(path), add = TRUE)
        Rprofmem(path)
        tryCatch(invisible(run()), finally = Rprofmem(NULL))
        bytes <- suppressWarnings(as.numeric(sub(" .*", "", readLines(path))))
        allocation <- sum(bytes, na.rm = TRUE)
    }
    data.frame(operation = operation, count = count, median_ms = median(times),
        min_ms = min(times), max_ms = max(times), samples = samples,
        allocated_bytes = allocation)
}
results <- rbind(
    measure("extract_wide_default", function() extract(default), size),
    measure("complete_many_defaults", function() complete(NULL, workspace, NULL, NULL, "", "example"),
        length(defaults)))
write.csv(results, output, row.names = FALSE)
print(results, row.names = FALSE)
cat(sprintf("R %s; %s; languageserver %s\n", getRversion(), R.version$platform,
        as.character(utils::packageVersion("languageserver"))))
