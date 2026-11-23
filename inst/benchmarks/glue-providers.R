# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/glue-providers.R output.csv 200,1000 3 references
# Each cold request starts on a newly accepted parse; parsing/transport are
# excluded. Warm requests rotate among declarations with distinct occurrences.
args <- commandArgs(TRUE)
output <- if (length(args)) args[[1L]] else "glue-providers.csv"
sizes <- if (length(args) >= 2L) as.integer(strsplit(args[[2L]], ",", fixed = TRUE)[[1L]]) else c(200L, 1000L)
passes <- if (length(args) >= 3L) as.integer(args[[3L]]) else 3L
method <- if (length(args) >= 4L) match.arg(args[[4L]], c("references", "rename")) else "references"
stopifnot(all(sizes > 0L), passes > 0L)
ns <- asNamespace("languageserver")

glue_benchmark_fixture <- function(n, density) {
    metrics <- sprintf("metric_%02d", seq_len(23L))
    i <- seq_len(n)
    vars <- metrics[(i - 1L) %% length(metrics) + 1L]
    is_glue <- switch(density, ordinary = rep(FALSE, n), sparse = i %% 50L == 0L, dense = rep(TRUE, n))
    fn <- c("round", "signif")[(i %% 2L) + 1L]
    inner <- sprintf("%s(%s + %d, digits = 2)", fn, vars, i)
    lines <- sprintf("paste(%s, %d)", inner, i)
    plain <- is_glue & i %% 4L < 2L
    named <- is_glue & i %% 4L == 2L
    data <- is_glue & i %% 4L == 3L
    lines[plain] <- sprintf('glue::glue("Value {%s}")', inner[plain])
    lines[named] <- sprintf('glue::glue("Value {%s}: {label}", label = %d)', inner[named], i[named])
    lines[data] <- sprintf('glue::glue_data(list(label = %d), "Value {%s}: {label}")', i[data], inner[data])
    content <- c(sprintf("%s <- %d", metrics, seq_along(metrics)), lines)
    uri <- "file:///glue-benchmark.R"
    doc <- ns$Document$new(uri, language = "r", version = 1L, content = content)
    parsed <- ns$parse_document(uri, content, include_xml = FALSE)
    parsed$version <- doc$version
    workspace <- ns$Workspace$new(NULL)
    workspace$documents$set(uri, doc)
    workspace$update_parse_data(uri, parsed)
    probe <- c(2L, 5L, 8L, 14L, 20L)
    points <- lapply(probe, function(j) list(row = j - 1L, col = 3L))
    feature <- if (method == "rename") "glue_rename_references" else "glue_reference_occurrences"
    supported <- exists(feature, envir = ns, inherits = FALSE)
    expected <- vapply(metrics[probe], function(nm) 1L + sum(vars == nm & (!is_glue | supported)), integer(1L))
    list(uri = uri, workspace = workspace, document = doc, points = points,
        names = metrics[probe], expected = expected, templates = sum(is_glue))
}

glue_benchmark_request <- function(f, i, method) {
    if (method == "references") {
        refs <- ns$references_reply(1L, f$uri, f$workspace, f$document, f$points[[i]])$result
    } else {
        new <- paste0("renamed_", f$names[[i]])
        edits <- ns$rename_reply(1L, f$uri, f$workspace, f$document, f$points[[i]], new)
        if (!is.null(edits$error)) stop(edits$error$message)
        refs <- edits$result$changes[[f$uri]]
    }
    stopifnot(length(refs) == f$expected[[i]])
    invisible(length(refs))
}

# Warm JIT paths in both revisions before comparing per-document work.
warmup <- glue_benchmark_fixture(20L, "dense")
invisible(glue_benchmark_request(warmup, 1L, method))
measure <- function(f, i) {
    started <- Sys.time()
    glue_benchmark_request(f, i, method)
    as.numeric(difftime(Sys.time(), started, units = "secs")) * 1000
}
rows <- list()
for (n in sizes) for (density in c("ordinary", "sparse", "dense")) {
    cold <- warm <- numeric()
    for (pass in seq_len(passes)) {
        f <- glue_benchmark_fixture(n, density)
        gc()
        cold <- c(cold, measure(f, 1L))
        for (i in seq_along(f$points)) glue_benchmark_request(f, i, method)
        warm <- c(warm, vapply(seq_along(f$points), function(i) measure(f, i), numeric(1L)))
    }
    row <- data.frame(method = method, calls = n, density = density, templates = f$templates,
        passes = passes, cold_median_ms = stats::median(cold), warm_median_ms = stats::median(warm),
        warm_p95_ms = unname(stats::quantile(warm, .95)), first_count = f$expected[[1L]])
    rows[[length(rows) + 1L]] <- row
    print(row, row.names = FALSE)
}
utils::write.csv(do.call(rbind, rows), output, row.names = FALSE)
