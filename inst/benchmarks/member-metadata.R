# R_LIBS=/path/to/library Rscript inst/benchmarks/member-metadata.R results.csv 10
# An optional third argument reads an R document instead of the fixture below.
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else "member-metadata-performance.csv"
passes <- if (length(args) > 1L) as.integer(args[[2L]]) else 10L
stopifnot(!is.na(passes), passes > 0L)
content <- if (length(args) > 2L) readLines(args[[3L]]) else c(
    "library(polars)", "",
    "csv_file <- tempfile(fileext = \".csv\")",
    "write.csv(iris, csv_file, row.names = FALSE)", "",
    "q <- pl$scan_csv(csv_file, infer_schema_files = 10)", "q", "",
    "q1 <- q$filter(pl$col(\"Sepal.Length\") > 5)", "q1", "",
    "q1 <- q$filter(pl$col(\"Sepal.Length\") > 5)$group_by(\"Species\")$agg(pl$all()$median())$collect()", "q1", "",
    "q2 <- q1$group_by(\"Species\")$agg(pl$all()$sum())", "q2", "",
    "Leaf <- methods::setClass(\"Leaf\", slots = c(value = \"numeric\"))",
    "Dataset <- R6::R6Class(\"Dataset\", public = list(value = 1))"
)
# Match production's clean metadata worker, independent of benchmark state.
snapshots <- callr::r(function() {
    packages <- c("polars", "methods", "R6")
    stats::setNames(lapply(packages, languageserver:::member_prepare_package), packages)
})
benchmark <- new.env(parent = asNamespace("languageserver"))
benchmark$content <- content
benchmark$snapshots <- snapshots
benchmark$output <- output
benchmark$passes <- passes
evalq({
    cache <- MemberMetadataCache$new(32 * 1024^2)
    for (package in names(snapshots)) cache$set(package, snapshots[[package]])
    stats <- new.env(parent = emptyenv())
    metadata <- list(keys = cache$keys, get = function(package) {
        if (cache$.__enclos_env__$private$indexes$has(package)) {
            stats$hits <- stats$hits + 1L
        } else {
            stats$restored <- c(stats$restored, package)
        }
        cache$get(package)
    })
    # The same workload runs on the pre-catalog revision for comparison.
    if (is.function(cache$catalog)) metadata$catalog <- cache$catalog
    measurements <- list()
    rows <- which(content %in% c("q", "q1", "q2")) - 1L
    stopifnot(length(rows) > 0L)
    for (row in rows) {
        edited <- content
        name <- edited[[row + 1L]]
        edited[[row + 1L]] <- paste0(name, "$group_by(\"Species\", .maintain_order = ")
        uri <- "file:///member-metadata-benchmark.R"
        document <- Document$new(uri, version = 1L, content = edited)
        parsed <- parse_document(uri, edited)
        parsed$version <- 1L
        document$update_parse_data(parsed)
        workspace <- list(member_metadata = metadata, get_documentation = function(...) NULL)
        for (pass in seq_len(passes)) {
            for (provider in c("completion", "signature", "hover")) {
                stats$hits <- 0L
                stats$restored <- character()
                gc()
                start <- proc.time()[[3L]]
                result <- switch(provider,
                    completion = member_completion(uri, workspace, document,
                        list(row = row, col = nchar(name) + 1L), TRUE, 200L),
                    signature = signature_reply(1L, uri, workspace, document,
                        list(row = row, col = nchar(edited[[row + 1L]])))$result,
                    hover = hover_reply(1L, uri, workspace, document,
                        list(row = row, col = nchar(name) + 3L))$result
                )
                elapsed <- (proc.time()[[3L]] - start) * 1000
                # Failed inference is not a fast request.
                signature <- "group_by(..., .maintain_order = FALSE)"
                valid <- switch(provider,
                    completion = "group_by" %in% vapply(result, `[[`, character(1L), "label"),
                    signature = identical(result$signatures[[1L]]$label, signature),
                    hover = identical(result$contents[[1L]], sprintf("```r\n%s\n```", signature))
                )
                stopifnot(valid)
                measurements[[length(measurements) + 1L]] <- data.frame(
                    row = row, pass = pass, provider = provider, elapsed_ms = elapsed,
                    hits = stats$hits, restores = length(stats$restored),
                    packages = paste(stats$restored, collapse = ",")
                )
            }
        }
    }
    data <- do.call(rbind, measurements)
    write.csv(data, output, row.names = FALSE)
    print(aggregate(elapsed_ms ~ provider, data, function(x) {
        c(median = median(x), p95 = unname(quantile(x, 0.95)), max = max(x))
    }))
    print(aggregate(cbind(hits, restores) ~ provider, data, sum))
}, benchmark)
