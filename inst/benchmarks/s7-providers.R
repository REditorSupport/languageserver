# Run the same file against two installed languageserver revisions with S7
# 1.0.0 first on .libPaths(). See README.md for commands and sample results.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, args[[1L]] %in% c("baseline", "candidate"))
ns <- asNamespace("languageserver")
stopifnot(as.character(utils::packageVersion("S7")) == "1.0.0")
mode <- args[[1L]]
invisible(ns$lsp_settings$set("index_persistent_cache", FALSE))
invisible(ns$lsp_settings$set("member_completion", TRUE))

# Pass "ggplot2" in place of the iteration count for one fresh worker.
# Alternate baseline and candidate Rscript processes for independent samples.
if (identical(args[[2L]], "ggplot2")) {
    options(languageserver.package_cache_dir = FALSE)
    elapsed <- system.time(snapshot <- ns$member_prepare_package("ggplot2"))[["elapsed"]]
    roots <- sum(vapply(snapshot$roots, function(x) !is.null(x$s7_descriptor), logical(1L)))
    uri <- "file:///ggplot-members.R"
    query <- "ggplot2::class_ggplot()@"
    document <- ns$Document$new(uri, language = "r", version = 1L, content = query)
    data <- ns$parse_document(uri, query, include_xml = FALSE)
    data$version <- 1L
    document$update_parse_data(data)
    workspace <- ns$Workspace$new(NULL)
    workspace$member_metadata$set("ggplot2", snapshot)
    items <- ns$member_completion(uri, workspace, document, list(row = 0L, col = nchar(query)), TRUE, 200L)
    labels <- sort(vapply(items, `[[`, character(1L), "label"))
    expected <- if (mode == "baseline") character() else sort(S7::prop_names(ggplot2::class_ggplot()))
    stopifnot(identical(unname(labels), expected))
    cat(sprintf("%s ggplot2 %s: %.3f s, %d bytes, %d S7 roots, %d total roots, %d properties\n",
            mode, as.character(utils::packageVersion("ggplot2")), elapsed, utils::object.size(snapshot),
            roots, length(snapshot$roots), length(labels)))
    quit(status = 0L)
}
iterations <- as.integer(args[[2L]])
stopifnot(iterations >= 3L)

bench <- function(name, run, validate, count = iterations) {
    validate(run())
    samples <- numeric(count)
    for (i in seq_len(count)) {
        start <- Sys.time()
        result <- run()
        samples[[i]] <- as.numeric(difftime(Sys.time(), start, units = "secs")) * 1000
        validate(result)
    }
    cat(sprintf("%-38s %9.2f %9.2f\n", name, median(samples), unname(quantile(samples, 0.9))))
}

main <- function() {
    root <- tempfile("s7-provider-bench-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE))
    dir.create(file.path(root, "R"))
    writeLines("Package: benchpkg", file.path(root, "DESCRIPTION"))
    writeLines("import(S7)", file.path(root, "NAMESPACE"))
    plain_uri <- ns$path_to_uri(file.path(root, "interactive.R"))
    pkg_uri <- ns$path_to_uri(file.path(root, "R", "points.R"))
    make_model <- function() ns$member_index_freeze(ns$member_package_index(ns$member_namespace_input(asNamespace("S7"))))
    cat("R", as.character(getRversion()), R.version$platform, "- S7 1.0.0 -", mode,
        "-", iterations, "samples\n")
    cat(sprintf("%-38s %9s %9s\n", "Workload (ms)", "median", "p90"))
    bench("S7 namespace member index", make_model,
        function(m) stopifnot("new_class" %in% m$exports), min(5L, iterations))
    model <- ns$member_prepare_package("S7")
    workspace <- ns$Workspace$new(root)
    workspace$member_metadata$set("S7", model)
    point <- "Point := new_class(properties = list(x = class_numeric, y = class_numeric))"
    corpus <- c("library(S7)", point, 'distance := new_generic("x")',
        sprintf("calculate_%03d <- function(x, y) x + y", seq_len(200L)),
        "method(distance, Point) <- function(x) sqrt(x@x^2 + x@y^2)", "p <- Point(3, 4)")
    bench("Parse 200 functions and S7 declarations",
        function() ns$parse_document(plain_uri, corpus, include_xml = FALSE),
        function(d) stopifnot(!d$parse_error, "calculate_200" %in% names(d$definitions)))
    complete <- function(uri, lines, row, col, expected, name) {
        doc <- ns$Document$new(uri, language = "r", version = 1L, content = lines)
        data <- ns$parse_document(uri, lines, include_xml = FALSE)
        if (uri == pkg_uri) {
            workspace <- ns$Workspace$new(root)
            if ("S7" %in% ns$member_package_request(data)$namespace_packages) workspace$member_metadata$set("S7", model)
        }
        data$version <- 1L
        doc$update_parse_data(data)
        pos <- list(row = row, col = col)
        read <- function() {
            items <- ns$member_completion(uri, workspace, doc, pos, TRUE, 200L)
            sort(vapply(items, `[[`, character(1L), "label"), method = "radix")
        }
        check <- function(x) if (!identical(unname(x), expected)) stop(name, " returned ", paste(x, collapse = ", "))
        bench(paste(name, "cold"), function() {
            doc$update_parse_data(data)
            read()
        }, check)
        bench(paste(name, "warm (10 requests)"), function() {
            for (i in seq_len(9L)) check(read())
            read()
        }, check)
    }
    complete(plain_uri, c("p <- list(x = 3, y = 4)", "p@unused", "p$"), 2L, 2L, c("x", "y"), "Ordinary list")
    complete(plain_uri, c("library(S7)", point, "p <- Point()", "p@"), 3L, 2L, c("x", "y"), "Attached S7")
    added <- if (mode == "baseline") character() else c("x", "y")
    complete(pkg_uri, c(point, "p <- Point()", "p@"), 2L, 2L, added, "Package import")
    complete(plain_uri, c(corpus[1:3], "method(distance, Point) <- function(x) {", "  x@", "}"),
        4L, 4L, added, "Method argument")
}
catalog <- function() {
    root <- tempfile("s7-catalog-bench-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE))
    dir.create(file.path(root, "R"))
    writeLines("Package: catalogbench", file.path(root, "DESCRIPTION"))
    writeLines(c("import(S7)", 'import(rlang, except = ":=")'), file.path(root, "NAMESPACE"))
    paths <- file.path(root, "R", sprintf("source%03d.R", seq_len(128L)))
    code <- lapply(seq_along(paths), function(i) {
        if (i <= 8L) {
            c(sprintf("Shape%d := new_class(properties = list(x = class_double))", i),
                sprintf('measure%d := new_generic("x")', i),
                sprintf("method(measure%d, list(Shape%d)) <- function(x) x@x", i, i))
        } else {
            sprintf("ordinary%d <- function(x) x + 1L", i)
        }
    })
    for (i in seq_along(paths)) writeLines(code[[i]], paths[[i]])
    index_one <- function(i) ns$index_shallow_summary(paths[[i]], code[[i]], root)
    definitions <- if (mode == "candidate") 2L else 0L
    check_one <- function(x) stopifnot(length(x$definitions) == definitions)
    bench("Index one package class/generic/method", function() index_one(1L), check_one)
    all_files <- function() lapply(seq_along(paths), index_one)
    bench("Index 8 S7 + 120 ordinary files", all_files, function(all) {
        check_one(all[[8L]])
        stopifnot(length(all) == 128L, names(all[[128L]]$definitions) == "ordinary128")
    })
    workspace <- ns$Workspace$new(root)
    for (s in all_files()) workspace$index$set_summary(s)
    search <- function(query = "measure") ns$workspace_symbol_reply(1L, list(workspace), query)$result
    bench("10 ordinary symbol searches (128 files)", function() {
        for (i in seq_len(9L)) search("ordinary128")
        search("ordinary128")
    }, function(x) stopifnot(length(x) == 1L, x[[1L]]$name == "ordinary128"))
    expected <- if (mode == "candidate") rep(c(6L, 12L), each = 8L) else integer()
    bench("10 symbol searches over 128 files", function() {
        for (i in seq_len(9L)) search()
        search()
    }, function(x) stopifnot(identical(sort(vapply(x, function(item) as.integer(item$kind), integer(1L))), expected)))
}
main()
catalog()
