# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/roxygen-type-hints.R [samples]
args <- commandArgs(trailingOnly = TRUE)
samples <- if (length(args)) as.integer(args[[1L]]) else 5L
stopifnot(length(args) <= 1L, samples >= 1L)
ns <- asNamespace("languageserver")
if (!exists("member_roxygen_type_index", ns, inherits = FALSE)) {
    cat("Direct and inherited roxygen type hints unavailable in this installation.\n")
    quit(save = "no", status = 0L)
}
has_inheritance <- exists("roxygen_effective_params", ns, inherits = FALSE)
if (!has_inheritance) cat("Inherited completion and effective-parameter lookup unavailable in this installation.\n")
measure <- function(label, run) {
    tryCatch({
        first <- system.time(run())[["elapsed"]]
        iterations <- if (first < 0.015) 8L else 1L
        elapsed <- replicate(samples, system.time(for (i in seq_len(iterations)) run())[["elapsed"]] / iterations)
        cat(sprintf("%-24s %9.2f ms\n", label, 1000 * median(elapsed)))
        TRUE
    }, error = function(e) {
        cat(label, conditionMessage(e), "\n")
        FALSE
    })
}

deep <- c("#' @param x (`Record`)", paste0("f <- function(x) {", paste(rep("x", 2000L), collapse = "+"), "}"))
ok <- measure("parse 2000 terms", function() {
    parsed <- ns$parse_document("file:///deep.R", deep, include_xml = FALSE)
    stopifnot(!parsed$parse_error, parsed$member_data$has_type_hints)
})

for (n in c(1000L, 2000L, 4000L, 8000L)) {
    lines <- c("#' @param x (`Record`)", "f <- function(x) {", rep("  foo(x)", n), "}")
    expr <- parse(text = lines, keep.source = TRUE)
    hints <- ns$member_roxygen_type_index(lines, utils::getParseData(expr))
    ok <- measure(paste("attach", n, "calls"), function() {
        annotated <- ns$member_attach_type_hints(expr, hints)
        stopifnot(identical(attr(annotated[[1L]][[3L]], "member_type_hints")$params$x, "Record"))
    }) && ok
}

slots <- sprintf("p%03d", 1:100)
source <- c(paste0("mk <- function() list(", paste(rep("1", 64L), collapse = ", "), ")"),
    paste0('methods::setClass("Fields", slots = c(', paste0(slots, ' = "list"', collapse = ", "),
        "), prototype = list(", paste0(slots, " = mk()", collapse = ", "), "))"),
    'methods::setClass("Left", contains = "Fields")', 'methods::setClass("Right", contains = "Fields")')
for (type in c("Left", "Left | Right")) {
    code <- c(source, sprintf("#' @param x (`%s`)", type), "f <- function(x) x@p001")
    uri <- "file:///slots.R"
    doc <- ns$Document$new(uri, version = 1L, content = code)
    parsed <- ns$parse_document(uri, code)
    parsed$version <- doc$version
    doc$update_parse_data(parsed)
    point <- list(row = length(code) - 1L, col = nchar(tail(code, 1L)))
    ok <- measure(paste("complete", type), function() {
        items <- ns$member_completion(uri, list(), doc, point, FALSE, 100L)
        stopifnot(identical(vapply(items, `[[`, "", "label"), "p001"))
    }) && ok
}
if (!ok) stop("A roxygen type-hint workload failed")

accepted_document <- function(workspace, uri, lines) {
    doc <- ns$Document$new(uri, version = 1L, content = lines)
    parsed <- ns$parse_document(uri, lines, include_xml = FALSE)
    stopifnot(!parsed$parse_error)
    parsed$version <- doc$version
    doc$update_parse_data(parsed)
    workspace$documents$set(uri, doc)
    doc
}

# Cold is the first request on a newly accepted document with a fresh
# document cache. The package index and process are already initialized.
measure_requests <- function(label, prepare, request, check, next_request = NULL) {
    sample <- function() {
        input <- prepare()
        start <- Sys.time()
        result <- request(input)
        cold <- as.numeric(difftime(Sys.time(), start, units = "secs"))
        check(result)
        another <- NULL
        warm_request <- request
        if (!is.null(next_request)) {
            start <- Sys.time()
            result <- next_request(input)
            another <- as.numeric(difftime(Sys.time(), start, units = "secs"))
            check(result)
            warm_request <- next_request
        }
        start <- Sys.time()
        for (i in seq_len(20L)) result <- warm_request(input)
        warm <- as.numeric(difftime(Sys.time(), start, units = "secs")) / 20
        check(result)
        c(first = cold, next_cursor = another, repeat_cursor = warm)
    }
    invisible(sample())
    times <- apply(replicate(samples, sample()), 1L, median) * 1000
    next_time <- if (!is.null(next_request)) sprintf("  next cursor %8.3f ms", times[["next_cursor"]]) else ""
    cat(sprintf("%-28s first %8.3f ms%s  repeat %8.3f ms\n", label, times[["first"]], next_time, times[["repeat_cursor"]]))
}

params <- letters[1:4]
direct <- sprintf("#' @param %s (`Box`) An input.", params)
graph <- c('Box <- R6::R6Class("Box", public = list(label = "x", step = function() self))',
    direct, "seed <- function(a, b, c, d) NULL")
leaves <- character()
for (param in params) {
    donor <- "seed"
    for (depth in 1:5) {
        topic <- paste("branch", param, depth, sep = "_")
        graph <- c(graph, paste("#' @inheritParams", donor), sprintf("%s <- function(%s) NULL", topic, param))
        donor <- topic
    }
    leaves <- c(leaves, donor)
}
variants <- list("direct completion" = direct)
if (has_inheritance) {
    variants[["one-donor completion"]] <- "#' @inheritParams seed"
    variants[["four-branch completion"]] <- paste("#' @inheritParams", leaves)
}
workspace <- ns$Workspace$new(NULL)
uri <- "file:///roxygen-member-benchmark.R"
complete <- function(doc, next_cursor = FALSE) {
    pos <- list(row = doc$nline - if (next_cursor) 2L else 3L, col = nchar("    picked$step()$"))
    ns$member_completion(uri, workspace, doc, pos, FALSE, 100L)
}
visible_items <- function(items) {
    stopifnot(identical(vapply(items, `[[`, "", "label"), c("clone", "label", "step")))
    lapply(items, function(item) {
        list(label = item$label, kind = item$kind, detail = item$detail,
            documentation = item$documentation, insertion = item$textEdit$newText)
    })
}
expected <- NULL
for (name in names(variants)) {
    lines <- c(graph, variants[[name]], "consume <- function(a, b, c, d, choice) {",
        "    picked <- switch(choice, a, b, c, d)", "    picked$step()$label", "    picked$step()$label", "}")
    prepare <- function() accepted_document(workspace, uri, lines)
    if (is.null(expected)) expected <- visible_items(complete(prepare()))
    measure_requests(name, prepare, complete, function(items) stopifnot(identical(visible_items(items), expected)),
        next_request = function(doc) complete(doc, next_cursor = TRUE))
}

benchmark_prose_package <- function(n) {
    root <- ns$index_normalize_path(tempfile("roxygen-prose-"))
    dir.create(file.path(root, "R"), recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE))
    writeLines("Package: prosefixture", file.path(root, "DESCRIPTION"))
    writeLines("", file.path(root, "NAMESPACE"))
    workspace <- ns$Workspace$new(root)
    groups <- split(seq_len(n), ceiling(seq_len(n) / 16L))
    scripts <- lapply(groups, function(ids) {
        unlist(lapply(ids, function(i) {
            key <- params[[(i - 1L) %% 4L + 1L]]
            c(sprintf("#' Document input %d.", i), sprintf("#' @param %s Descriptive prose.", key),
                sprintf("topic_%03d <- function(%s) NULL", i, key))
        }), use.names = FALSE)
    })
    paths <- file.path(root, "R", sprintf("prose%03d.R", seq_along(scripts)))
    for (i in seq_along(paths)) writeLines(scripts[[i]], paths[[i]])
    if (n > 4L) {
        ok <- measure(paste("shallow index", n, "topics"), function() {
            counts <- vapply(seq_along(paths), function(i) {
                length(ns$index_shallow_summary(paths[[i]], scripts[[i]], root)$definitions)
            }, integer(1L))
            stopifnot(sum(counts) == n)
        })
        if (!ok) stop("The prose-only shallow-index workload failed")
    }
    if (!has_inheritance) return(invisible(NULL))
    for (i in seq_along(paths)) {
        uri <- ns$path_to_uri(paths[[i]])
        workspace$index$update_content(uri, scripts[[i]])
        for (topic in sprintf("topic_%03d", groups[[i]])) {
            stopifnot(identical(workspace$index$roxygen_topic_uris(topic), uri))
        }
    }
    typed <- c(sprintf("#' @param %s (`integer`) Fallback.", c(params, "record")),
        "typed <- function(a, b, c, d, record) NULL")
    path <- file.path(root, "R", "typed.R")
    writeLines(typed, path)
    typed_uri <- ns$path_to_uri(path)
    workspace$index$update_content(typed_uri, typed)
    lines <- c(sprintf("#' @inheritParams topic_%03d", 1:4), "#' @inheritParams typed",
        "consume <- function(a, b, c, d, record) NULL")
    path <- file.path(root, "R", "consumer.R")
    writeLines(lines, path)
    uri <- ns$path_to_uri(path)
    workspace$index$update_content(uri, lines)
    measure_requests(paste("resolve among", n, "topics"), function() accepted_document(workspace, uri, lines), function(doc) {
        hints <- doc$parse_data$member_data$type_hints[[1L]]
        ns$roxygen_effective_params(hints, workspace, doc)
    }, function(found) {
        # Each untyped donor consumes its formal. Only record can inherit
        # the typed fallback, whose declaration lives in an unopened file.
        stopifnot(identical(names(found), "record"), identical(found$record$types, "integer"),
            identical(found$record$uri, typed_uri))
    })
}
for (size in c(4L, 512L)) benchmark_prose_package(size)
