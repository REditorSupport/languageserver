# Rscript --vanilla inst/benchmarks/completion-label-details.R results.csv 20 . fixed
# Optional arguments: output CSV, repetitions, source checkout, revision label.
# The source checkout needs its native library built; an installed package is
# used when the third argument is omitted. No editor, IPC or parse debounce is
# timed. Namespace preparation and the package fixture's parse are outside the
# timer; rapid-edit fallback parses are included.
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else "completion-label-details.csv"
passes <- if (length(args) > 1L) as.integer(args[[2L]]) else 20L
stopifnot(!is.na(passes), passes > 0L)
if (length(args) > 2L) pkgload::load_all(args[[3L]], quiet = TRUE, compile = FALSE)
revision <- if (length(args) > 3L) args[[4L]] else "installed"
benchmark <- new.env(parent = asNamespace("languageserver"))
benchmark$output <- output
benchmark$passes <- passes
benchmark$revision <- revision
evalq({
    capabilities <- list(completionItem = list(labelDetailsSupport = TRUE, snippetSupport = TRUE))
    lsp_settings$set("max_completions", 200L)
    measurements <- new.env(parent = emptyenv())
    measurements$rows <- list()
    record <- function(scenario, lines, inline, cache, pass, elapsed, result, basic = NA, parsed = FALSE) {
        items <- result$result$items
        stopifnot(length(items) == 200L)
        stopifnot(length(unique(vapply(items, `[[`, character(1L), "label"))) == 200L)
        signatures <- sum(vapply(items, function(item) !is.null(item$labelDetails$detail), logical(1L)))
        if (inline) stopifnot(signatures == 200L)
        measurements$rows[[length(measurements$rows) + 1L]] <- data.frame(
            revision = revision, scenario = scenario, lines = lines, inline = inline,
            cache = cache, pass = pass, elapsed_ms = elapsed, items = length(items),
            signatures = signatures, basic = basic, parsed = parsed)
    }
    fixture <- function(content, namespace, accepted = TRUE) {
        uri <- "file:///completion-label-details-benchmark.R"
        document <- Document$new(uri, version = 1L, content = content)
        if (accepted) {
            parsed <- parse_document(uri, content)
            parsed$version <- document$version
            document$update_parse_data(parsed)
        }
        namespaces <- collections::dict()
        namespaces$set("base", namespace)
        documents <- collections::dict()
        documents$set(uri, document)
        globals <- GlobalEnv$new(documents)
        workspace <- list(
            documents = documents, namespaces = namespaces,
            loaded_packages = "base", startup_packages = "base",
            imported_objects = collections::dict(),
            member_metadata = MemberMetadataCache$new(1024^2),
            get_namespace = function(package, ...) {
                if (identical(package, WORKSPACE)) globals else namespaces$get(package, NULL)
            }, get_parse_data = function(...) document$parse_data,
            get_documentation = function(...) NULL)
        list(uri = uri, document = document, workspace = workspace)
    }
    namespace <- PackageNamespace$new("base")
    candidates <- namespace$get_symbols(TRUE, FALSE)
    candidates <- candidates[vapply(candidates, function(name) {
        typeof(get(name, asNamespace("base"))) == "closure"
    }, logical(1L))]
    candidates <- head(sort(candidates), 200L)
    stopifnot(length(candidates) == 200L)
    unlockBinding("get_symbols", namespace)
    unlockBinding("get_lazydata", namespace)
    namespace$get_symbols <- function(want_functs, ...) if (want_functs) candidates else character()
    namespace$get_lazydata <- function() character()
    state <- fixture("base:::", namespace)
    point <- list(row = 0L, col = 7L)
    for (inline in c(FALSE, TRUE)) {
        lsp_settings$set("completion_label_details", inline)
        for (cache in c("cold", "warm")) {
            if (cache == "warm") completion_reply(1L, state$uri, state$workspace, state$document, point, capabilities)
            gc()
            for (pass in seq_len(passes)) {
                if (cache == "cold") namespace$.__enclos_env__$private$function_metadata$clear()
                start <- proc.time()[[3L]]
                result <- completion_reply(1L, state$uri, state$workspace, state$document, point, capabilities)
                elapsed <- (proc.time()[[3L]] - start) * 1000
                record("package_candidates", 1L, inline, cache, pass, elapsed, result)
            }
        }
    }
    for (lines in c(250L, 10000L)) {
        content <- c(sprintf("bench_fun_%03d <- function(x, y = 1, ...) x", seq_len(200L)),
            sprintf("unrelated_%05d <- %d", seq_len(lines - 201L), seq_len(lines - 201L)), "bench")
        for (inline in c(FALSE, TRUE)) {
            lsp_settings$set("completion_label_details", inline)
            state <- fixture(content, namespace, accepted = FALSE)
            point <- list(row = length(content) - 1L, col = 5L)
            for (pass in seq_len(passes)) {
                edited <- content
                edited[[length(edited)]] <- if (pass %% 2L) "bench" else "bench_"
                state$document$set_content(state$document$version + 1L, edited)
                point$col <- nchar(tail(edited, 1L))
                gc()
                start <- proc.time()[[3L]]
                result <- basic_name_completion_reply(1L, state$workspace, state$document, point, capabilities)
                basic <- !is.null(result)
                if (!basic) {
                    parsed <- parse_document(state$uri, edited)
                    parsed$version <- state$document$version
                    state$document$update_parse_data(parsed)
                    result <- completion_reply(1L, state$uri, state$workspace, state$document, point, capabilities)
                }
                elapsed <- (proc.time()[[3L]] - start) * 1000
                record("rapid_edit_first_reply", lines, inline, "edited", pass, elapsed,
                    result, basic = basic, parsed = !basic)
            }
        }
    }
    data <- do.call(rbind, measurements$rows)
    write.csv(data, output, row.names = FALSE)
    print(aggregate(elapsed_ms ~ scenario + lines + inline + cache, data, function(x) {
        c(median = median(x), p95 = unname(quantile(x, 0.95)), max = max(x))
    }))
    print(aggregate(cbind(signatures, parsed) ~ scenario + lines + inline + cache, data, mean))
    cat(R.version.string, "\n", R.version$platform, "\n")
    print(Sys.info()[c("sysname", "release", "machine")])
}, benchmark)
