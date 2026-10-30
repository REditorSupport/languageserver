# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/text-changes.R results.csv 500 10000 3
# Measure notification handling with a pending disk-index backlog. Parse task
# creation, worker execution, transport, and editor rendering are excluded.
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else "text-changes-performance.csv"
edits <- if (length(args) > 1L) as.integer(args[[2L]]) else 500L
files <- if (length(args) > 2L) as.integer(args[[3L]]) else 10000L
passes <- if (length(args) > 3L) as.integer(args[[4L]]) else 3L
stopifnot(!is.na(edits), edits > 0L, !is.na(files), files >= 0L,
    !is.na(passes), passes > 0L)
benchmark <- new.env(parent = asNamespace("languageserver"))
benchmark$output <- output
benchmark$edits <- edits
benchmark$files <- files
benchmark$passes <- passes
evalq({
    lsp_settings$set("index_mode", "lazy")
    lsp_settings$set("index_persistent_cache", FALSE)
    lsp_settings$set("diagnostics", FALSE)
    root <- tempdir()
    uri <- path_to_uri(file.path(root, "main.R"))
    rows <- lapply(seq_len(passes), function(pass) {
        workspace <- new.env(parent = emptyenv())
        workspace$documents <- collections::dict()
        workspace$index <- WorkspaceIndex$new(root)
        pending <- path_to_uri(file.path(root, paste0(seq_len(files), ".R")))
        workspace$index$pending <- pending
        document <- Document$new(uri, language = "r", version = 1L, content = "x")
        document$did_open()
        workspace$documents$set(uri, document)
        self <- new.env(parent = emptyenv())
        self$get_workspace <- function(...) workspace
        self$pending_replies <- collections::dict()
        # Isolate text changes and index invalidation from parse task creation.
        self$text_sync <- function(uri, ...) workspace$index$await_parse(uri)
        params <- list(textDocument = list(uri = uri, version = 2L),
            contentChanges = list(list(range = range(position(0L, 0L), position(0L, 1L)), text = "y")))
        gc()
        started <- proc.time()
        for (edit in seq_len(edits)) {
            params$textDocument$version <- edit + 1L
            params$contentChanges[[1L]]$text <- if (edit %% 2L) "y" else "x"
            text_document_did_change(self, params)
        }
        elapsed <- proc.time() - started
        stopifnot(identical(workspace$index$pending, pending),
            identical(document$version, edits + 1L),
            identical(document$content, if (edits %% 2L) "y" else "x"))
        data.frame(pass = pass, pending_files = files, edits = edits,
            elapsed_ms = 1000 * elapsed[[3L]], cpu_ms = 1000 * sum(elapsed[1:2]),
            ms_per_edit = 1000 * elapsed[[3L]] / edits)
    })
    results <- do.call(rbind, rows)
    utils::write.csv(results, output, row.names = FALSE)
    print(results, row.names = FALSE)
}, benchmark)
