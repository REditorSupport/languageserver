#' Start a message-based language server for open documents
#'
#' Creates an independent server in the current R runtime. This entry point is
#' suitable for dedicated webR workers and other hosts without subprocesses.
#' Send complete, unframed LSP JSON-RPC messages in arrival order. The host
#' forwards each returned string to its language client. On disposal, send
#' `shutdown` and `exit`, or release the function and its worker.
#'
#' The server uses the editor's text (including unsaved incremental changes)
#' and the R packages available in the runtime. Definitions, references and
#' rename consult open documents in the same workspace folder. Changes are
#' applied immediately and parsed on demand before servicing a request. User
#' scripts are never sourced. Pull diagnostics report R syntax errors and, when lintr is installed,
#' its infix-spacing rule. They respect the diagnostics setting and do not load
#' project lint configuration. Formatting is advertised when styler is installed.
#'
#' @return A function taking a single JSON string and returning a character
#'   vector of unframed JSON replies. Notifications normally return `character(0)`.
#' @export
run_embedded <- function() {
    workspaces <- new.env(hash = TRUE, parent = emptyenv())
    folders <- character()
    capabilities <- NULL
    formatting <- FALSE
    diagnostics <- TRUE
    lint <- TRUE
    refresh_id <- 0
    generated <- NULL
    state <- "new"

    diagnostic_mode <- function() {
        if (!isTRUE(getOption("languageserver.diagnostics", diagnostics))) return("off")
        if (identical(getOption("languageserver.diagnostics_backend", if (lint) "lintr" else ""), "lintr")) {
            return("lintr")
        }
        "syntax"
    }

    workspace_for <- function(uri) {
        folder <- folders[startsWith(uri, folders)]
        key <- if (length(folder)) folder[[1L]] else "<default>"
        if (is.null(workspaces[[key]])) {
            workspaces[[key]] <- Workspace$new(NULL, open_documents_only = TRUE)
        }
        workspaces[[key]]
    }

    provide <- function(method, id, params, uri) {
        if (identical(method, "workspace/textDocumentContent") && !is.null(generated)) {
            text <- generated$get(params$uri)
            if (is.null(text)) return(ResponseErrorMessage$new(id, "RequestFailed", "Document unavailable"))
            return(Response$new(id, list(text = text)))
        }
        if (identical(method, "completionItem/resolve")) {
            uri <- params$data$context_uri
            if (is.null(uri)) uri <- params$data$uri
            ws <- workspace_for(if (is.null(uri)) "" else uri)
            embedded_parse_open(ws)
            return(completion_item_resolve_reply(id, ws, params, capabilities$textDocument$completion))
        }
        if (is.null(uri)) return(ResponseErrorMessage$new(id, "MethodNotFound", method))
        if (identical(method, "textDocument/diagnostic")) {
            doc <- workspace_for(uri)$documents$get(uri, NULL)
            mode <- diagnostic_mode()
            return(Response$new(id, embedded_diagnostics(doc, mode != "off", mode == "lintr", params$previousResultId)))
        }
        operation <- switch(method,
            "textDocument/completion" = function() completion_reply(id, uri, ws, doc, point,
                capabilities$textDocument$completion),
            "textDocument/hover" = function() hover_reply(id, uri, ws, doc, point),
            "textDocument/signatureHelp" = function() signature_reply(id, uri, ws, doc, point, capabilities),
            "textDocument/definition" = function() embedded_definition(id, uri, ws, doc, point, generated),
            "textDocument/references" = function() references_reply(id, uri, ws, doc, point,
                include_declaration = !isFALSE(params$context$includeDeclaration)),
            "textDocument/rename" = function() rename_reply(id, uri, ws, doc, point, params$newName),
            "textDocument/prepareRename" = function() prepare_rename_reply(id, uri, ws, doc, point),
            "textDocument/documentHighlight" = function() document_highlight_reply(id, uri, ws, doc, point),
            "textDocument/documentSymbol" = function() document_symbol_reply(id, uri, ws, doc,
                capabilities$textDocument$documentSymbol),
            "textDocument/foldingRange" = function() document_folding_range_reply(id, uri, ws, doc),
            "textDocument/semanticTokens/full" = function() semantic_tokens_full_reply(id, uri, ws, doc),
            "textDocument/formatting" = if (formatting) {
                function() Response$new(id, styler_document_edits(uri, doc, params$options))
            }
        )
        if (is.null(operation)) return(ResponseErrorMessage$new(id, "MethodNotFound", method))
        ws <- workspace_for(uri)
        doc <- ws$documents$get(uri, NULL)
        if (is.null(doc)) return(Response$new(id))
        embedded_parse_open(ws)
        point <- if (!is.null(params$position)) doc$from_lsp_position(params$position)
        result <- operation()
        if (is.null(result)) Response$new(id) else result
    }

    dispatch <- function(packet) {
        method <- packet$method
        params <- packet$params
        id <- packet$id
        request <- !is.null(id)
        if (identical(method, "exit") && !request) {
            workspaces <<- NULL
            generated <<- NULL
            state <<- "closed"
            return(NULL)
        }
        if (state == "closed") return(NULL)
        if (identical(method, "initialize") && request && state == "new") {
            roots <- vapply(params$workspaceFolders, "[[", character(1), "uri")
            if (!length(roots)) roots <- params$rootUri
            roots <- sub("/+$", "", roots[nzchar(roots)])
            if (length(roots)) folders <<- paste0(roots[order(nchar(roots), decreasing = TRUE)], "/")
            capabilities <<- params$capabilities
            formatting <<- requireNamespace("styler", quietly = TRUE)
            state <<- "ready"
            features <- ServerCapabilities[c("positionEncoding", "textDocumentSync",
                "completionProvider", "hoverProvider", "signatureHelpProvider",
                "definitionProvider", "referencesProvider", "renameProvider",
                "documentSymbolProvider", "documentHighlightProvider", "foldingRangeProvider",
                "semanticTokensProvider")]
            features$semanticTokensProvider$full <- TRUE
            features$semanticTokensProvider$range <- FALSE
            features$diagnosticProvider <- list(interFileDependencies = FALSE, workspaceDiagnostics = FALSE)
            features$documentFormattingProvider <- formatting
            if (is.list(capabilities$workspace$textDocumentContent)) {
                generated <<- TextDocumentContent$new()
                features$workspace <- list(textDocumentContent = list(schemes = list(generated$scheme)))
            }
            return(Response$new(id, list(capabilities = features)))
        }
        if (state != "ready" || identical(method, "initialize")) {
            if (!request) return(NULL)
            code <- if (state == "new") "ServerNotInitialized" else "InvalidRequest"
            return(ResponseErrorMessage$new(id, code, paste("Server is", state)))
        }
        if (identical(method, "shutdown") && request) {
            workspaces <<- NULL
            generated <<- NULL
            state <<- "shutdown"
            return(Response$new(id))
        }

        uri <- params$textDocument$uri
        if (!request) {
            if (identical(method, "workspace/didChangeConfiguration")) {
                previous <- diagnostic_mode()
                settings <- params$settings
                if (!is.null(settings$r$lsp)) settings <- settings$r$lsp
                if (!is.null(settings$diagnostics)) diagnostics <<- isTRUE(settings$diagnostics)
                if (!is.null(settings$diagnostics_backend)) lint <<- identical(settings$diagnostics_backend, "lintr")
                if (!identical(previous, diagnostic_mode()) && isTRUE(capabilities$workspace$diagnostics$refreshSupport)) {
                    refresh_id <<- refresh_id + 1
                    return(Request$new(paste0("diagnostics-", refresh_id), "workspace/diagnostic/refresh"))
                }
            }
            if (!is.null(uri)) embedded_text_sync(workspace_for(uri), method, uri, params)
            return(NULL)
        }
        provide(method, id, params, uri)
    }

    function(message) {
        if (!is.character(message) || length(message) != 1L || is.na(message)) {
            stop("Expected one unframed JSON-RPC message string")
        }
        packet <- tryCatch(jsonlite::fromJSON(message, simplifyVector = FALSE), error = identity)
        if (inherits(packet, "error")) {
            return(as.character(ResponseErrorMessage$new(NULL, "ParseError", conditionMessage(packet))$to_json()))
        }
        if (is.list(packet) && is.null(packet$method) && !is.null(packet$id) &&
                any(c("result", "error") %in% names(packet))) return(character())
        if (!is.list(packet) || !identical(packet$jsonrpc, "2.0") ||
                !is.character(packet$method) || length(packet$method) != 1L) {
            return(as.character(ResponseErrorMessage$new(NULL, "InvalidRequest", "Expected a JSON-RPC method")$to_json()))
        }
        reply <- tryCatch(dispatch(packet), error = function(error) {
            if (!is.null(packet$id)) ResponseErrorMessage$new(packet$id, "InternalError", conditionMessage(error))
        })
        if (is.null(reply)) character() else as.character(reply$to_json())
    }
}

embedded_text_sync <- function(workspace, method, uri, params) {
    item <- params$textDocument
    documents <- workspace$documents
    doc <- documents$get(uri, NULL)
    if (method == "textDocument/didOpen") {
        doc <- Document$new(uri, item$languageId, item$version, split_document_lines(item$text))
        doc$did_open()
        documents$set(uri, doc)
    } else if (!is.null(doc)) {
        if (method == "textDocument/didChange" && item$version > doc$version) {
            doc$apply_content_changes(item$version, params$contentChanges)
        } else if (method == "textDocument/didSave" && !is.null(params$text)) {
            doc$set_content(doc$version, split_document_lines(params$text))
        } else if (method == "textDocument/didClose") {
            doc$did_close()
            documents$remove(uri)
            workspace$update_loaded_packages()
        }
    }
    invisible(NULL)
}

# Coalesce consecutive edits. A request may resolve a symbol in any other open
# buffer in the same workspace, including one which has never had a request.
embedded_parse_open <- function(workspace) {
    changed <- FALSE
    for (doc in workspace$documents$values()) {
        if (identical(doc$version, doc$parse_data$version) &&
                identical(doc$content, doc$parsed_content)) next
        parsed <- parse_document(doc$uri, doc$content, doc$is_rmarkdown, include_xml = FALSE,
            attachment_exclusions = doc$attachment_exclusions)
        parsed$version <- doc$version
        workspace$update_parse_data(doc$uri, parsed)
        doc$loaded_packages <- normalize_package_request(parsed$packages)
        changed <- TRUE
    }
    if (changed) workspace$update_loaded_packages()
    invisible(NULL)
}

# A native server can ask its client to open a temporary file. A worker's
# private filesystem is invisible to the editor. Expose only open buffers and
# generated documents which the client negotiated and can actually retrieve.
embedded_definition <- function(id, uri, workspace, document, point, generated) {
    reply <- definition_reply(id, uri, workspace, document, point, rootPath = "",
        text_document_content = generated)
    target <- reply$result
    if (is.null(target)) return(reply)
    single <- !is.null(target$uri)
    locations <- if (single) list(target) else target
    visible <- Filter(function(location) {
        is.character(location$uri) && (workspace$documents$has(location$uri) ||
            (!is.null(generated) && !is.null(generated$get(location$uri))))
    }, locations)
    if (!length(visible)) return(Response$new(id))
    Response$new(id, if (single) visible[[1L]] else visible)
}

# Pull only the requested buffer. Parse errors are available even in a minimal
# library. Explicit linters and parse_settings = FALSE keep project .lintr
# expressions outside a runtime created solely for editor analysis.
embedded_diagnostics <- function(document, enabled, use_lintr, previous_id) {
    full <- list(kind = "full", items = list())
    if (!enabled || is.null(document)) return(full)
    use_lintr <- use_lintr && requireNamespace("lintr", quietly = TRUE)
    content <- if (document$is_rmarkdown) literate_r_content(document$content, document$regions) else document$content
    full$resultId <- paste0(get_content_hash(content), if (use_lintr) "-lintr" else "-syntax")
    if (identical(full$resultId, previous_id)) return(list(kind = "unchanged", resultId = full$resultId))

    error <- tryCatch({ parse(text = content, srcfile = srcfilecopy("<text>", content)); NULL }, error = identity)
    if (!is.null(error)) {
        full$items <- list(embedded_syntax_diagnostic(error, document, content))
    } else if (use_lintr) {
        lints <- lintr::lint(text = paste(content, collapse = "\n"),
            linters = list(infix_spaces_linter = lintr::infix_spaces_linter()),
            cache = FALSE, parse_settings = FALSE)
        full$items <- unname(lapply(lints, diagnostic_from_lint, content = content))
    }
    full
}

embedded_syntax_diagnostic <- function(error, document, content) {
    header <- strsplit(conditionMessage(error), "\n", fixed = TRUE)[[1L]][[1L]]
    parsed <- regmatches(header, regexec("^<text>:(\\d+):(\\d+): (.*)$", header, perl = TRUE))[[1L]]
    line <- column <- 0L
    if (length(parsed)) {
        row <- as.numeric(parsed[[2L]])
        # R reports unexpected EOF on a synthetic line after the final
        # newline. Editors need an actual position inside the open buffer.
        row <- min(max(1, row), length(content))
        line <- as.integer(row - 1)
        column <- if (as.numeric(parsed[[2L]]) > length(content)) nchar(content[[row]]) else
            index_physical_columns(max(0, as.numeric(parsed[[3L]]) - 1), row, content,
                grepl("\t", content, fixed = TRUE))
        header <- parsed[[4L]]
    }
    width <- nchar(content[[line + 1L]])
    list(source = "R", severity = DiagnosticSeverity$Error, message = header,
        range = range(document$to_lsp_position(line, min(column, width)),
            document$to_lsp_position(line, min(column + 1, width))))
}
