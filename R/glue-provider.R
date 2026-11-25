# Parse only the selected expression. The cache belongs to this accepted host
# snapshot; identical local coordinates in a later edit cannot reuse it.
glue_fragment_document <- function(document, fragment) {
    parsed <- document$parse_data
    key <- paste(fragment$start$row, fragment$start$col, sep = ":")
    cache <- parsed$glue_document_cache
    if (is.null(cache)) {
        cache <- ByteLruCache$new(4 * 1024^2, 16L)
        parsed$glue_document_cache <- cache
    }
    inner <- cache$get(key)
    if (is.null(inner) || !identical(inner$uri, document$uri) || !identical(inner$version, document$version) ||
            !identical(inner$content, fragment$content)) {
        # A .qmd host still contains an ordinary R expression at this point.
        inner <- Document$new("untitled:glue-expression.R", "r", document$version, fragment$content)
        inner$uri <- document$uri
        data <- parse_document(document$uri, fragment$content, include_xml = FALSE)
        data$version <- document$version
        inner$update_parse_data(data)
        cache$set(key, inner, additional_bytes = as.numeric(utils::object.size(as.list(data))))
    }
    inner$requested_packages <- document$requested_packages
    inner$loaded_packages <- document$loaded_packages
    inner
}

glue_cursor_context <- function(document, point, workspace) {
    parsed <- document$parse_data
    if (!length(parsed$glue_templates) || !identical(document$version, parsed$version)) return(NULL)
    fragment <- glue_fragment_at(parsed, point)
    if (is.null(fragment)) return(NULL)
    bindings <- glue_context_at(workspace, document, fragment)
    if (bindings$shadowed) return(NULL)
    if (isTRUE(fragment$lexical_prefix) && is.null(fragment$package)) {
        # A damaged outer call may have no surviving syntax row. Do not guess
        # that a source-defined function of the same name is glue.
        index <- parsed$reference_index
        occurrences <- reference_indices(index, fragment$call_name)
        if (any(index$is_definition[occurrences])) return(NULL)
        owner <- if (is.function(workspace$guess_namespace)) workspace$guess_namespace(fragment$call_name, isf = TRUE, uri = document$uri)
        if (length(owner) && !identical(owner, glue_template_packages[[fragment$call_name]])) return(NULL)
    }
    list(host = document, host_point = point, fragment = fragment, bindings = bindings,
        document = glue_fragment_document(document, fragment), point = glue_inner_point(fragment, point))
}

# A request-local workspace view supplies the expression's syntax to normal R
# providers. Namespace and project lookup still use the host workspace and URI.
glue_provider_workspace <- function(workspace, context) {
    glue_capture_members(workspace, context)
    view <- as.list(workspace)
    documents <- workspace$documents
    view$documents <- as.list(documents)
    view$documents$get <- function(key, ...) {
        if (identical(key, context$host$uri)) context$document else documents$get(key, ...)
    }
    view$get_parse_data <- function(uri) {
        if (identical(uri, context$host$uri)) context$document$parse_data else workspace$get_parse_data(uri)
    }
    view
}

glue_host_range <- function(context, inner_range) {
    point <- function(position) {
        p <- glue_source_point(context$fragment, context$document$from_lsp_position(position))
        if (!is.null(p)) {
            source <- if (is.null(context$source)) context$host else context$source
            source$to_lsp_position(p$row, p$col)
        }
    }
    start <- point(inner_range$start)
    end <- point(inner_range$end)
    if (!is.null(start) && !is.null(end)) range(start, end)
}

# A local assignment or formal inside the expression wins before its glue
# evaluation environment. A named argument, member or qualified token has its
# own R meaning and does not name a glue binding.
glue_symbol_target <- function(workspace, context) {
    inner <- context$document
    p <- context$point
    data <- inner$parse_data
    syntax <- data$syntax_data
    row <- navigation_token(data, p$row + 1L, p$col + 1L)
    if (!length(row) || !row || !syntax$token[[row]] %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL") ||
            navigation_after_dollar(syntax, row)) return(NULL)
    detected <- reference_token_at(inner, data, p)
    if (nzchar(detected$accessor) || !nzchar(detected$token)) return(NULL)
    if (syntax$token[[row]] == "SYMBOL_FUNCTION_CALL") return(glue_cursor_callee(workspace, context, detected$token, row))
    if (length(navigation_definitions(data, p$row + 1L, p$col + 1L, detected$token, token = row, last = TRUE))) {
        return(list(kind = "inner"))
    }
    glue_context_at(workspace, context$host, context$fragment, detected$token)$target
}

glue_cursor_callee <- function(workspace, context, name, row) {
    if (length(row) && row) {
        target <- glue_call_target(workspace, context$document, list(call_row = row), name, list(), lexical_only = TRUE)
        if (!is.null(target)) {
            if (identical(target$kind, "lexical")) target$kind <- "inner"
            return(target)
        }
    }
    glue_context_at(workspace, context$host, context$fragment, name, function_position = TRUE)$target
}

glue_target_source <- function(workspace, context, target) {
    name <- target$name
    inner <- identical(target$kind, "inner")
    if (length(target$callable)) target <- target$callable
    if (identical(target$kind, "package")) {
        signature <- workspace$get_signature(target$name, target$package, uri = context$host$uri)
        if (is.null(signature)) return(NULL)
        if (!identical(name, target$name)) signature <- paste0(quote_completion_name(name), substring(signature, regexpr("(", signature, fixed = TRUE)))
        return(list(text = signature, signature = signature, params = workspace$get_formals(target$name, target$package, uri = context$host$uri),
                documentation = workspace$get_documentation(target$name, target$package, isf = TRUE, uri = context$host$uri)))
    }
    if (is.null(target$range)) return(NULL)
    doc <- if (inner) context$document else if (identical(target$uri, context$host$uri)) context$host else workspace$documents$get(target$uri, NULL)
    if (is.null(doc)) return(NULL)
    data <- doc$parse_data
    syntax <- data$syntax_data
    value <- target$value_row
    text <- NULL
    signature <- NULL
    params <- NULL
    if (length(value) && value > 0L && !is.null(syntax)) {
        closing <- syntax$function_close[[value]]
        if (closing > 0L) {
            header <- get_range_text(doc$content, syntax$line1[[value]], syntax$col1[[value]],
                syntax$line2[[closing]], syntax$col2[[closing]])
            info <- analysis_function_header(doc, header, quote_completion_name(name))
            signature <- info$signature
            params <- info$formals
        } else {
            # A long initializer is not needed to describe its binding.
            line <- syntax$line1[[value]]
            rhs <- substr(doc$line(line), syntax$col1[[value]],
                if (syntax$line2[[value]] == line) syntax$col2[[value]] else syntax$col1[[value]] + 300L)
            operator <- if (target$kind %in% c("named", "field")) " = " else " <- "
            text <- str_trunc(paste0(quote_completion_name(target$name), operator, rhs), 300L)
        }
    }
    if (is.null(signature) && !identical(target$uri, context$host$uri)) {
        signature <- data$signatures[[target$name]]
        if (!is.null(data$functions[[target$name]])) params <- formals(data$functions[[target$name]])
    }
    first <- target$range$start$line + 1L
    if (is.null(text)) text <- if (!is.null(signature)) signature else quote_completion_name(target$name)
    list(text = text, signature = signature, params = params, documentation = navigation_documentation(doc, data, first))
}

glue_hover_reply <- function(id, uri, workspace, context) {
    target <- glue_symbol_target(workspace, context)
    if (!is.null(target) && (!identical(target$kind, "inner") || length(target$range))) {
        source <- glue_target_source(workspace, context, target)
        if (is.null(source)) return(Response$new(id))
        token <- reference_token_at(context$document, context$document$parse_data, context$point)
        at <- lapply(token$range, function(p) context$document$to_lsp_position(p$row, p$col))
        doc <- source$documentation
        if (is.list(doc)) doc <- if (is.null(doc$markdown)) doc$description else doc$markdown
        contents <- c(sprintf("```r\n%s\n```", source$text), doc)
        return(Response$new(id, list(contents = contents, range = glue_host_range(context, at))))
    }
    view <- glue_provider_workspace(workspace, context)
    reply <- hover_reply(id, uri, view, context$document, context$point, embedded = TRUE)
    if (!is.null(reply$result$range)) reply$result$range <- glue_host_range(context, reply$result$range)
    reply
}

glue_definition_reply <- function(id, uri, workspace, context, rootPath, text_document_content) {
    target <- glue_symbol_target(workspace, context)
    if (identical(target$kind, "inner") && length(target$range)) {
        return(Response$new(id, location(uri, glue_host_range(context, target$range))))
    }
    if (identical(target$kind, "package")) {
        view <- glue_provider_workspace(workspace, context)
        # Use the normal virtual-document and tempfile paths with the package
        # chosen in function position, even if the host has a same-name value.
        lookup <- view$get_definition
        view$get_definition <- function(symbol, pkgname = NULL, ...) lookup(symbol, target$package, ...)
        return(symbol_definition_reply(id, uri, view, context$document, context$point, rootPath, text_document_content = text_document_content))
    }
    if (!is.null(target) && !identical(target$kind, "inner")) {
        result <- if (!is.null(target$range)) location(target$uri, target$range)
        return(Response$new(id, result))
    }
    view <- glue_provider_workspace(workspace, context)
    reply <- definition_reply(id, uri, view, context$document, context$point, rootPath,
        text_document_content = text_document_content, embedded = TRUE)
    if (identical(target$kind, "inner") && identical(reply$result$uri, uri)) {
        reply$result$range <- glue_host_range(context, reply$result$range)
    }
    reply
}

glue_signature_reply <- function(id, uri, workspace, context, capabilities) {
    inner <- context$document
    call <- inner$detect_call(context$point, signature = TRUE)
    view <- glue_provider_workspace(workspace, context)
    if (nzchar(call$token) && identical(call$accessor, "") && !is.null(call$opening)) {
        p <- call$opening
        row <- navigation_token(inner$parse_data, p$row + 1L, p$col + 1L)
        target <- glue_cursor_callee(workspace, context, call$token, row)
        if (identical(target$kind, "package")) {
            call$package <- target$package
            call$accessor <- "::"
            return(signature_reply(id, uri, view, inner, context$point, capabilities, call = call, embedded = TRUE))
        }
        if (!is.null(target) && !(identical(target$kind, "unknown") && isTRUE(context$fragment$lexical_prefix))) {
            source <- glue_target_source(workspace, context, target)
            if (is.null(source$signature)) return(Response$new(id, list(signatures = list())))
            none <- signature_no_active_parameter_support(capabilities)
            active <- detect_active_parameter(inner$content, p$row, p$col, context$point$row,
                context$point$col, source$signature, none)
            result <- list(signatures = list(local_signature_information(source$signature, source$documentation)),
                activeSignature = 0)
            if (!is.null(active) || none) result["activeParameter"] <- list(active)
            return(Response$new(id, result))
        }
    }
    signature_reply(id, uri, view, inner, context$point, capabilities, call = call, embedded = TRUE)
}

glue_completion_candidates <- function(workspace, context, token, snippet, limit) {
    bindings <- context$bindings
    blocked <- isTRUE(bindings$blocked) && !isTRUE(context$fragment$lexical_prefix)
    labels <- bindings$names[match_with(bindings$names, token)]
    labels <- utils::head(labels, limit)
    items <- lapply(labels, function(label) {
        list(label = label, kind = CompletionItemKind$Variable, sortText = paste0(sort_prefixes$scope, label),
            detail = "[glue]", insertText = quote_completion_name(label))
    })
    if (!blocked) {
        host <- scope_completion(context$host$uri, workspace, token, context$host_point, snippet, limit)
        host <- completion_context_data(host, context$host$uri, context$host, context$host_point)
        host <- lapply(host, function(item) {
            if (!is.null(item$data$completion_position)) item$data$glue_host_scope <- TRUE
            item
        })
        items <- c(items, host)
    }
    list(items = items, masked = bindings$names, blocked = blocked)
}

# A raw terminator can straddle an edit boundary. Include just enough of
# each neighboring line to catch a match that overlaps the replacement.
glue_edit_closes_raw_string <- function(closer, text, document, span) {
    width <- nchar(closer)
    if (width == 1L) return(FALSE)
    start <- document$from_lsp_position(span$start)
    end <- document$from_lsp_position(span$end)
    before <- substr(document$line0(start$row), max(1L, start$col - width + 2L), start$col)
    after <- substr(document$line0(end$row), end$col + 1L, end$col + width - 1L)
    grepl(closer, paste0(before, text, after), fixed = TRUE)
}

glue_completion_edits <- function(items, context, token) {
    inner <- context$document
    if (isTRUE(token$quoted)) {
        # The decoded prefix can omit spaces, escapes and both backticks.
        # Use the lexer span even for host/mask candidates without a textEdit.
        start <- token$range$start$col
        end <- token$range$end$col
    } else {
        # With a qualified name the replacement starts after the namespace.
        start <- context$point$col - nchar(token$token)
        end <- inner$detect_token(context$point)$range$end$col
    }
    default <- range(inner$to_lsp_position(context$point$row, max(0L, start)),
        inner$to_lsp_position(context$point$row, end))
    default <- glue_host_range(context, default)
    source <- if (is.null(context$source)) context$host else context$source
    closer <- context$fragment$quote
    Filter(Negate(is.null), lapply(items, function(item) {
        edit <- item$textEdit
        span <- if (is.null(edit)) default else glue_host_range(context, edit$range)
        if (is.null(span) || span$start$line != span$end$line) return(NULL)
        inserted <- if (!is.null(edit)) edit$newText else if (!is.null(item$insertText)) item$insertText else item$label
        if (identical(item$insertTextFormat, InsertTextFormat$Snippet)) {
            # Function snippets consist of an escaped literal name and a $0
            # tabstop. Escape the literal at the R layer before the snippet layer.
            if (!endsWith(inserted, "($0)")) return(NULL)
            literal <- substr(inserted, 1L, nchar(inserted) - 4L)
            literal <- gsub("\\\\([\\\\$}])", "\\1", literal, perl = TRUE)
            literal <- glue_encode_text(context$fragment, literal)
            if (glue_edit_closes_raw_string(closer, paste0(literal, "()"), source, span)) return(NULL)
            inserted <- paste0(escape_completion_snippet(literal), "($0)")
        } else {
            inserted <- glue_encode_text(context$fragment, inserted)
            if (glue_edit_closes_raw_string(closer, inserted, source, span)) {
                if (!identical(item$kind, CompletionItemKind$Value) || !identical(inserted, encodeString(item$label, quote = '"'))) return(NULL)
                # String argument choices can use the other quote. Escape the
                # outer quote in the value itself so the raw closer cannot recur.
                if (endsWith(closer, '"')) {
                    inserted <- gsub('"', "\\x22", encodeString(item$label, quote = "'"), fixed = TRUE)
                } else {
                    inserted <- gsub("'", "\\x27", inserted, fixed = TRUE)
                }
            }
        }
        item$textEdit <- text_edit(span, inserted)
        item$insertText <- NULL
        if (isTRUE(token$quoted) && is.null(item$filterText)) {
            item$filterText <- if (is.null(item$data$package) && startsWith(item$label, "`")) item$label else encodeString(item$label, quote = "`")
        }
        if (!is.null(item$data$completion_position)) {
            if (is.null(context$source)) {
                item$data$completion_position <- context$host_point
                item$data$completion_version <- context$host$version
            } else {
                item$data$completion_position <- context$source_point
                item$data$completion_version <- context$source$version
                item$data$glue_identifier_edit <- TRUE
            }
        }
        item
    }))
}

glue_completion_reply <- function(id, uri, workspace, context, capabilities, opener = FALSE) {
    view <- glue_provider_workspace(workspace, context)
    inner <- context$document
    point <- context$point
    token <- completion_token(inner, point)
    s <- inner$parse_data$syntax_data
    row <- navigation_token(inner$parse_data, point$row + 1L, point$col + 1L)
    if (length(row) && row && s$token[[row]] == "COMMENT") {
        return(Response$new(id, list(isIncomplete = FALSE, items = list())))
    }
    member <- if (isTRUE(lsp_settings$get("member_completion"))) member_cursor(inner, point)
    snippet <- isTRUE(capabilities$completionItem$snippetSupport) && lsp_settings$get("snippet_support")
    if (!is.null(member)) {
        items <- member_completion(uri, view, inner, point, snippet, lsp_settings$get("max_completions"),
            completion_label_details_enabled(capabilities))
        result <- list(isIncomplete = isTRUE(attr(items, "truncated")), items = glue_completion_edits(items, context, token))
        return(Response$new(id, result))
    }
    reply <- completion_reply(id, uri, view, inner, point, capabilities, embedded = TRUE)
    items <- lapply(reply$result$items, function(item) {
        if (identical(item$data$uri, uri) && !is.null(item$data$line)) {
            source <- glue_source_point(context$fragment, list(row = item$data$line - 1L, col = 0L))
            if (!is.null(source)) item$data$line <- source$row + 1L
        }
        item
    })
    if (check_scope(uri, inner, point) && is.null(token$package)) {
        nmax <- lsp_settings$get("max_completions")
        snippet <- snippet && !completion_has_call_parens(inner, token)
        candidates <- glue_completion_candidates(workspace, context, token$token, snippet, nmax)
        # Start the suggestion session before an identifier has been typed.
        # The ordinary R provider consults workspace names only for a prefix.
        if (opener && !candidates$blocked) {
            items <- c(items, workspace_completion(workspace, "", package = WORKSPACE, snippet_support = snippet,
                    limit = nmax, uri = uri), constant_completion(""))
        }
        single_word <- identical(context$fragment$content, token$token)
        group <- vapply(items, function(item) {
            type <- item$data$type
            # The fragment lexer also offers the unfinished identifier itself.
            # VS Code ranks that no-op exact match above useful prefix matches.
            if (single_word && identical(item$kind, CompletionItemKind$Text) && identical(item$label, token$token)) return(0L)
            # Declarations inside the R expression precede its glue environment.
            declared <- identical(item$data$uri, uri) && isTRUE(type %in% c("function", "nonfunction"))
            if (declared || identical(type, "scope")) return(1L)
            if (isTRUE(type %in% c("parameter", "argument_value", "member", "s3_parameter"))) return(2L)
            if (!isTRUE(candidates$blocked) && !item$label %in% candidates$masked) 2L else 0L
        }, integer(1L))
        items <- c(items[group == 1L], candidates$items, items[group == 2L])
        call <- inner$detect_call(point)
        if (!is.null(call$opening) && nzchar(call$token) && is.null(call$package)) {
            target <- glue_cursor_callee(workspace, context, call$token, row)
            if (!is.null(target) && !identical(target$kind, "package")) {
                # A masked function may have different formals from the workspace
                # function of the same name. Its defaults are read as syntax.
                items <- Filter(function(item) !isTRUE(item$data$type %in% c("argument_value", "parameter", "s3_parameter")), items)
                source <- glue_target_source(workspace, context, target)
                parameters <- names(source$params)
                matched <- vapply(parameters[match_with(parameters, token$token)], quote_completion_name, character(1L))
                meta <- list(type = "parameter", funct = call$token, uri = target$uri, line = target$range$start$line + 1L)
                args <- argument_name_items(unname(matched), meta)
                for (i in seq_along(parameters)) {
                    values <- extract_default_values(source$params[[i]])
                    args <- c(args, argument_value_items(values, call$token, NULL, parameters[[i]], token$token, uri))
                }
                items <- c(args, items)
            }
        }
        if (length(items)) {
            items <- items[!duplicated(completion_matching_names(items))]
        }
        if (length(items) > nmax) {
            selected <- completion_select_indices(completion_matching_names(items),
                vapply(items, function(item) if (is.null(item$sortText)) item$label else item$sortText, character(1L)),
                token$token, nmax)
            items <- items[selected]
            reply$result$isIncomplete <- TRUE
        }
    }
    reply$result$items <- glue_completion_edits(items, context, token)
    if (opener) reply$result$isIncomplete <- length(reply$result$items) > 0L
    reply
}

# Before the debounce finishes the current text is authoritative. An earlier
# snapshot can identify an affected template, but cannot supply its offsets.
glue_request_pending <- function(document, point, opener = FALSE) {
    if (identical(document$version, document$parse_data$version) || !check_r_region(document, point)) return(FALSE)
    parsed <- document$parse_data
    if (length(parsed$glue_templates) && !is.null(glue_fragment_at(parsed, point))) return(TRUE)
    if (!opener && !enclosed_by_quotes(document, point)) return(FALSE)
    isTRUE(document$detect_call(point, signature = TRUE)$token %in% names(glue_template_packages))
}

glue_request_waiting <- function(self, uri, workspace, context, method, request_id = NULL) {
    inner <- context$document
    point <- context$point
    signature <- identical(method, "textDocument/signatureHelp")
    hover <- identical(method, "textDocument/hover")
    call <- inner$detect_call(point, signature = signature)
    if (index_context_waiting(self, workspace, inner, point, hover = hover, call = if (signature) call)) return(TRUE)
    if (!hover && member_metadata_waiting(self, uri, inner, point, call = call)) return(TRUE)
    if (identical(method, "textDocument/definition")) return(FALSE)
    s3_metadata_waiting(self, uri, inner, point, hover = hover, call = if (signature) call, request_id = request_id)
}

glue_cursor_request <- function(self, id, params, method, workspace, document, point) {
    uri <- document$uri
    completion <- identical(method, "textDocument/completion")
    opener <- completion && identical(params$context$triggerCharacter, "{")
    if (completion && !is.null(document$glue_edit_line)) {
        verified <- glue_identifier_context(workspace, document, point)
        if (!is.null(verified) && (!opener || verified$point$col == 0L) && !source_context_waiting(workspace, document)) {
            reply <- glue_completion_reply(id, uri, workspace, verified, self$ClientCapabilities$textDocument$completion, opener)
            reply$result$isIncomplete <- length(reply$result$items) > 0L || !opener
            self$deliver(reply)
            return(TRUE)
        }
    }
    pending <- glue_request_pending(document, point, opener)
    context <- if (!pending) glue_cursor_context(document, point, workspace)
    if (opener && !pending && (is.null(context) || context$point$row != 0L || context$point$col != 0L ||
                substr(document$line0(point$row), point$col, point$col) != "{")) {
        self$deliver(Response$new(id, list(isIncomplete = FALSE, items = list())))
        return(TRUE)
    }
    if (!pending && is.null(context)) return(FALSE)
    if (pending || glue_request_waiting(self, uri, workspace, context, method, id)) {
        enqueue_latest_reply(self, uri, method, list(id = id, version = document$version, params = params))
        if (is.function(self$resolve_task_manager$promote)) self$resolve_task_manager$promote(uri)
        if (is.function(self$member_task_manager$promote)) self$member_task_manager$promote(uri)
        return(TRUE)
    }
    reply <- switch(method,
        "textDocument/completion" = glue_completion_reply(id, uri, workspace, context, self$ClientCapabilities$textDocument$completion, opener),
        "textDocument/hover" = glue_hover_reply(id, uri, workspace, context),
        "textDocument/signatureHelp" = glue_signature_reply(id, uri, workspace, context, self$ClientCapabilities),
        "textDocument/definition" = glue_definition_reply(id, uri, workspace, context,
            get_root_path_for_uri(uri, workspace$root), self$text_document_content))
    self$deliver(reply)
    TRUE
}

# The normal member engine reads a history of parsed right-hand sides. Supply
# only the receiver roots referenced by this expression, in its glue scope.
# Their declaration ranges are already physical, including UTF-16 conversion.
glue_capture_members <- function(workspace, context) {
    inner <- context$document
    data <- inner$parse_data
    if (!isTRUE(lsp_settings$get("member_completion")) ||
            !any(grepl("[$@]|\\[\\[", inner$content))) return()
    revision <- list(workspace$s3_revision, workspace$index$revision)
    cache <- data$glue_member_cache
    if (!identical(cache$revision, revision)) {
        originals <- if (is.null(cache)) data$member_data$bindings else cache$originals
        data$member_data$bindings <- originals
        cache <- list(revision = revision, originals = originals, seen = character())
        inner$member_receivers$clear()
    }
    syntax <- data$syntax_data
    names <- reference_symbol_names(syntax$text[syntax$token == "SYMBOL"])
    cursor <- member_cursor(inner, context$point)
    if (!is.null(cursor$operator)) {
        receiver <- inner$find_token(cursor$operator_row, cursor$operator - 1L, forward = FALSE)
        names <- c(names, receiver$token)
    }
    pending <- setdiff(unique(names[nzchar(names)]), cache$seen)
    for (name in utils::head(pending, 64L)) {
        cache$seen <- c(cache$seen, name)
        target <- glue_context_at(workspace, context$host, context$fragment, name)$target
        if (!length(target$value_row) || !target$value_row) next
        doc <- if (identical(target$uri, context$host$uri)) context$host else workspace$documents$get(target$uri, NULL)
        if (is.null(doc)) next
        tree <- doc$parse_data$syntax_data
        value <- target$value_row
        first <- tree$line1[[value]]
        last <- tree$line2[[value]]
        if (last - first > 256L) next
        start <- tree$col1[[value]]
        code <- get_range_text(doc$content, first, start, last, tree$col2[[value]])
        if (sum(nchar(code, type = "bytes")) > 65536L) next
        expr <- tryCatch(parse(text = normalize_parse_content(code), keep.source = TRUE), error = function(e) NULL)
        if (length(expr) != 1L) next
        expr <- member_source_origins(expr, NULL, doc$uri, first - 1L, start - 1L, source_document = doc)
        entry <- list(expr = expr[[1L]], start = c(-1L, 0L), end = c(-1L, 0L))
        data$member_data$bindings[[name]] <- c(list(entry), cache$originals[[name]])
    }
    data$glue_member_cache <- cache
}

# Verify an inserted "{name}" or "{name" against a directly quoted template
# in the accepted syntax. Rescan that literal to distinguish escaped braces,
# strings within R expressions and nested blocks from a new interpolation.
glue_inserted_fragment <- function(document, point, start, previous, line, name) {
    parsed <- document$parse_data
    before <- start - 1L
    if (before < 0L || !startsWith(line, substr(previous, 1L, before))) return(NULL)
    width <- nchar(line) - nchar(previous)
    if (width < 1L || width > 1026L) return(NULL)
    insert <- substr(line, start, before + width)
    if (!insert %in% c(paste0("{", name), paste0("{", name, "}")) ||
            substring(line, start + width) != substring(previous, start)) return(NULL)
    s <- parsed$syntax_data
    literal <- navigation_token(parsed, point$row + 1L, start, prefer_start = "STR_CONST")
    if (!length(literal) || !literal || s$token[[literal]] != "STR_CONST") return(NULL)
    value <- s$parent_row[[literal]]
    if (!value) return(NULL)
    row <- s$parent_row[[value]]
    if (!row) return(NULL)
    head <- s$children[[s$child_start[[row]]]]
    if (s$child_start[[head]] > s$child_end[[head]]) return(NULL)
    symbol <- s$children[[s$child_end[[head]]]]
    if (s$token[[symbol]] != "SYMBOL_FUNCTION_CALL") return(NULL)
    calls <- glue_template_calls(s, symbol)
    if (!length(calls) || !calls[[1L]]$all_direct || !identical(calls[[1L]]$literals, literal)) return(NULL)
    first <- s$line1[[literal]]
    last <- s$line2[[literal]]
    if (last - first > 32L || sum(nchar(document$parsed_content[first:last], type = "bytes")) > 16384L) return(NULL)
    lines <- glue_literal_lines(s, literal, document$parsed_content)
    i <- point$row - first + 2L
    offset <- before - if (i == 1L) s$col1[[literal]] - 1L else 0L
    text <- lines[[i]]
    closer <- glue_raw_closer(lines[[1L]])
    closing_width <- if (is.null(closer)) 1L else nchar(closer)
    opening_width <- closing_width + !is.null(closer)
    if ((i == 1L && offset < opening_width) || (i == length(lines) && offset > nchar(text) - closing_width)) return(NULL)
    if (!is.null(closer)) {
        changed <- range(document$to_lsp_position(point$row, before), document$to_lsp_position(point$row, before + width))
        if (glue_edit_closes_raw_string(closer, insert, document, changed)) return(NULL)
    }
    lines[[i]] <- paste0(substr(text, 1L, offset), insert, substring(text, offset + 1L))
    decoded <- glue_decode_literal(lines, first - 1L, s$col1[[literal]])
    if (!is.null(decoded)) for (fragment in glue_scan_call(list(decoded), row)) {
        if (fragment$start$row == point$row && fragment$start$col == start && identical(fragment$content, name)) return(fragment)
    }
    NULL
}

# A bare identifier has no declarations or nested calls. When its spelling is
# the sole change to a mapped template, all enclosing syntax and lexical facts
# remain valid. Read binding positions against their original source, and
# construct all completion edits against the current source and version.
glue_identifier_context <- function(workspace, document, point) {
    if (is.null(document$glue_edit_line) || point$row != document$glue_edit_line) return(NULL)
    parsed <- document$parse_data
    if (is.null(parsed$syntax_data) || identical(parsed$version, document$version) ||
            point$row >= length(document$parsed_content)) return(NULL)
    line <- document$line0(point$row)
    previous <- document$parsed_content[[point$row + 1L]]
    # Bound synchronous verification independently of buffer and line size.
    if (max(nchar(c(line, previous), type = "bytes")) > 65536L) return(NULL)
    before <- substr(line, 1L, point$col)
    prefix <- regexpr("[A-Za-z0-9._]+$", before, perl = TRUE)[[1L]]
    start <- if (prefix > 0L) prefix - 1L else point$col
    anchor <- list(row = point$row, col = start)
    fragment <- glue_fragment_at(parsed, anchor)
    simple <- "^([A-Za-z][A-Za-z0-9._]*)?$"
    if (!is.null(fragment) && isTRUE(fragment$complete) && fragment$call_row && is.null(fragment$boundaries) &&
            fragment$start$col == start && fragment$start$row == point$row && fragment$end$row == point$row) {
        if (!grepl(simple, fragment$content)) return(NULL)
        suffix <- substring(previous, fragment$end$col + 1L)
        end <- nchar(line) - nchar(suffix)
        if (end < point$col || end - start > 1024L || !endsWith(line, suffix) ||
                !startsWith(line, substr(previous, 1L, start))) return(NULL)
        name <- substr(line, start + 1L, end)
        if (!grepl(simple, name)) return(NULL)
    } else {
        end <- point$col
        name <- substr(line, start + 1L, end)
        if (!grepl(simple, name)) return(NULL)
        fragment <- glue_inserted_fragment(document, point, start, previous, line, name)
        if (is.null(fragment)) return(NULL)
        anchor$col <- start - 1L
    }

    original <- document$clone(deep = FALSE)
    original$content <- document$parsed_content
    original$version <- parsed$version
    bindings <- glue_context_at(workspace, original, fragment)
    if (bindings$shadowed) return(NULL)
    fragment$content <- name
    fragment$length <- end - start
    fragment$end$col <- end
    list(host = original, host_point = anchor, source = document, source_point = point,
        fragment = fragment, bindings = bindings, document = glue_fragment_document(document, fragment),
        point = list(row = 0L, col = point$col - start))
}
