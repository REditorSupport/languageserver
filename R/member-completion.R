# Document indexes contain syntax and positions, never user values. Recovery
# parses a bounded current expression; earlier statements come from the index.
member_document_index <- function(content, parsed = NULL) {
    items <- list()
    append <- function(expr, start, end) {
        items[[length(items) + 1L]] <<- list(expr = expr, start = start, end = end)
    }
    if (!is.null(parsed)) {
        refs <- attr(parsed, "srcref")
        for (i in seq_along(parsed)) {
            ref <- refs[[i]]
            append(parsed[[i]], c(ref[[1L]] - 1L, ref[[5L]] - 1L), c(ref[[3L]] - 1L, ref[[6L]]))
        }
    } else {
        # Recover complete statements after unrelated syntax errors. Bounds
        # apply to incomplete runs; valid large documents use the parser above.
        start <- 1L
        for (end in seq_along(content)) {
            if (end - start > 128L) start <- end
            text <- content[seq.int(start, end)]
            if (sum(nchar(text, type = "bytes")) > 65536L) {
                start <- end
                next
            }
            parsed <- tryCatch(parse(text = text, keep.source = TRUE), error = function(e) NULL)
            if (is.null(parsed)) {
                # Discard an invalid single line but retain incomplete calls.
                message <- tryCatch(
                    {
                        parse(text = text)
                        ""
                    },
                    error = conditionMessage
                )
                if (!grepl("unexpected end of input|INCOMPLETE_STRING", message)) start <- end + 1L
                next
            }
            refs <- attr(parsed, "srcref")
            for (i in seq_along(parsed)) {
                ref <- refs[[i]]
                append(
                    parsed[[i]], c(start + ref[[1L]] - 2L, ref[[5L]] - 1L),
                    c(start + ref[[3L]] - 2L, ref[[6L]])
                )
            }
            start <- end + 1L
        }
    }
    bindings <- new.env(hash = TRUE, parent = emptyenv())
    s4 <- list()
    imports <- list()
    effects <- list()
    packages <- character()
    for (item in items) {
        expr <- item$expr
        member_walk(expr, function(node) {
            if (member_head(node, "::") || member_head(node, ":::")) {
                package <- member_name(node[[2L]])
                if (!is.null(package)) packages <<- union(packages, package)
            }
        })
        declaration <- if (member_head(expr, "<-") || member_head(expr, "=")) expr[[3L]] else expr
        if (is.call(declaration)) {
            head <- declaration[[1L]]
            name <- if (member_head(head, "::")) member_name(head[[3L]]) else member_name(head)
            args <- as.list(declaration)[-1L]
            key <- if ("Class" %in% names(args)) "Class" else if ("name" %in% names(args)) "name" else 1L
            class <- if (length(args) && !identical(args[[key]], quote(expr = ))) args[[key]] else NULL
            if (!is.null(name) && name %in% c("setClass", "setClassUnion") &&
                    is.character(class) && length(class) == 1L) {
                s4[[class]] <- c(s4[[class]], list(item))
            }
        }
        if (member_head(expr, "<-") || member_head(expr, "=") || member_head(expr, "->") || member_head(expr, ":=")) {
            lhs <- if (member_head(expr, "->")) expr[[3L]] else expr[[2L]]
            rhs <- if (member_head(expr, "->")) expr[[2L]] else expr[[3L]]
            if (member_head(expr, ":=")) rhs <- expr
            name <- member_name(lhs)
            if (is.null(name) && is.call(lhs)) {
                name <- member_name(lhs[[2L]])
                rhs <- quote(.__unknown_mutation__)
            }
            if (!is.null(name) && nzchar(name)) {
                history <- get0(name, bindings, inherits = FALSE, ifnotfound = list())
                history[[length(history) + 1L]] <- list(expr = rhs, start = item$start, end = item$end)
                assign(name, history, bindings)
            }
        }
        if (member_head(expr, "library") || member_head(expr, "require")) imports[[length(imports) + 1L]] <- item
        if (is.call(expr) && !member_head(expr, "<-") && !member_head(expr, "=") &&
                !member_head(expr, "->") && !member_head(expr, ":=")) {
            for (name in member_assigned_names(expr)) {
                history <- get0(name, bindings, inherits = FALSE, ifnotfound = list())
                history[[length(history) + 1L]] <- list(
                    expr = quote(.__unknown_branch_write__),
                    start = item$start, end = item$end
                )
                assign(name, history, bindings)
            }
            effects[[length(effects) + 1L]] <- item
        }
    }
    list(items = items, bindings = as.list(bindings), imports = imports, effects = effects, s4 = s4,
        packages = packages)
}

member_before <- function(a, b) {
    a[[1L]] < b[[1L]] ||
        (a[[1L]] == b[[1L]] && a[[2L]] <= b[[2L]])
}

member_cursor <- function(document, point) {
    if (!check_r_region(document, point)) {
        return(NULL)
    }
    line <- document$line0(point$row)
    prefix <- substr(line, 1L, point$col)
    # Include a closing backtick in the replacement when editing a quoted name.
    match <- regexec("[$@][ \\t]*(`[^`]*`?|[[:alnum:]_.]*)$", prefix, perl = TRUE)[[1L]]
    operator_row <- point$row
    accessor <- NULL
    if (match[[1L]] < 0L) {
        # The member name can start on a later line, with intervening blank
        # lines or comments. Recovery still validates the operator in the AST.
        match <- regexec("^[ \\t]*(`[^`]*`?|[[:alnum:]_.]*)$", prefix, perl = TRUE)[[1L]]
        if (match[[1L]] < 0L) return(NULL)
        for (row in seq.int(point$row - 1L, max(0L, point$row - 128L))) {
            if (row < 0L || !check_r_region(document, list(row = row, col = 0L))) break
            previous <- document$line0(row)
            if (grepl("^[ \\t]*(#.*)?$", previous)) next
            operator <- regexpr("[$@][ \\t]*(#.*)?$", previous, perl = TRUE)[[1L]]
            if (operator > 0L) {
                operator_row <- row
                accessor <- substr(previous, operator, operator)
            }
            break
        }
        if (is.null(accessor)) return(NULL)
    } else {
        operator <- match[[1L]]
        accessor <- substr(prefix, operator, operator)
    }
    start <- match[[2L]] - 1L
    token <- substr(prefix, start + 1L, point$col)
    quoted <- startsWith(token, "`")
    token <- gsub("^`|`$", "", token)
    rest <- substring(line, point$col + 1L)
    closed <- quoted && endsWith(substr(prefix, start + 1L, point$col), "`") && point$col > start + 1L
    suffix <- regexpr(if (quoted) "^[^`]*`?" else "^[[:alnum:]_.]*", rest, perl = TRUE)
    end <- point$col + if (closed) 0L else attr(suffix, "match.length")[[1L]]
    # The sentinel must be a member of the cursor's AST, not a string/comment.
    list(
        operator = operator, operator_row = operator_row, start = start, end = end, token = token,
        accessor = accessor,
        before = substr(prefix, 1L, start), quoted = quoted
    )
}

member_recover <- function(document, point, cursor, data) {
    content <- if (document$is_rmarkdown) purl(document$content, parseable_only = FALSE) else document$content
    content[[point$row + 1L]] <- cursor$before
    items <- data$items
    # Items are ordered by end position. Avoid scanning a long document on each
    # request; only the bounded expression after the preceding item is parsed.
    lo <- 1L
    hi <- length(items)
    preceding <- integer()
    while (lo <= hi) {
        middle <- as.integer(floor((lo + hi) / 2L))
        if (member_before(items[[middle]]$end, c(cursor$operator_row, cursor$operator - 1L))) {
            preceding <- middle
            lo <- middle + 1L
        } else {
            hi <- middle - 1L
        }
    }
    start <- if (length(preceding)) items[[utils::tail(preceding, 1L)]]$end[[1L]] else 0L
    start <- max(start, point$row - 128L)
    sentinel <- ".__languageserver_member_cursor__"
    lines <- content[seq.int(start + 1L, point$row + 1L)]
    if (length(preceding)) {
        last <- items[[utils::tail(preceding, 1L)]]
        if (last$end[[1L]] == start) {
            lines[[1L]] <- sub(
                "^[ \\t]*;[ \\t]*", "",
                substring(lines[[1L]], last$end[[2L]] + 1L)
            )
        }
    }
    if (sum(nchar(lines, type = "bytes")) > 65536L || any(grepl(sentinel, lines, fixed = TRUE))) {
        return(NULL)
    }
    for (skip in seq_len(min(length(lines), 129L))) {
        local <- lines[seq.int(skip, length(lines))]
        local[[length(local)]] <- paste0(local[[length(local)]], sentinel)
        closers <- missing_closing_delimiters(local)
        parsed <- tryCatch(parse(text = paste0(paste(local, collapse = "\n"), closers)), error = function(e) NULL)
        if (is.null(parsed)) next
        found <- FALSE
        member_walk(parsed, function(node) {
            at_cursor <- if (is.null(cursor$accessor)) {
                is.symbol(node) && identical(member_name(node), sentinel)
            } else {
                member_head(node, cursor$accessor) && identical(member_name(node[[3L]]), sentinel)
            }
            if (at_cursor) found <<- TRUE
        })
        column <- if (skip == 1L && length(preceding) &&
                items[[utils::tail(preceding, 1L)]]$end[[1L]] == start) {
            items[[utils::tail(preceding, 1L)]]$end[[2L]]
        } else {
            0L
        }
        if (found) {
            context <- member_recover_context(document, content, point, cursor, start + skip - 1L, column, sentinel, parsed)
            return(list(
                parsed = parsed, sentinel = sentinel,
                context = context,
                start = c(start + skip - 1L, column)
            ))
        }
        return(NULL)
    }
    NULL
}

member_recover_context <- function(document, content, point, cursor, start, column, sentinel, parsed) {
    # Only R6 declarations need later members for method-body context. Stop at
    # the first complete declaration, before unrelated trailing syntax errors.
    declaration <- FALSE
    member_walk(parsed, function(node) {
        if (is.call(node) && (identical(member_name(node[[1L]]), "R6Class") ||
                    (member_head(node[[1L]], "::") && identical(member_name(node[[1L]][[3L]]), "R6Class")))) declaration <<- TRUE
    })
    if (!declaration) return(NULL)
    last <- min(length(content) - 1L, start + 128L)
    if (document$is_rmarkdown) {
        cell <- literate_r_cell_at(document$regions, point$row)
        last <- min(last, cell$body_end - 1L)
    }
    full <- content[seq.int(start + 1L, last + 1L)]
    offset <- point$row - start + 1L
    full[[offset]] <- paste0(cursor$before, sentinel, substring(document$line0(point$row), cursor$end + 1L))
    if (column > 0L) full[[1L]] <- sub("^[ \\t]*;[ \\t]*", "", substring(full[[1L]], column + 1L))
    for (end in seq.int(offset, length(full))) {
        lines <- full[seq_len(end)]
        if (sum(nchar(lines, type = "bytes")) > 65536L) break
        context <- tryCatch(parse(text = lines), error = function(e) NULL)
        if (!is.null(context)) return(context)
    }
    NULL
}

member_context_index <- function(workspace, uri, document, at, parsed = NULL) {
    metadata <- if (!is.null(workspace$member_metadata)) workspace$member_metadata else NULL
    # Choose the extractor associated with roots actually referenced here. The
    # core does not recognize package names, class names, or wrapper conventions.
    referenced <- character()
    member_walk(parsed, function(node) {
        if (is.symbol(node)) referenced <<- c(referenced, as.character(node))
    })
    # Follow a bounded set of referenced assignments (q may have a package
    # receiver several aliases back). Do this before choosing an extractor.
    seen <- character()
    for (pass in seq_len(16L)) {
        pending <- setdiff(unique(referenced), seen)
        if (!length(pending) || length(seen) > 256L) break
        seen <- c(seen, pending)
        for (name in pending) {
            history <- document$parse_data$member_data$bindings[[name]]
            # A dangling $ can make the following assignment parse as a
            # member write. Later bindings must not hide the receiver's package.
            history <- Filter(function(item) member_before(item$end, at), history)
            for (item in utils::tail(history, 1L)) {
                member_walk(item$expr, function(node) {
                    if (is.symbol(node)) referenced <<- c(referenced, as.character(node))
                })
            }
        }
    }
    index <- member_generic_index("")
    selected <- NULL
    package_catalogs <- list()
    if (!is.null(metadata)) {
        fallback <- NULL
        for (package in metadata$keys()) {
            candidate <- if (is.function(metadata$catalog)) metadata$catalog(package) else metadata$get(package)
            package_catalogs[package] <- list(candidate)
            # A receiver root identifies its package more precisely than an
            # exported name used as a member or argument (for example filter).
            # Cache order must not let those incidental names hide the root.
            if (length(intersect(referenced, names(candidate$roots))) || package %in% referenced) {
                selected <- metadata$get(package)
                index <- list2env(as.list(selected), parent = emptyenv())
                fallback <- NULL
                break
            }
            if (is.null(fallback) && length(intersect(referenced, candidate$exports))) fallback <- package
        }
        if (!is.null(fallback)) {
            selected <- metadata$get(fallback)
            index <- list2env(as.list(selected), parent = emptyenv())
        }
    }
    index$document_bindings <- document$parse_data$member_data$bindings
    index$methods_attached <- TRUE
    index$attached_roots <- list()
    index$attached_s4 <- list()
    index$document_s4 <- document$parse_data$member_data$s4
    index$namespace_indices <- list()
    index$namespace_metadata <- metadata
    index$s4_dependencies <- list()
    if (!is.null(metadata)) {
        for (package in metadata$keys()) {
            package_index <- package_catalogs[[package]]
            if (is.null(package_index)) {
                package_index <- if (is.function(metadata$catalog)) metadata$catalog(package) else metadata$get(package)
            }
            # Full indexes are restored only if inference follows this package.
            # The chosen receiver index is already decoded for this request.
            if (identical(package, index$package)) {
                package_index <- as.list(selected)
            } else if (is.function(metadata$catalog)) {
                package_index$.metadata_lazy <- TRUE
            }
            index$namespace_indices[package] <- list(package_index)
            for (dependency in names(package_index$s7_dependencies)) {
                if (!is.null(index$namespace_indices[[dependency]])) next
                index$namespace_indices[dependency] <- package_index$s7_dependencies[dependency]
            }
            if (!is.null(package_index$s4_dependencies)) {
                index$s4_dependencies <- utils::modifyList(index$s4_dependencies, package_index$s4_dependencies)
            }
            for (name in names(package_index$namespace_roots)) index$namespace_roots[name] <- list(package_index$namespace_roots[[name]])
        }
    }
    data <- document$parse_data$member_data
    if (is.null(data)) {
        return(index)
    }
    for (item in data$imports) {
        if (!member_before(item$end, at)) next
        expr <- item$expr
        if (member_head(expr, "library") || member_head(expr, "require")) {
            package <- member_name(expr[[2L]])
            if (!is.null(package) && package %in% names(index$namespace_indices)) {
                index$attached_roots <- utils::modifyList(index$attached_roots, index$namespace_indices[[package]]$roots)
                package_index <- index$namespace_indices[[package]]
                if (!is.null(package_index$s4_classes)) {
                    index$attached_s4 <- utils::modifyList(index$attached_s4, package_index$s4_classes)
                }
                functions <- if (isTRUE(package_index$.metadata_lazy)) {
                    package_index$functions
                } else {
                    Filter(function(name) member_head(package_index$definitions[[name]], "function"),
                        intersect(package_index$exports, names(package_index$definitions)))
                }
                for (name in functions) {
                    if (is.null(index$attached_roots[[name]]$s4_generator) &&
                            is.null(index$attached_roots[[name]]$s7_generator)) {
                        index$attached_roots[name] <- list(member_value(function_key = name, metadata = package))
                    }
                }
            }
            if (identical(package, "R6")) index$r6_attached <- TRUE
            if (identical(package, "S7")) index$s7_attached <- TRUE
        }
    }
    index
}

member_resolve_document <- function(name, index, bindings, budget, depth, trail) {
    if (depth > 64L) return(member_value(reason = "binding_recursion"))
    history <- index$document_bindings[[name]]
    at <- bindings$.__member_position__
    if (is.null(at)) at <- c(Inf, Inf)
    candidates <- which(vapply(history, function(item) member_before(item$end, at), logical(1L)))
    if (!length(candidates)) {
        return(if (is.null(index$attached_roots[[name]])) {
            member_value(reason = "not_yet_bound")
        } else {
            index$attached_roots[[name]]
        })
    }
    position <- utils::tail(candidates, 1L)
    item <- history[[position]]
    id <- paste0("binding:", name, ":", position)
    if (id %in% trail) {
        return(member_value(reason = "binding_cycle"))
    }
    env <- bindings
    if (!member_head(item$expr, "function")) env$.__member_position__ <- item$start
    expr <- item$expr
    if (member_head(expr, ":=")) {
        bound <- member_s7_bind(expr, index, env)
        if (is.null(bound)) return(member_resolve_document(name, index, env, budget, depth + 1L, c(trail, id)))
        expr <- bound[[3L]]
    }
    value <- member_infer(expr, index, env,
        depth = depth + 1L,
        trail = c(trail, id), budget = budget
    )
    value
}

member_cursor_value <- function(parsed, sentinel, index, bindings, budget, name = NULL, accessor = "$", context = NULL) {
    result <- NULL
    has_cursor <- function(node) {
        found <- FALSE
        member_walk(node, function(child) {
            if (member_head(child, accessor) && identical(member_name(child[[3L]]), sentinel)) found <<- TRUE
        })
        found
    }
    visit <- function(node, env) {
        if (is.null(accessor) && is.symbol(node) && identical(member_name(node), sentinel)) {
            result <<- member_infer(as.name(name), index, env, budget = budget)
            return(invisible(NULL))
        }
        if (!is.call(node) && !is.expression(node)) {
            return(invisible(NULL))
        }
        if (is.null(accessor) && member_head(node, "::") && identical(member_name(node[[3L]]), sentinel)) {
            node[[3L]] <- as.name(name)
            result <<- member_infer(node, index, env, budget = budget)
            return(invisible(NULL))
        }
        if (member_head(node, accessor) && identical(member_name(node[[3L]]), sentinel)) {
            if (is.null(name)) {
                result <<- member_infer(node[[2L]], index, env, budget = budget)
            } else {
                receiver <- member_infer(node[[2L]], index, env, budget = budget)
                env$.__member_receiver__ <- receiver
                node[[2L]] <- as.name(".__member_receiver__")
                node[[3L]] <- as.name(name)
                result <<- member_infer(node, index, env, budget = budget)
                key <- member_members(receiver, index, env, accessor)[name]
                if (is.null(result$function_key) && length(key) && !is.na(key[[1L]]) &&
                    !is.null(result$function_expr)) {
                    result$function_key <<- key[[1L]]
                }
            }
            return(invisible(NULL))
        }
        if (member_head(node, "function")) {
            for (name in names(node[[2L]])) env[name] <- list(member_value(reason = "formal"))
            visit(node[[3L]], env)
            return(invisible(NULL))
        }
        if (member_is_r6_call(node, index, env) && has_cursor(node)) {
            scope <- member_r6_context(node, index, env, budget)
            args <- as.list(node)[-1L]
            for (section in c("public", "private", "active")) {
                if (!member_head(args[[section]], "list")) next
                for (method in as.list(args[[section]])[-1L]) {
                    if (member_head(method, "function") && has_cursor(method)) visit(method, utils::modifyList(env, scope))
                }
            }
            return(invisible(NULL))
        }
        if (member_head(node, "{") || is.expression(node)) {
            children <- if (is.expression(node)) as.list(node) else as.list(node)[-1L]
            for (child in children) {
                if (member_head(child, ":=")) {
                    bound <- member_s7_bind(child, index, env)
                    if (!is.null(bound)) child <- bound
                }
                env <- member_s4_effect(child, index, env)
                visit(child, env)
                if (!is.null(result)) break
                if ((member_head(child, "<-") || member_head(child, "=")) && is.symbol(child[[2L]])) {
                    env[as.character(child[[2L]])] <- list(member_infer(child[[3L]], index, env, budget = budget))
                } else {
                    for (name in member_assigned_names(child)) env[name] <- list(member_value(reason = "unknown_local_write"))
                }
            }
        } else {
            # A member can be the callee of a call in the complete context.
            for (child in as.list(node)) visit(child, env)
        }
        invisible(NULL)
    }
    visit(if (is.null(context)) parsed else context, bindings)
    result
}

member_resolve_cursor <- function(uri, workspace, document, point, cursor, name = NULL) {
    if (!identical(document$version, document$parse_data$version) &&
        !is.null(document$parse_data$version)) {
        return(NULL)
    }
    if (is.null(cursor)) {
        return(NULL)
    }
    data <- document$parse_data$member_data
    if (is.null(data)) {
        return(NULL)
    }
    recovered <- member_recover(document, point, cursor, data)
    if (is.null(recovered)) {
        return(NULL)
    }
    index <- member_context_index(workspace, uri, document, recovered$start, recovered$parsed)
    budget <- new.env(parent = emptyenv())
    budget$remaining <- 20000L
    budget$exhausted <- budget$transient <- FALSE
    # Coverage rewrites byte-compiled functions and adds counters to every
    # branch. Allow that overhead while retaining node, depth and time bounds.
    budget$time_limit <- if (identical(Sys.getenv("R_COVR"), "true")) 10 else 0.25
    bindings <- list(.__member_position__ = recovered$start)
    if (length(index$registration_rules)) {
        for (item in data$effects) {
            if (!member_before(item$end, recovered$start)) next
            env <- bindings
            env$.__member_position__ <- item$start
            bindings <- member_registration_effect(item$expr, index, env)
        }
    }
    bindings$.__member_position__ <- recovered$start
    value <- member_cursor_value(recovered$parsed, recovered$sentinel, index, bindings, budget, name, cursor$accessor, recovered$context)
    list(value = value, index = index, bindings = bindings, budget = budget)
}

member_completion <- function(uri, workspace, document, point, snippet_support, limit) {
    cursor <- member_cursor(document, point)
    resolved <- member_resolve_cursor(uri, workspace, document, point, cursor)
    if (is.null(resolved)) {
        return(NULL)
    }
    value <- resolved$value
    index <- resolved$index
    bindings <- resolved$bindings
    budget <- resolved$budget
    if (is.null(value) || !length(value$type)) {
        return(NULL)
    }
    members <- member_members(value, index, bindings, cursor$accessor)
    labels <- as.character(names(members))
    if (!length(labels)) {
        return(list())
    }
    # Filter before pruning so unrelated labels cannot consume the limit.
    matches <- sort(labels[fuzzy_find(labels, cursor$token)])
    keep <- completion_select_indices(matches, matches, cursor$token, limit)
    labels <- matches
    labels <- labels[keep]
    items <- lapply(labels, function(label) {
        shape <- if (identical(cursor$accessor, "@")) {
            member_slot(value, label, index, bindings, budget)
        } else {
            value$fields[[label]]
        }
        key <- members[[label]]
        symbol <- member_symbol_info(label, shape, key, index)
        is_function <- !is.null(shape$function_expr) || !is.null(shape$result_shape) ||
            !is.null(shape$function_key) || (!is.null(key) && !is.na(key))
        inserted <- quote_completion_name(label)
        following <- substring(document$line0(point$row), cursor$end + 1L)
        snippet <- is_function && snippet_support && !startsWith(trimws(following), "(")
        text <- if (snippet) paste0(escape_completion_snippet(inserted), "($0)") else inserted
        signature <- symbol$signature
        documentation_id <- symbol$function_id
        list(
            label = label, kind = if (is_function) CompletionItemKind$Method else CompletionItemKind$Field,
            detail = if (!is.null(signature)) {
                signature
            } else if (identical(cursor$accessor, "@")) {
                paste0(label, ": ", paste(value$slot_types[[label]], collapse = " | "))
            } else {
                "[static member]"
            }, sortText = label,
            filterText = label, insertTextFormat = if (snippet) InsertTextFormat$Snippet else InsertTextFormat$PlainText,
            textEdit = text_edit(range(
                document$to_lsp_position(point$row, cursor$start),
                document$to_lsp_position(point$row, cursor$end)
            ), text),
            data = list(
                type = "member", package = symbol$package, function_id = documentation_id,
                signature = signature, context_uri = uri, generation = index$generation
            )
        )
    })
    if (length(matches) > limit || budget$exhausted) attr(items, "truncated") <- TRUE
    items
}
