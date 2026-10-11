# Document indexes contain syntax and positions, never user values. Recovery
# parses a bounded current expression; earlier statements come from the index.
member_document_index <- function(content, parsed = NULL, syntax = NULL) {
    complete <- !is.null(parsed)
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
            message <- NULL
            parsed <- tryCatch(parse(text = text, keep.source = TRUE), error = function(e) {
                message <<- conditionMessage(e)
                NULL
            })
            if (is.null(parsed)) {
                # Discard an invalid single line but retain incomplete calls.
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
    class_scope <- FALSE
    symbol_scope <- FALSE
    parts <- if (complete) member_function_parts(if (is.null(syntax)) utils::getParseData(parsed) else syntax) else NULL
    for (i in seq_along(items)) {
        item <- items[[i]]
        expr <- item$expr
        syntax_names <- member_syntax_names(expr)
        if (!is.null(parts) && any(syntax_names %in% c("function", "{"))) {
            item$source <- member_source_index(expr, parts)
            items[[i]] <- item
        }
        class_scope <- class_scope || "R6Class" %in% syntax_names
        symbol_scope <- symbol_scope || any(syntax_names %in% c("R6Class", member_s7_intrinsics))
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
        packages = packages, class_scope = class_scope, symbol_scope = symbol_scope)
}

# Complete valid ordinary scopes from their source index. Class analysis is
# needed only when source syntax or package metadata could supply R6 values;
# incomplete scopes still use recovery to discover newly typed local bindings.
member_scope_required <- function(workspace, document, point = NULL) {
    data <- document$parse_data
    if ((!identical(document$version, data$version) && !member_ordinary_request(workspace, document)) ||
            isTRUE(data$parse_error)) return(TRUE)
    if (!identical(data$member_data$class_scope, FALSE)) return(TRUE)
    if (!is.null(point)) return(member_package_scope_required(workspace, data$member_data, "class_scope"))
    metadata <- workspace$member_metadata
    if (is.null(metadata)) return(FALSE)
    for (package in metadata$keys()) {
        # Older snapshots without the hint retain the inference path.
        candidate <- if (is.function(metadata$catalog)) metadata$catalog(package) else metadata$get(package)
        if (!identical(candidate$class_scope, FALSE)) return(TRUE)
    }
    FALSE
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
    # A cursor in a formal default may leave a function without its body.
    # Recover from the following text when closing delimiters alone cannot
    # complete the prefix, using the same bounded forward scan as methods.
    if (is.null(cursor$accessor)) {
        column <- if (length(preceding) && items[[preceding]]$end[[1L]] == start) {
            items[[preceding]]$end[[2L]]
        } else {
            0L
        }
        context <- member_recover_context(document, content, point, cursor, start, column, sentinel, NULL)
        if (!is.null(context)) return(list(
            parsed = context, sentinel = sentinel, context = context, start = c(start, column)
        ))
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
    if (!is.null(parsed) && !declaration) return(NULL)
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

member_context_index <- function(workspace, uri, document, at, parsed = NULL,
    referenced = member_syntax_names(parsed)) {
    metadata <- if (!is.null(workspace$member_metadata)) workspace$member_metadata else NULL
    # Choose the extractor associated with roots actually referenced here. The
    # core does not recognize package names, class names, or wrapper conventions.
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
            position <- member_history_position(history, at)
            if (position) referenced <- union(referenced, member_syntax_names(history[[position]]$expr))
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
    index$document_cache <- new.env(parent = emptyenv())
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
        if (length(expr) < 2L) next
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
    position <- member_history_position(history, at)
    if (!position) {
        return(if (is.null(index$attached_roots[[name]])) {
            member_value(reason = "not_yet_bound")
        } else {
            index$attached_roots[[name]]
        })
    }
    item <- history[[position]]
    id <- paste0("binding:", name, ":", position)
    if (id %in% trail) {
        return(member_value(reason = "binding_cycle"))
    }
    # A source definition in a request's top-level context has stable lexical
    # inputs. Locals/actual arguments use their own lazy binding memos instead.
    memo <- !is.null(index$document_cache) && !length(setdiff(names(bindings), ".__member_position__"))
    memo_key <- paste(id, paste(at, collapse = ":"), sep = ":")
    if (memo && exists(memo_key, index$document_cache, inherits = FALSE)) return(index$document_cache[[memo_key]])
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
    if (memo && length(value$type) && !budget$exhausted && !budget$transient) index$document_cache[[memo_key]] <- value
    value
}

member_cursor_value <- function(parsed, sentinel, index, bindings, budget, name = NULL, accessor = "$", context = NULL,
    path = NULL, source = NULL) {
    result <- NULL
    # Find the cursor once, including formal defaults. Replaying a lexical
    # scope must not recursively visit every earlier RHS or sibling method.
    visit <- function(node, env, path, ast_path = integer()) {
        if (is.null(accessor) && is.symbol(node) && identical(member_name(node), sentinel)) {
            result <<- if (is.null(name)) {
                internal <- c(".__member_position__", ".__s4_classes__", ".__member_properties__",
                    ".__member_context_bindings__", ".__member_context_locals__")
                value <- member_value(type = "environment", fields = env[setdiff(names(env), internal)])
                value$context_bindings <- env$.__member_context_bindings__
                value$context_locals <- unique(env$.__member_context_locals__)
                value
            } else {
                member_infer(as.name(name), index, env, budget = budget)
            }
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
                result <<- if (identical(accessor, "@")) {
                    member_slot(receiver, name, index, env, budget)
                } else {
                    member_access(receiver, name, index, env)
                }
                key <- member_members(receiver, index, env, accessor)[name]
                if (is.null(result$function_key) && length(key) && !is.na(key[[1L]]) &&
                        !is.null(result$function_expr)) {
                    result$function_key <<- key[[1L]]
                }
            }
            return(invisible(NULL))
        }
        if (member_head(node, "function")) {
            for (formal in names(node[[2L]])) env[formal] <- list(member_value(reason = "formal"))
            env$.__member_context_bindings__ <- setdiff(env$.__member_context_bindings__, names(node[[2L]]))
            if (!is.null(env$.__member_context_locals__)) {
                env$.__member_context_locals__ <- c(env$.__member_context_locals__, names(node[[2L]]))
            }
            if (length(path) && path[[1L]] == 2L) {
                visit(node[[2L]][[path[[2L]]]], env, path[-c(1L, 2L)], c(ast_path, 2L, path[[2L]]))
            } else {
                visit(node[[3L]], env, path[-1L], c(ast_path, 3L))
            }
            return(invisible(NULL))
        }
        if (member_is_r6_call(node, index, env)) {
            scope <- member_r6_context(node, index, env, budget)
            scope$.__member_context_bindings__ <- names(scope)
            scope$.__member_context_locals__ <- character()
            section <- names(as.list(node))[path[[1L]]]
            if (length(section) && section %in% c("public", "private", "active") &&
                    member_head(node[[path[[1L]]]], "list") && length(path) >= 2L) {
                method <- node[[path[[1L]]]][[path[[2L]]]]
                if (member_head(method, "function")) visit(method, utils::modifyList(env, scope), path[-c(1L, 2L)],
                    c(ast_path, path[1:2]))
            }
            return(invisible(NULL))
        }
        if (member_head(node, "{") || is.expression(node)) {
            children <- if (is.expression(node)) as.list(node) else as.list(node)[-1L]
            target <- path[[1L]] - if (is.expression(node)) 0L else 1L
            summary <- source$scopes[[member_source_key(ast_path)]]
            selected <- seq_len(target)
            if ((!is.null(accessor) || !is.null(name)) && !is.null(summary)) {
                reads <- if (target <= length(summary$reads)) summary$reads[[target]] else character()
                selected <- member_scope_slice(summary, target, union(name, reads))
            } else if (target > 1L && (!is.null(accessor) || !is.null(name))) {
                keep <- rep(TRUE, target)
                needed <- union(name, member_syntax_names(children[[target]]))
                # Work backwards through binding definitions. Opaque writes
                # and class declarations still replay in their original order.
                for (i in rev(seq_len(target - 1L))) {
                    child <- children[[i]]
                    if ((member_head(child, "<-") || member_head(child, "=")) && is.symbol(child[[2L]])) {
                        assigned <- as.character(child[[2L]])
                        declaration <- any(member_syntax_names(child[[3L]]) %in% c("setClass", "setClassUnion"))
                        if (!assigned %in% needed && !declaration) {
                            keep[[i]] <- FALSE
                        } else {
                            needed <- union(setdiff(needed, assigned), member_syntax_names(child[[3L]]))
                        }
                    } else {
                        # Static S4/S7 declarations may refer to earlier locals.
                        needed <- union(needed, member_syntax_names(child))
                    }
                }
                selected <- which(keep)
            }
            # Hash current values for dependency capture, while retaining
            # ordinary lists as immutable snapshots consumed by inference.
            lookup <- list2env(env, hash = TRUE, parent = emptyenv())
            for (i in selected) {
                child <- children[[i]]
                if (member_head(child, ":=")) {
                    bound <- member_s7_bind(child, index, env)
                    if (!is.null(bound)) child <- bound
                }
                env <- member_s4_effect(child, index, env)
                if (!is.null(env$.__s4_classes__)) lookup$.__s4_classes__ <- env$.__s4_classes__
                if (i == target) {
                    visit(child, env, path[-1L], c(ast_path, path[[1L]]))
                    break
                }
                if ((member_head(child, "<-") || member_head(child, "=")) && is.symbol(child[[2L]])) {
                    assigned <- as.character(child[[2L]])
                    env[assigned] <- list(if (member_head(child[[3L]], "function")) {
                        member_infer(child[[3L]], index, env, budget = budget)
                    } else {
                        member_local_binding(child[[3L]], env,
                            if (!is.null(summary)) summary$reads[[i]] else member_syntax_names(child[[3L]]), lookup)
                    })
                } else {
                    assigned <- if (!is.null(summary)) summary$writes[[i]] else member_assigned_names(child)
                    for (name in assigned) env[name] <- list(member_value(reason = "unknown_local_write"))
                }
                for (name in assigned) lookup[[name]] <- env[[name]]
                env$.__member_context_bindings__ <- setdiff(env$.__member_context_bindings__, assigned)
                if (!is.null(env$.__member_context_locals__)) {
                    env$.__member_context_locals__ <- c(env$.__member_context_locals__, assigned)
                }
            }
        } else {
            # A member can be the callee of a call in the complete context.
            if (length(path)) visit(node[[path[[1L]]]], env, path[-1L], c(ast_path, path[[1L]]))
        }
        invisible(NULL)
    }
    root <- if (is.null(context)) parsed else context
    if (is.null(path)) path <- member_cursor_path(root, sentinel, accessor)
    if (!is.null(path)) visit(root, bindings, path)
    result
}

# Use source references for complete documents so scope lookup also works in
# methods and class declarations larger than the bounded recovery window.
member_recover_scope <- function(document, point, cursor, data) {
    at <- c(point$row, point$col)
    items <- data$items
    lo <- 1L
    hi <- length(items)
    while (lo <= hi) {
        middle <- as.integer(floor((lo + hi) / 2L))
        if (member_before(at, items[[middle]]$end)) hi <- middle - 1L else lo <- middle + 1L
    }
    if (!isTRUE(document$parse_data$parse_error) && lo <= length(items) &&
            member_before(items[[lo]]$start, at)) {
        sentinel <- ".__languageserver_member_cursor__"
        found <- FALSE
        found_path <- NULL
        source_index <- items[[lo]]$source
        source <- NULL
        syntax <- NULL
        contains <- function(ref) {
            !is.null(ref) && member_before(c(ref[[1L]] - 1L, ref[[5L]] - 1L), at) &&
                member_before(at, c(ref[[3L]] - 1L, ref[[6L]]))
        }
        recover_node <- function(node, ref, ast_path) {
            if (!contains(ref)) return(node)
            if (is.null(source)) source <<- normalize_parse_content(document$content, document$is_rmarkdown)
            lines <- source[seq.int(ref[[1L]], ref[[3L]])]
            offset <- point$row - ref[[1L]] + 2L
            lines[[offset]] <- paste0(gsub("\t", " ", cursor$before, fixed = TRUE), sentinel,
                substring(source[[point$row + 1L]], cursor$end + 1L))
            # Trim the source range after replacing the token: its last column
            # moves when the cursor and range end share a line.
            end <- ref[[6L]]
            if (point$row == ref[[3L]] - 1L) end <- end + nchar(sentinel) - (cursor$end - cursor$start)
            lines[[length(lines)]] <- substr(lines[[length(lines)]], 1L, end)
            lines[[1L]] <- substring(lines[[1L]], ref[[5L]])
            parsed <- tryCatch(parse(text = lines), error = function(e) NULL)
            if (length(parsed) == 1L) {
                local_path <- member_cursor_path(parsed[[1L]], sentinel, cursor$accessor)
                if (!is.null(local_path)) {
                    found <<- TRUE
                    found_path <<- c(ast_path, local_path)
                    return(parsed[[1L]])
                }
            }
            node
        }
        insert_cursor <- function(node, ref = NULL, ast_path = 1L) {
            if (!is.call(node)) return(node)
            if (member_head(node, "function")) {
                ref <- if (length(node) >= 4L) node[[4L]]
                if (!contains(ref)) return(node)
                # Function source references cover defaults as well as the
                # body. Locate each part in the parser's child rows so a
                # default closure keeps its own formals and earlier locals.
                descriptor <- source_index$functions[[member_source_key(ast_path)]]
                if (is.null(descriptor)) {
                    if (is.null(syntax)) syntax <<- utils::getParseData(attr(ref, "srcfile"))
                    rows <- which(syntax$token == "expr" & syntax$line1 == ref[[1L]] &
                            syntax$col1 == ref[[5L]] & syntax$line2 == ref[[3L]] & syntax$col2 == ref[[6L]])
                    parts <- syntax[syntax$parent %in% syntax$id[rows] & syntax$token == "expr", ]
                    parts <- parts[order(parts$line1, parts$col1), ]
                    defaults <- which(vapply(as.list(node[[2L]]), function(x) !identical(x, quote(expr = )), logical(1L)))
                    if (nrow(parts) == length(defaults) + 1L) descriptor <- list(defaults = defaults,
                        refs = lapply(seq_len(nrow(parts)), function(i) {
                            part <- parts[i, ]
                            c(part$line1, 0L, part$line2, 0L, part$col1, part$col2)
                        }))
                }
                if (!is.null(descriptor)) {
                    defaults <- descriptor$defaults
                    for (i in seq_along(descriptor$refs)) {
                        part_ref <- descriptor$refs[[i]]
                        if (!contains(part_ref)) next
                        child <- if (i <= length(defaults)) node[[2L]][[defaults[[i]]]] else node[[3L]]
                        child_path <- c(ast_path, if (i <= length(defaults)) c(2L, defaults[[i]]) else 3L)
                        child <- insert_cursor(child, part_ref, child_path)
                        if (!found) child <- recover_node(child, part_ref, child_path)
                        if (i <= length(defaults)) node[[2L]][defaults[[i]]] <- list(child) else node[[3L]] <- child
                        return(node)
                    }
                }
                if (is.null(cursor$accessor)) {
                    node[[3L]] <- as.name(sentinel)
                    found <<- TRUE
                    found_path <<- c(ast_path, 3L)
                }
                return(node)
            }
            if (member_head(node, "{")) {
                ref <- attr(node, "wholeSrcref")
                if (!is.null(ref) && (!member_before(c(ref[[1L]] - 1L, ref[[5L]] - 1L), at) ||
                            !member_before(at, c(ref[[3L]] - 1L, ref[[6L]])))) return(node)
                refs <- attr(node, "srcref")
                if (length(node) > 1L && length(refs) == length(node)) {
                    scope <- source_index$scopes[[member_source_key(ast_path)]]
                    candidates <- seq.int(2L, length(node))
                    if (!is.null(scope$refs)) {
                        low <- 1L
                        high <- length(scope$refs)
                        while (low <= high) {
                            middle <- as.integer(floor((low + high) / 2L))
                            end <- scope$refs[[middle]]
                            if (member_before(at, c(end[[3L]] - 1L, end[[6L]]))) high <- middle - 1L else low <- middle + 1L
                        }
                        candidates <- if (low <= length(scope$refs)) low + 1L else integer()
                    }
                    for (i in candidates) {
                        ref <- refs[[i]]
                        if (member_before(at, c(ref[[1L]] - 1L, ref[[5L]] - 1L))) {
                            node <- as.call(c(as.list(node)[seq_len(i - 1L)], list(as.name(sentinel))))
                            found <<- TRUE
                            found_path <<- c(ast_path, i)
                            return(node)
                        }
                        if (member_before(at, c(ref[[3L]] - 1L, ref[[6L]]))) {
                            child <- insert_cursor(node[[i]], ref, c(ast_path, i))
                            if (!found) child <- recover_node(child, ref, c(ast_path, i))
                            if (!found && is.null(cursor$accessor)) {
                                child <- as.name(sentinel)
                                found_path <<- c(ast_path, i)
                            }
                            if (!found && !is.null(cursor$accessor)) return(node)
                            node <- as.call(c(as.list(node)[seq_len(i - 1L)], list(child)))
                            found <<- TRUE
                            return(node)
                        }
                    }
                    node <- as.call(c(as.list(node), list(as.name(sentinel))))
                    found <<- TRUE
                    found_path <<- c(ast_path, length(node))
                    return(node)
                }
            }
            routes <- source_index$routes[[member_source_key(ast_path)]]
            candidates <- if (length(routes)) as.integer(names(routes)[vapply(routes, contains, logical(1L))]) else seq_along(node)
            for (i in candidates) {
                if (is.call(node[[i]])) node[[i]] <- insert_cursor(node[[i]], ast_path = c(ast_path, i))
                if (found) break
            }
            if (!found) node <- recover_node(node, ref, ast_path)
            node
        }
        expr <- insert_cursor(items[[lo]]$expr)
        if (found) return(list(
            parsed = as.expression(list(expr)), sentinel = sentinel,
            context = NULL, start = items[[lo]]$start, path = found_path, source = source_index
        ))
    }
    member_recover(document, point, cursor, data)
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
    budget <- member_request_budget()
    metadata <- workspace$member_metadata
    cache <- document$member_receivers
    revision <- if (is.null(metadata)) 0L else if (is.function(metadata$revision)) metadata$revision() else NULL
    identity <- list(metadata = if (is.function(metadata$identity)) metadata$identity() else metadata,
        revision = revision)
    if (!identical(document$member_revision, identity)) {
        cache$clear()
        document$member_revision <- identity
    }
    # The receiver does not depend on the member prefix or the call arguments
    # after it. Edits and accepted parses clear this document-local cache.
    key <- paste(point$row, cursor$start, cursor$end, cursor$operator_row,
        cursor$operator, cursor$accessor, if (is.null(cursor$accessor)) name, sep = ":")
    cached <- if (!is.null(revision)) cache$get(key) else NULL
    if (!is.null(cached)) {
        cached <- unserialize(cached)
        index <- member_context_index(workspace, uri, document, cached$start, referenced = cached$referenced)
        bindings <- cached$bindings
        value <- cached$value
    } else {
        recovered <- member_recover_scope(document, point, cursor, data)
        if (is.null(recovered)) return(NULL)
        referenced <- member_source_names(recovered$parsed, recovered$source, recovered$path, name, cursor$accessor)
        index <- member_context_index(workspace, uri, document, recovered$start, referenced = referenced)
        bindings <- list(.__member_position__ = recovered$start)
        if (length(index$registration_rules)) {
            for (item in data$effects) {
                if (!member_before(item$end, recovered$start)) next
                env <- bindings
                env$.__member_position__ <- item$start
                bindings <- member_registration_effect(item$expr, index, env, budget)
            }
        }
        bindings$.__member_position__ <- recovered$start
        value <- member_cursor_value(recovered$parsed, recovered$sentinel, index, bindings, budget,
            if (is.null(cursor$accessor)) name, cursor$accessor, recovered$context,
            recovered$path, recovered$source)
        if (!is.null(revision) && member_receiver_cacheable(value, index, budget)) {
            # Raw storage accounts for closure environments and makes each
            # hit independent of mutable request-local binding memos.
            record <- serialize(list(value = value, bindings = bindings,
                    start = recovered$start, referenced = referenced), NULL)
            cache$set(key, record)
        }
    }
    if (!is.null(cursor$accessor) && !is.null(name)) {
        receiver <- value
        value <- if (identical(cursor$accessor, "@")) member_slot(receiver, name, index, bindings, budget) else
            member_access(receiver, name, index, bindings)
        key <- member_members(receiver, index, bindings, cursor$accessor)[name]
        if (is.null(value$function_key) && length(key) && !is.na(key[[1L]]) && !is.null(value$function_expr)) {
            value$function_key <- key[[1L]]
        }
    }
    list(value = value, index = index, bindings = bindings, budget = budget)
}

member_request_budget <- function() {
    budget <- new.env(parent = emptyenv())
    budget$remaining <- 20000L
    budget$exhausted <- budget$transient <- FALSE
    # Parallel tests compete with parse/metadata workers; coverage also rewrites
    # byte-compiled functions. Allow that overhead while keeping all bounds.
    budget$time_limit <- if (identical(Sys.getenv("R_COVR"), "true")) {
        10
    } else if (identical(Sys.getenv("TESTTHAT"), "true")) {
        5
    } else {
        0.25
    }
    budget$deadline <- proc.time()[[3L]] + budget$time_limit
    budget
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
    following <- substring(document$line0(point$row), cursor$end + 1L)
    call_snippet <- snippet_support && !startsWith(trimws(following), "(")
    edit_range <- range(document$to_lsp_position(point$row, cursor$start),
        document$to_lsp_position(point$row, cursor$end))
    items <- lapply(labels, function(label) {
        shape <- if (identical(cursor$accessor, "@")) {
            member_slot(value, label, index, bindings, budget)
        } else {
            value$fields[[label]]
        }
        if (!is.null(shape)) shape <- member_r6_member_info(shape, value)
        key <- members[[label]]
        symbol <- member_symbol_info(label, shape, key, index)
        is_function <- !is.null(shape$function_expr) || !is.null(shape$result_shape) ||
            !is.null(shape$function_key) || (!is.null(key) && !is.na(key))
        inserted <- quote_completion_name(label)
        snippet <- is_function && call_snippet
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
            documentation = if (!is.null(symbol$description)) list(kind = "markdown", value = symbol$description),
            filterText = label, insertTextFormat = if (snippet) InsertTextFormat$Snippet else InsertTextFormat$PlainText,
            textEdit = text_edit(edit_range, text),
            data = list(
                type = "member", package = symbol$package, function_id = documentation_id,
                signature = signature, context_uri = uri, generation = index$generation
            )
        )
    })
    if (length(matches) > limit || budget$exhausted) attr(items, "truncated") <- TRUE
    items
}

# Ordinary document/workspace items may represent a class generator, instance,
# or method alias. Enrich the already-pruned list using one bounded lexical
# inference context, leaving unknown symbols to their existing providers.
member_class_completion_info <- function(uri, workspace, document, point, token, items, resolved = NULL) {
    if (!length(items) || !is.null(token$package) || !identical(token$accessor, "")) return(items)
    if (identical(resolved, FALSE)) return(items)
    start <- token$range$start$col
    cursor <- list(operator = start + 1L, operator_row = point$row,
        start = start, end = token$range$end$col, accessor = NULL,
        before = substr(document$line0(point$row), 1L, start))
    if (is.null(resolved)) resolved <- member_resolve_cursor(uri, workspace, document, point, cursor)
    if (is.null(resolved$value)) return(items)
    fields <- resolved$value$fields
    env <- c(fields, resolved$bindings[setdiff(names(resolved$bindings), names(fields))])
    field_names <- names(fields)
    local_names <- names(env)
    document_names <- names(resolved$index$document_bindings)
    for (i in seq_along(items)) {
        if (isTRUE(resolved$budget$exhausted)) break
        package <- items[[i]]$data$package
        if (!is.null(package) && !package %in% c("", WORKSPACE)) next
        if (identical(items[[i]]$data$type, "parameter") || !is.null(items[[i]]$data$parameter)) next
        label <- items[[i]]$label
        if (startsWith(label, "`")) label <- tryCatch(as.character(parse(text = label)[[1L]]), error = function(e) label)
        if (!label %in% local_names && !label %in% document_names) next
        value <- if (label %in% field_names) {
            member_completion_value(fields[[label]], resolved$index, resolved$budget)
        } else {
            member_infer(as.name(label), resolved$index, env, budget = resolved$budget)
        }
        description <- member_r6_description(value)
        if (!is.null(description)) {
            doc <- unique(c(description, items[[i]]$documentation$value))
            items[[i]]$documentation <- list(kind = "markdown", value = paste(doc, collapse = "\n\n"))
        }
    }
    items
}
