# Document indexes contain syntax and positions, never user values. Recovery
# parses a bounded current expression; earlier statements come from the index.
member_incomplete_packages <- function(content) {
    sentinel <- ".__languageserver_namespace_hint__"
    if (length(content) > 129L || sum(nchar(content, type = "bytes")) > 65536L ||
            any(grepl(sentinel, content, fixed = TRUE))) return(character())
    repaired <- c(content, sentinel)
    parsed <- tryCatch(str2expression(paste0(paste(repaired, collapse = "\n"),
                missing_closing_delimiters(repaired))), error = function(e) NULL)
    packages <- character()
    member_walk(parsed, function(node) {
        if (length(node) == 3L && (member_head(node, "$") || member_head(node, "@")) &&
                identical(member_name(node[[3L]]), sentinel)) {
            member_walk(node[[2L]], function(receiver) {
                if (length(receiver) == 3L && (member_head(receiver, "::") || member_head(receiver, ":::"))) {
                    package <- member_name(receiver[[2L]])
                    if (!is.null(package)) packages <<- union(packages, package)
                }
            })
        }
    })
    packages
}

# A partially parsed R cell still has the original receiver subtree for a
# dangling $/@. Use its physical extent to limit the existing package probe,
# even when the receiver sits inside a long cell or an unfinished call.
member_cell_packages <- function(content, data, terminals) {
    n <- length(terminals)
    if (n < 2L) return(character())
    # The lexer can record a closing delimiter or a comma after the accessor
    # that triggered the error (e.g. fn(pkg::object$, x)).
    last <- n
    if (!data$token[[terminals[[last]]]] %in% c("'$'", "'@'")) last <- last - 1L
    op <- terminals[[last]]
    if (last < 2L || data$parent[[op]] != 0L || !data$token[[op]] %in% c("'$'", "'@'")) return(character())
    prev <- terminals[[last - 1L]]
    receiver <- which(data$token == "expr" & data$parent == 0L &
            data$line2 == data$line2[[prev]] & data$col2 == data$col2[[prev]])
    if (length(receiver) != 1L) return(character())
    first <- data$line1[[receiver]]
    end <- data$line2[[op]]
    if (end - first > 128L || sum(nchar(content[seq.int(first, end)], type = "bytes")) > 65536L) return(character())
    text <- get_range_text(content, first, data$col1[[receiver]], end, data$col2[[op]])
    member_incomplete_packages(text)
}

member_attachment_arguments <- c("package", "quietly", "verbose", "warn.conflicts", "logical.return", "character.only")

# Normalize explicitly qualified attachment calls for the same import history
# as a bare library/require. Keep the original item for expression recovery.
member_attachment_item <- function(item) {
    expr <- item$expr
    if (!is.call(expr)) return(NULL)
    head <- expr[[1L]]
    if (identical(head, quote(base::library)) || identical(head, quote(base::require))) {
        head <- head[[3L]]
        item$qualified <- TRUE
    } else if (!identical(head, as.name("library")) && !identical(head, as.name("require"))) {
        return(NULL)
    }
    fn <- if (identical(head, as.name("library"))) base::library else base::require
    args <- tryCatch(match.call(fn, expr), error = function(e) NULL)
    if (!is.null(args) && is.null(args$package)) return(NULL)
    package <- member_name(args$package)
    if (is.null(package) || is.na(package) || !nzchar(package) || (is.symbol(args$package) &&
            !is.null(args$character.only) && !identical(args$character.only, FALSE))) package <- NULL
    if (is.null(package) || !all(names(as.list(args)[-1L]) %in% member_attachment_arguments)) {
        item$uncertain <- TRUE
    }
    item$expr <- as.call(list(head, package))
    item
}

# Ordered bindings can be built for a document or a workspace source sequence.
member_index_bindings <- function(items) {
    bindings <- new.env(hash = TRUE, parent = emptyenv())
    aliases <- new.env(hash = TRUE, parent = emptyenv())
    record <- function(name, rhs, item, write = FALSE, field = NULL, subset = FALSE, setter = NULL, slot = FALSE, conditional = FALSE) {
        history <- bindings[[name]]
        last <- length(history)
        entry <- list(expr = rhs, start = item$start, end = item$end, cache_prefix = item$cache_prefix)
        if (!is.null(item$declaration)) entry$declaration <- item$declaration
        if (write) {
            entry$writer <- name
            if (conditional || (last && isTRUE(history[[last]]$uncertain_owner))) entry$uncertain_owner <- TRUE
            entry$field <- field
            if (!is.null(setter)) entry$setter <- setter
            if (subset) entry$subset <- TRUE
            if (slot) entry$slot <- TRUE
            entry$first_write <- if (last && !is.null(history[[last]]$first_write)) {
                history[[last]]$first_write
            } else {
                last + 1L
            }
            # Store a shared write only once, even when the receiver has
            # many aliases. Each name retains the point where it joined.
            group <- aliases[[name]]
            if (!is.null(group)) {
                group$items[[length(group$items) + 1L]] <- entry
                entry$aliases <- history[[last]]$aliases
            }
        } else {
            source <- if (is.symbol(rhs)) as.character(rhs)
            group <- if (length(source) && nzchar(source)) aliases[[source]]
            if (is.null(group) && length(source) && nzchar(source) && !identical(name, source)) {
                previous <- bindings[[source]]
                anchor <- length(previous)
                if (anchor) {
                    group <- new.env(parent = emptyenv())
                    previous[[anchor]]$aliases <- list(group = group, anchor = anchor, skip = 0L)
                    attr(previous, "member_aliases") <- TRUE
                    bindings[[source]] <- previous
                    aliases[[source]] <- group
                }
            }
            if (!is.null(group)) {
                entry$aliases <- list(group = group, anchor = last + 1L, skip = length(group$items))
                aliases[[name]] <- group
            } else if (!is.null(aliases[[name]])) {
                rm(list = name, envir = aliases)
            }
        }
        history[[last + 1L]] <- entry
        if (!is.null(entry$aliases)) attr(history, "member_aliases") <- TRUE
        bindings[[name]] <- history
    }
    for (item in items) {
        expr <- item$expr
        control <- member_r6_control(expr)
        if (!is.null(control)) record(control$receiver, control$call, item, write = TRUE, setter = "r6_control")
        head <- if (is.call(expr)) expr[[1L]]
        named_bind <- identical(head, quote(`:=`))
        if (identical(head, quote(`<-`)) || identical(head, quote(`=`)) || named_bind) {
            lhs <- expr[[2L]]
            rhs <- if (named_bind) expr else expr[[3L]]
            name <- member_name(lhs)
            if (!is.null(name) && nzchar(name)) {
                record(name, rhs, item)
            } else if (is.call(lhs)) {
                name <- member_write_root(lhs)
                if (!is.null(name) && nzchar(name)) {
                    field <- if (!named_bind) member_write_field(lhs)
                    setter <- if (is.null(field)) member_s7_method_setter(lhs)
                    if (is.null(field) && is.null(setter) && !named_bind) setter <- member_s7_property_setter(lhs)
                    record(name, if (is.null(field) && is.null(setter)) quote(.__unknown_mutation__) else rhs, item,
                        write = TRUE, field = field, subset = member_head(lhs, "[["),
                        setter = setter, slot = member_head(lhs, "@"))
                }
            }
        } else if (is.call(expr)) {
            for (name in member_assigned_names(expr)) {
                record(name, quote(.__unknown_branch_write__), item, write = TRUE, conditional = TRUE)
                if (!is.null(aliases[[name]])) rm(list = name, envir = aliases)
            }
            if (is.null(control)) {
                for (name in member_r6_conditional_controls(expr)) {
                    record(name, NULL, item, write = TRUE, setter = "r6_control")
                }
            }
        }
    }
    as.list(bindings)
}

# Start bounded member recovery at the first uncertain top-level statement.
# Earlier expressions came from the original text. Retain the recovery
# window's line/byte limits and never cut a semicolon-separated source line.
member_recovery_start <- function(content, data, incomplete_lines, verified_blocks = FALSE) {
    if (!length(incomplete_lines) || !length(data$id)) return(1L)
    roots <- which(data$parent == 0L & data$token %in% c("expr", "expr_or_assign_or_help"))
    begin <- data$line1[roots]
    end <- data$line2[roots]
    stop_line <- incomplete_lines[[1L]] + 1L
    if (stop_line <= 1L) return(1L)
    prefix <- which(begin < stop_line)
    bytes <- c(0, cumsum(as.double(nchar(content, type = "bytes"))))
    oversized <- end[prefix] - begin[prefix] > 128L | bytes[end[prefix] + 1L] - bytes[begin[prefix]] > 65536
    if (verified_blocks && any(oversized)) {
        # Large standalone blocks already have an original AST for their
        # statements. The line and byte limits apply to reparsing uncertain input.
        oversized <- oversized & !data$id[roots[prefix]] %in% data$parent[data$token == "'{'"]
    }
    unsupported <- which(end[prefix] >= stop_line | oversized)
    if (length(unsupported)) stop_line <- begin[[prefix[[unsupported[[1L]]]]]]
    # An earlier expression can end on the first line of the next expression.
    i <- findInterval(stop_line - 1L, begin)
    while (i > 0L && end[[i]] >= stop_line) {
        stop_line <- begin[[i]]
        i <- findInterval(stop_line - 1L, begin)
    }
    stop_line
}

# Bounded line recovery can miss the beginning of a valid function inside a
# block, or recover its body as separate expressions. Keep the verified AST
# for every such range and retain cursor recovery in the remaining gaps.
member_merge_verified_items <- function(items, verified) {
    refs <- attr(verified, "srcref")
    confirmed <- lapply(seq_along(verified), function(i) {
        ref <- refs[[i]]
        list(expr = verified[[i]], start = c(ref[[7L]] - 1L, ref[[5L]] - 1L),
            end = c(ref[[8L]] - 1L, ref[[6L]]))
    })
    if (!length(items)) return(confirmed)
    bounds <- function(items) vapply(items, function(item) c(item$start, item$end), integer(4L))
    recovered <- bounds(items)
    original <- bounds(confirmed)
    stride <- as.double(max(recovered[c(2L, 4L), ], original[c(2L, 4L), ])) + 1
    start <- function(x) x[1L, ] * stride + x[2L, ]
    end <- function(x) x[3L, ] * stride + x[4L, ]
    # The first confirmed item ending after an old start is its only possible
    # first overlap. Confirmed statement ranges are disjoint and ordered.
    next_item <- findInterval(start(recovered), end(original)) + 1L
    keep <- next_item > length(confirmed) | start(original)[next_item] >= end(recovered)
    c(items[keep], confirmed)[order(c(start(recovered)[keep], start(original)))]
}

member_document_index <- function(content, parsed = NULL, syntax = NULL, incomplete_lines = NULL,
    defer_small_scopes = FALSE, value_source = NULL, verified = NULL,
    type_hints = member_roxygen_type_index(content, if (is.null(syntax)) utils::getParseData(parsed) else syntax)) {
    parsed <- member_attach_type_hints(parsed, type_hints)
    verified <- member_attach_type_hints(verified, type_hints)
    complete <- !is.null(parsed) && is.null(incomplete_lines)
    recovery_start <- 1L
    if (!complete && !is.null(parsed)) {
        recovery_start <- member_recovery_start(content, syntax, incomplete_lines, length(verified) > 0L)
        if (recovery_start > 1L && length(verified)) {
            refs <- attr(verified, "srcref")
            later <- vapply(refs, `[[`, integer(1L), 8L) >= recovery_start
            verified <- verified[later]
            attr(verified, "srcref") <- refs[later]
        }
    }
    recovered_packages <- character()
    items <- list()
    has_blocks <- FALSE
    append <- function(parsed, offset = 0L) {
        statements <- top_level_statements(parsed)
        if (!identical(statements, parsed)) has_blocks <<- TRUE
        refs <- attr(statements, "srcref")
        for (i in seq_along(statements)) {
            ref <- refs[[i]]
            # first_parsed/last_parsed are physical, including across #line.
            items[[length(items) + 1L]] <<- list(expr = statements[[i]],
                start = c(offset + ref[[7L]] - 1L, ref[[5L]] - 1L),
                end = c(offset + ref[[8L]] - 1L, ref[[6L]]))
        }
    }
    if (complete) {
        append(parsed)
    } else if (recovery_start > 1L) {
        refs <- attr(parsed, "srcref")
        prefix <- vapply(refs, `[[`, integer(1L), 7L) < recovery_start
        append(structure(parsed[prefix], srcref = refs[prefix]))
    }
    prefix_bytes <- 0
    if (!complete) {
        if (length(items) || length(verified)) {
            # Incomplete buffers use cursor recovery, not function-part indexes.
            # The reused source references need their lines but not a duplicate
            # full parse table in every worker snapshot.
            prefix_bytes <- member_compact_source(parsed, NULL)
        }
        # Recover complete statements after unrelated syntax errors. Bounds
        # apply to incomplete runs; valid large documents use the parser above.
        start <- recovery_start
        end <- recovery_start
        batch_end <- 0L
        append_parsed <- function(parsed, start) {
            if (!is.null(value_source)) {
                text <- attr(parsed, "srcfile")$lines
                rows <- seq.int(start, length.out = length(text))
                parsed <- restore_tabbed_parse(parsed, utils::getParseData(parsed), text,
                    value_source[rows])$expr
            }
            append(member_attach_type_hints(parsed, type_hints, start - 1L), start - 1L)
        }
        while (end <= length(content)) {
            if (end == start && end > batch_end) {
                batch_end <- min(length(content), start + 128L)
                text <- content[seq.int(start, batch_end)]
                if (sum(nchar(text, type = "bytes")) <= 65536L) {
                    parsed <- tryCatch(parse(text = text, keep.source = TRUE), error = function(e) NULL)
                    if (!is.null(parsed)) {
                        append_parsed(parsed, start)
                        start <- end <- batch_end + 1L
                        next
                    }
                }
                # A failing batch uses the original bounded recovery below.
                # Do not retry that same broken section for each valid line.
            }
            if (end - start > 128L) start <- end
            text <- content[seq.int(start, end)]
            if (sum(nchar(text, type = "bytes")) > 65536L) {
                start <- end
                end <- end + 1L
                next
            }
            message <- NULL
            parsed <- tryCatch(parse(text = text, keep.source = TRUE), error = function(e) {
                message <<- conditionMessage(e)
                NULL
            })
            if (is.null(parsed)) {
                if (grepl("unexpected end of input", message)) {
                    recovered_packages <- union(recovered_packages, member_incomplete_packages(text))
                }
                if (!grepl("unexpected end of input|INCOMPLETE_STRING", message)) {
                    # An unfinished member call can absorb the next assignment
                    # as its first argument. Once the following statement makes
                    # that parse impossible, retry after the dangling opener.
                    if (end > start && endsWith(trimws(text[[1L]]), "(")) {
                        call <- tryCatch(str2lang(paste0(text[[1L]], ")")), error = function(e) NULL)
                        if (is.call(call) && length(call) == 1L &&
                                (member_head(call[[1L]], "$") || !is.null(member_write_field(call[[1L]])))) {
                            start <- end <- start + 1L
                            next
                        }
                    }
                    start <- end + 1L
                }
                end <- end + 1L
                next
            }
            append_parsed(parsed, start)
            start <- end + 1L
            end <- end + 1L
        }
        if (length(verified)) items <- member_merge_verified_items(items, verified)
    }
    s4 <- list()
    imports <- list()
    sources <- list()
    effects <- list()
    type_names <- unique(unlist(type_hints, use.names = FALSE))
    qualified <- type_names[grepl("::", type_names, fixed = TRUE)]
    hint_packages <- sub("::.*", "", qualified)
    packages <- recovered_packages
    class_scope <- FALSE
    accessor_registrations <- FALSE
    refclasses <- initializers <- list()
    symbol_scope <- length(type_hints) > 0L
    parts <- if (complete) member_function_parts(if (is.null(syntax)) utils::getParseData(parsed) else syntax) else NULL
    # Small scopes in recovered literate documents can be built when visited.
    # Bound foreground work by both source bytes and AST size, including dense
    # one-line functions. The complete function-part table is already shared.
    defer_small_scopes <- defer_small_scopes && complete && !is.null(syntax)
    if (defer_small_scopes) {
        line_bytes <- c(0, cumsum(as.double(nchar(content, type = "bytes"))))
        line_tokens <- c(0, cumsum(as.double(tabulate(syntax$line1[syntax$terminal], length(content)))))
    }
    prefix <- ""
    previous <- c(0L, 0L)
    fingerprint_source <- if (is.null(value_source)) content else value_source
    for (i in seq_along(items)) {
        item <- items[[i]]
        prefix <- member_source_prefix(fingerprint_source, previous, item$end, prefix)
        previous <- item$end
        item$cache_prefix <- prefix
        items[[i]] <- item
        expr <- item$expr
        syntax_names <- member_syntax_names(expr)
        check_accessors <- !accessor_registrations && ("registerS3method" %in% syntax_names ||
                any(c("method", "method<-") %in% syntax_names) && any(c("$", "$<-", "[[", "[[<-") %in% syntax_names))
        if (check_accessors) {
            # A common method formal or a $ read in a method body is not a
            # registration. Inspect just the matching calls, including nested ones.
            for (call in .Call("member_accessor_registrations_c", expr, PACKAGE = "languageserver")) {
                if (member_s7_accessor_registration(call)) {
                    accessor_registrations <- TRUE
                    break
                }
            }
        }
        if (!is.null(parts) && any(syntax_names %in% c("function", "{"))) {
            first <- item$start[[1L]] + 1L
            after <- item$end[[1L]] + 2L
            if (defer_small_scopes && first >= 1L && after <= length(line_bytes) &&
                    line_bytes[[after]] - line_bytes[[first]] <= 2048 &&
                    line_tokens[[after]] - line_tokens[[first]] <= 128) {
                item$deferred_source <- TRUE
            } else {
                item$source <- member_source_index(expr, parts)
            }
            items[[i]] <- item
        }
        class_scope <- class_scope || "R6Class" %in% syntax_names
        symbol_scope <- symbol_scope || any(syntax_names %in% c("R6Class", "setClass", "setRefClass", member_s7_intrinsics))
        # The syntax names already cover nested calls. Walk the expression
        # only when it could contain a qualified name or a member assignment.
        if (any(c("::", ":::") %in% syntax_names) ||
                (!symbol_scope && any(c("$", "@", "[[") %in% syntax_names) &&
                        any(c("<-", "=") %in% syntax_names))) {
            member_walk(expr, function(node) {
                if (!is.call(node)) return()
                if (!symbol_scope && (member_head(node, "<-") || member_head(node, "=")) &&
                        is.symbol(node[[2L]]) && is.call(node[[3L]]) &&
                        (member_head(node[[3L]], "$") || member_head(node[[3L]], "@") || member_head(node[[3L]], "[["))) {
                    symbol_scope <<- TRUE
                }
                if (member_head(node, "::") || member_head(node, ":::")) {
                    package <- member_name(node[[2L]])
                    if (!is.null(package)) packages <<- union(packages, package)
                }
            })
        }
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
            } else if (identical(name, "setRefClass") && is.character(class) && length(class) == 1L && nzchar(class)) {
                refclasses[[class]] <- c(refclasses[[class]], list(list(expr = declaration, start = item$start, end = item$end)))
            }
        }
        if (is.call(expr) && (member_head(expr, "setMethod") || identical(expr[[1L]], quote(methods::setMethod)))) {
            args <- tryCatch(match.call(methods::setMethod, expr), error = function(e) NULL)
            if (identical(args$f, "initialize")) initializers[[length(initializers) + 1L]] <- item
        }
        import <- member_attachment_item(item)
        if (!is.null(import)) imports[[length(imports) + 1L]] <- import
        if (is.call(expr) && length(expr) == 2L && is.character(expr[[2L]]) &&
                (member_head(expr, "source") || identical(expr[[1L]], quote(base::source))) &&
                (is.null(names(expr)) || names(expr)[[2L]] %in% c("", "file"))) {
            spec <- index_static_path(expr[[2L]])
            if (!is.null(spec)) sources[[length(sources) + 1L]] <- list(
                item = i, spec = spec, bare = member_head(expr, "source"))
        }
        if (is.call(expr) && !member_head(expr, "<-") && !member_head(expr, "=") && !member_head(expr, ":=")) {
            effects[[length(effects) + 1L]] <- item
        }
    }
    source_bytes <- if (complete) member_compact_source(parsed, parts) else prefix_bytes
    list(items = items, has_blocks = has_blocks, bindings = member_index_bindings(items), imports = imports, sources = sources, effects = effects, s4 = s4, refclasses = refclasses, initializers = initializers,
        has_type_hints = length(type_hints) > 0L, type_hints = type_hints,
        content_key = digest::digest(fingerprint_source, algo = "xxhash64"),
        source_bytes = source_bytes, deferred_parts = if (defer_small_scopes) parts,
        packages = union(union(recovered_packages, hint_packages), packages),
        hint_only_packages = setdiff(hint_packages, packages),
        class_scope = class_scope, symbol_scope = symbol_scope, accessor_registrations = accessor_registrations,
        s3 = s3_source_index(items))
}

# Complete valid ordinary scopes from their source index. Class analysis is
# needed only when source syntax or package metadata could supply R6 values;
# incomplete scopes still use recovery to discover newly typed local bindings.
member_scope_required <- function(workspace, document, point = NULL) {
    data <- document$parse_data
    if ((!identical(document$version, data$version) && !member_ordinary_request(workspace, document)) ||
            isTRUE(data$parse_error)) return(TRUE)
    if (!isFALSE(data$member_data$class_scope)) return(TRUE)
    if (!is.null(point)) return(member_package_scope_required(workspace, data$member_data, "class_scope"))
    metadata <- workspace$member_metadata
    if (is.null(metadata)) return(FALSE)
    for (package in metadata$keys()) {
        # Older snapshots without the hint retain the inference path.
        candidate <- if (is.function(metadata$catalog)) metadata$catalog(package) else metadata$get(package)
        if (!isFALSE(candidate$class_scope)) return(TRUE)
    }
    FALSE
}

member_before <- function(a, b) {
    if (a[[1L]] != b[[1L]]) return(a[[1L]] < b[[1L]])
    if (a[[2L]] != b[[2L]]) return(a[[2L]] < b[[2L]])
    # The third coordinate orders statements inserted at a source() call.
    # A normal buffer position at its end sees every statement in the file.
    length(b) == 2L || (length(a) > 2L && a[[3L]] <= b[[3L]])
}

# Locate a single quoted key in x[["key"]]. The cursor can be inside the
# literal or at either quote. Recovery verifies the receiver in the R syntax.
member_bracket_cursor <- function(document, point, bounds = document_r_bounds(document, point$row),
    line = document$line0(point$row)) {
    if (is.null(bounds) || (!grepl("[[", line, fixed = TRUE) && !grepl("^[ \t]*[rR]?['\"]", line))) return(NULL)
    # Use the same cached, document-aware lexer as signature help. A string
    # containing what looks like an index has no opening bracket at that site.
    open <- document_open_bracket(document, point, bounds)
    if (open[[2L]] != "[") return(NULL)
    operator_row <- open[[1L]][[1L]]
    operator <- open[[1L]][[2L]] # zero-based second bracket, one-based first bracket
    if (operator < 1L || substr(document$line0(operator_row), operator, operator + 1L) != "[[") return(NULL)
    offset <- operator + 1L
    if (operator_row != point$row) {
        if (point$row - operator_row > 128L) return(NULL)
        lines <- document_uncomment_lines(document, operator_row, point$row - 1L, bounds)
        lines[[1L]] <- substring(lines[[1L]], offset + 1L)
        if (any(nzchar(trimws(lines)))) return(NULL)
        offset <- 0L
    }
    fragment <- substring(line, offset + 1L)
    col <- point$col - offset
    if (col < 0L) return(NULL)
    if (substr(fragment, col + 1L, col + 1L) %in% c("'", "\"") &&
            !nzchar(trimws(substr(fragment, 1L, col)))) col <- col + 1L
    key <- .Call("scan_string_completion_c", fragment, as.integer(col), PACKAGE = "languageserver")
    if (is.null(key)) return(NULL)
    prefix <- sub("^[ \t]*", "", substr(fragment, 1L, key$start))
    raw <- prefix %in% c("r", "R")
    if (nzchar(prefix) && !raw) return(NULL)
    quote <- substr(fragment, key$start + 1L, key$start + 1L)
    start <- offset + key$start - as.integer(raw)
    end <- offset + key$end
    # A string in a computed or multidimensional subscript is not a field.
    rest <- if (key$closed) trimws(substring(line, end + 1L), which = "left") else ""
    if (key$closed && (!nzchar(rest) || startsWith(rest, "#")) && point$row < bounds[[2L]]) {
        last <- min(bounds[[2L]], point$row + 128L)
        following <- trimws(document_uncomment_lines(document, point$row + 1L, last, bounds))
        tokens <- which(nzchar(following))
        if (length(tokens)) rest <- following[[tokens[[1L]]]]
    }
    if (nzchar(rest) && !startsWith(rest, "#") && !startsWith(rest, "]")) return(NULL)
    token <- if (raw) {
        # A raw literal can be replaced with an equivalent quoted key. Its
        # delimiter is not part of the prefix used to match member names.
        if (!key$closed) return(NULL)
        value <- tryCatch(str2lang(substr(line, start + 1L, end)), error = function(e) NULL)
        if (!is.character(value)) return(NULL)
        bare <- sub("^-*[([{]", "", key$token)
        substr(value, 1L, nchar(bare))
    } else if (grepl("\\", key$token, fixed = TRUE)) {
        tryCatch(str2lang(paste0(quote, key$token, quote)), error = function(e) key$token)
    } else {
        key$token
    }
    list(operator = operator, operator_row = operator_row, start = start, end = end,
        token = token, accessor = "[[", before = substr(line, 1L, start), quote = quote)
}

member_key_formals <- c(slot = "name", prop = "name", prop_exists = "name", props = "names")

member_cursor <- function(document, point) {
    bounds <- document_r_bounds(document, point$row)
    if (is.null(bounds)) {
        return(NULL)
    }
    line <- document$line0(point$row)
    bracket <- member_bracket_cursor(document, point, bounds, line)
    if (!is.null(bracket)) return(bracket)
    if (grepl("['\"]", line)) {
        col <- point$col
        if (substr(line, col + 1L, col + 1L) %in% c("'", "\"")) col <- col + 1L
        key <- .Call("scan_string_completion_c", line, as.integer(col), PACKAGE = "languageserver")
        if (!is.null(key)) {
            before <- list(row = point$row, col = key$start)
            cursor <- member_access_cursor(document, before, bounds, line)
            if (!is.null(cursor) && cursor$start == key$start) {
                quote <- substr(line, key$start + 1L, key$start + 1L)
                cursor$quote <- quote
                cursor$end <- key$end
                cursor$token <- tryCatch(str2lang(paste0(quote, key$token, quote)),
                    error = function(e) key$token)
                return(cursor)
            }
            call <- document$detect_call(before, signature = TRUE)
            if (isTRUE(call$token %in% names(member_key_formals))) {
                expected <- if (call$token == "slot") "methods" else "S7"
                if (is.null(call$package) || identical(call$package, expected)) {
                    helper <- call$token
                    parsed <- document$parse_data
                    if (helper != "slot" && !is.null(parsed$syntax_data) && identical(parsed$version, document$version)) {
                        s <- parsed$syntax_data
                        literal <- member_navigation_token(parsed, list(row = point$row, col = key$start + 1L))
                        if (literal && s$token[[literal]] == "STR_CONST") {
                            owner <- s$parent_row[[s$parent_row[[literal]]]]
                            if (owner && member_replacement_rhs(s, owner)) {
                                if (helper != "prop") return(NULL)
                                helper <- "prop<-"
                            }
                        }
                    }
                    quote <- substr(line, key$start + 1L, key$start + 1L)
                    token <- tryCatch(str2lang(paste0(quote, key$token, quote)), error = function(e) key$token)
                    return(list(
                        operator = key$start + 1L, operator_row = point$row, start = key$start, end = key$end,
                        accessor = "@", key_helper = helper, token = token, quote = quote,
                        before = substr(line, 1L, key$start)
                    ))
                }
            }
        }
    }
    member_access_cursor(document, point, bounds, line)
}

member_access_cursor <- function(document, point, bounds, line) {
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
        start_row <- max(bounds[[1L]], point$row - 128L)
        if (start_row < point$row) {
            uncommented <- document_uncomment_lines(document, start_row, point$row - 1L, bounds)
            for (row in seq.int(point$row - 1L, start_row)) {
                previous <- uncommented[[row - start_row + 1L]]
                if (grepl("^[ \\t]*$", previous)) next
                operator <- regexpr("[$@][ \\t]*$", previous, perl = TRUE)[[1L]]
                if (operator > 0L) {
                    operator_row <- row
                    accessor <- substr(previous, operator, operator)
                }
                break
            }
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

member_recover <- function(document, point, cursor, data, origin_uri = NULL) {
    content <- if (document$is_rmarkdown) literate_r_content(document$content, document$regions) else document$content
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
    if (document$is_rmarkdown) {
        cell <- literate_r_cell_at(document$regions, point$row)
        if (is.null(cell)) return(NULL)
        start <- max(start, cell$body_start - 1L)
    }
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
    skipped_columns <- nchar(content[[start + 1L]]) - nchar(lines[[1L]])
    if (sum(nchar(lines, type = "bytes")) > 65536L || any(grepl(sentinel, lines, fixed = TRUE))) {
        return(NULL)
    }
    for (skip in seq_len(min(length(lines), 129L))) {
        local <- lines[seq.int(skip, length(lines))]
        local[[length(local)]] <- paste0(local[[length(local)]], sentinel)
        closers <- missing_closing_delimiters(local)
        parsed <- tryCatch(member_parse_recovered(paste0(paste(local, collapse = "\n"), closers),
                origin_uri, start + skip - 1L, if (skip == 1L) skipped_columns else 0L,
                type_hints = data$type_hints, hint_comments = data$hint_comments), error = function(e) NULL)
        if (is.null(parsed) && !is.null(cursor$accessor) && any(grepl("|>", local, fixed = TRUE))) {
            # Native pipes require a call on the RHS. The cursor sentinel is
            # a callee here; its synthetic arguments are never evaluated.
            local[[length(local)]] <- paste0(local[[length(local)]], "()")
            closers <- missing_closing_delimiters(local)
            parsed <- tryCatch(member_parse_recovered(paste0(paste(local, collapse = "\n"), closers),
                    origin_uri, start + skip - 1L, if (skip == 1L) skipped_columns else 0L,
                    type_hints = data$type_hints, hint_comments = data$hint_comments), error = function(e) NULL)
        }
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
            context <- member_recover_context(document, content, point, cursor, start + skip - 1L, column, sentinel, parsed, origin_uri)
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
        context <- member_recover_context(document, content, point, cursor, start, column, sentinel, NULL, origin_uri)
        if (!is.null(context)) return(list(
            parsed = context, sentinel = sentinel, context = context, start = c(start, column)
        ))
    }
    NULL
}

member_recover_context <- function(document, content, point, cursor, start, column, sentinel, parsed, origin_uri = NULL) {
    # Only R6 declarations need later members for method-body context. Stop at
    # the first complete declaration, before unrelated trailing syntax errors.
    declaration <- FALSE
    member_walk(parsed, function(node) {
        if (is.call(node) && (identical(member_name(node[[1L]]), "R6Class") ||
                    (member_head(node[[1L]], "::") && identical(member_name(node[[1L]][[3L]]), "R6Class")))) declaration <<- TRUE
    })
    if (!is.null(parsed) && !declaration && is.null(cursor$key_helper) && !isTRUE(cursor$s4_new)) return(NULL)
    last <- min(length(content) - 1L, start + 128L)
    if (document$is_rmarkdown) {
        cell <- literate_r_cell_at(document$regions, point$row)
        last <- min(last, cell$body_end - 1L)
    }
    full <- content[seq.int(start + 1L, last + 1L)]
    offset <- point$row - start + 1L
    full[[offset]] <- paste0(cursor$before, sentinel, substring(document$line0(point$row), cursor$end + 1L))
    skipped_columns <- 0L
    if (column > 0L) {
        original <- full[[1L]]
        full[[1L]] <- sub("^[ \\t]*;[ \\t]*", "", substring(original, column + 1L))
        skipped_columns <- nchar(original) - nchar(full[[1L]])
    }
    edit <- list(row = point$row, start = cursor$start, end = cursor$end, inserted = nchar(sentinel))
    for (end in seq.int(offset, length(full))) {
        lines <- full[seq_len(end)]
        if (sum(nchar(lines, type = "bytes")) > 65536L) break
        context <- tryCatch(member_parse_recovered(lines, origin_uri, start, skipped_columns, edit,
                document$parse_data$member_data$type_hints, document$parse_data$member_data$hint_comments),
            error = function(e) NULL)
        if (!is.null(context)) return(context)
    }
    NULL
}

member_context_index <- function(workspace, uri, document, at, parsed = NULL,
    referenced = member_syntax_names(parsed)) {
    metadata <- if (!is.null(workspace$member_metadata)) workspace$member_metadata else NULL
    view <- member_workspace_view(workspace, document)
    revision <- member_document_cache_revision(document, metadata, view$key, workspace)
    bindings <- if (is.null(view$bindings)) document$parse_data$member_data$bindings else view$bindings
    # Choose the extractor associated with roots actually referenced here. The
    # core does not recognize package names, class names, or wrapper conventions.
    # Follow a bounded set of referenced assignments (q may have a package
    # receiver several aliases back). Do this before choosing an extractor.
    seen <- character()
    if (!is.null(metadata)) for (pass in seq_len(16L)) {
        pending <- setdiff(unique(referenced), seen)
        if (!length(pending) || length(seen) > 256L) break
        seen <- c(seen, pending)
        for (name in pending) {
            history <- member_history_at(bindings[[name]], at)
            # A dangling $ can make the following assignment parse as a
            # member write. Later bindings must not hide the receiver's package.
            position <- member_history_position(history, at)
            if (position) {
                first <- history[[position]]$first_write
                relevant <- if (is.null(first)) position else unique(c(max(1L, first - 1L),
                        seq.int(first, min(position, first + 255L)), position))
                for (i in relevant) referenced <- union(referenced, member_syntax_names(history[[i]]$expr))
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
    index$document_bindings <- bindings
    index$accessor_registrations <- isTRUE(document$parse_data$member_data$accessor_registrations) ||
        isTRUE(view$accessor_registrations)
    index$document_cache <- new.env(parent = emptyenv())
    index$dispatch_document <- document
    index$dispatch_workspace <- workspace
    index$document_binding_cache <- if (!is.null(revision)) document$member_binding_cache else NULL
    index$methods_attached <- TRUE
    index$s7_attached <- FALSE
    index$document_package <- document$parse_data$member_data$document_package
    if (member_package_context_required(document, workspace)) {
        index$source_workspace <- workspace
        index$source_uri <- uri
    }
    index$attached_roots <- list()
    index$attached_s4 <- list()
    index$document_initializers <- document$parse_data$member_data$initializers
    index$document_refclasses <- document$parse_data$member_data$refclasses
    index$document_s4 <- document$parse_data$member_data$s4
    index$hint_packages <- c(workspace$startup_packages, document$parse_data$member_data$hint_import_packages)
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
    # The startup vector is in attachment order (the search path reversed),
    # so later packages overwrite earlier ones just as R masks their names.
    # A cached namespace alone must not make its bare data names visible.
    for (package in workspace$startup_packages) {
        package_index <- member_lookup(index$namespace_indices, package)
        if (is.null(package_index)) next
        index$attached_roots[names(package_index$roots)] <- package_index$roots
        if (!is.null(package_index$s4_classes)) {
            index$attached_s4[names(package_index$s4_classes)] <- package_index$s4_classes
        }
    }
    imports <- character()
    # Bindings are resolved lazily at their source position. Retain the
    # previous S7 names only when an attachment changes that namespace's API.
    s7_names <- member_lookup(index$namespace_indices, "S7")$exports
    attachments <- list()
    for (item in if (is.null(view$bindings)) data$imports else view$imports) {
        if (!member_before(item$end, at)) next
        if (!isTRUE(item$qualified) &&
                member_history_position(bindings[[member_name(item$expr[[1L]])]], item$start)) next
        imports <- c(imports, if (is.null(item$cache_prefix)) paste(item$start, item$end, collapse = ":") else item$cache_prefix)
        expr <- item$expr
        if (length(expr) < 2L) next
        if (member_head(expr, "library") || member_head(expr, "require")) {
            package <- member_name(expr[[2L]])
            package_index <- member_lookup(index$namespace_indices, package)
            excluded <- if (isTRUE(index$s7_attached) && !isTRUE(item$uncertain)) {
                member_lookup(document$attachment_exclusions, package)
            }
            if (length(s7_names)) {
                # Selective or dynamic attachments cannot establish ownership.
                # Invalidate any overlapping S7 names at this source position.
                changed <- if (is.null(package_index) && isTRUE(item$uncertain)) s7_names else
                    intersect(setdiff(s7_names, excluded), package_index$exports)
                if (length(changed)) {
                    attachments[[length(attachments) + 1L]] <- list(end = item$end,
                        previous = index$attached_roots[intersect(s7_names, names(index$attached_roots))])
                    if (isTRUE(item$uncertain)) {
                        index$attached_roots[changed] <- rep(list(member_value(reason = "uncertain_attachment")), length(changed))
                    }
                }
            }
            if (isTRUE(item$uncertain)) next
            if (!is.null(package_index)) {
                index$hint_packages <- c(index$hint_packages, package)
                roots <- package_index$roots
                if (length(excluded)) roots <- roots[setdiff(names(roots), excluded)]
                index$attached_roots[names(roots)] <- roots
                if (!is.null(package_index$s4_classes)) {
                    index$attached_s4[names(package_index$s4_classes)] <- package_index$s4_classes
                }
                functions <- if (isTRUE(package_index$.metadata_lazy)) {
                    package_index$functions
                } else {
                    Filter(function(name) member_head(package_index$definitions[[name]], "function"),
                        intersect(package_index$exports, names(package_index$definitions)))
                }
                for (name in setdiff(functions, excluded)) {
                    if (is.null(index$attached_roots[[name]]$s7_dispatch_args) &&
                            is.null(index$attached_roots[[name]]$s4_generator) &&
                            is.null(index$attached_roots[[name]]$s7_generator)) {
                        index$attached_roots[name] <- list(member_value(function_key = name, metadata = package))
                    }
                }
            }
            if (identical(package, "R6")) index$r6_attached <- TRUE
            if (identical(package, "S7")) {
                if (!isTRUE(index$s7_attached)) index$s7_attached_at <- item$end
                index$s7_attached <- TRUE
            }
        }
    }
    fixed <- character()
    for (import in data$namespace_bindings) {
        pkg <- index$namespace_indices[[import$package]]
        # NAMESPACE imports bind exports only. Lazy data lives on the search
        # path, but is not part of import() or importFrom().
        exports <- if (is.null(pkg$namespace_exports)) pkg$exports else pkg$namespace_exports
        keep <- which(import$targets %in% exports)
        if (length(attachments)) fixed <- union(fixed, import$names[keep])
        for (i in keep) {
            target <- import$targets[[i]]
            value <- pkg$roots[[target]]
            if (is.null(value)) value <- member_value(function_key = target, metadata = import$package)
            index$attached_roots[import$names[[i]]] <- list(value)
        }
    }
    if (length(attachments)) {
        index$s7_attachments <- attachments
        index$s7_attachment_names <- setdiff(s7_names, fixed)
    }
    index$document_context_key <- digest::digest(list(index$package, workspace$startup_packages, data$namespace_bindings, document$attachment_exclusions, imports), algo = "xxhash64")
    index
}

member_resolve_document <- function(name, index, bindings, budget, depth, trail) {
    limit <- if (is.null(budget$depth_limit)) 64L else budget$depth_limit
    if (depth > limit) return(member_value(reason = "binding_recursion"))
    history <- index$document_bindings[[name]]
    at <- bindings$.__member_position__
    if (is.null(at)) at <- c(Inf, Inf)
    history <- member_history_at(history, at)
    position <- member_history_position(history, at)
    if (!position) {
        peer <- member_package_declaration(name, index, budget, depth, trail)
        if (!is.null(peer)) return(peer)
        attached <- member_attached_root(index, name, at)
        return(if (is.null(attached)) member_value(reason = "not_yet_bound") else attached)
    }
    item <- history[[position]]
    id <- paste(c("binding", name, item$end), collapse = ":")
    if (id %in% trail) {
        return(member_value(reason = "binding_cycle"))
    }
    # A source definition in a request's top-level context has stable lexical
    # inputs. Locals/actual arguments use their own lazy binding memos instead.
    memo <- !is.null(index$document_cache) && !length(setdiff(names(bindings), ".__member_position__"))
    cache_context <- index$document_context_key
    if (isTRUE(budget$track_returns)) cache_context <- paste0("returns:", cache_context)
    memo_key <- paste(index$package, cache_context, id, paste(at, collapse = ":"), sep = ":")
    if (memo && exists(memo_key, index$document_cache, inherits = FALSE)) return(index$document_cache[[memo_key]])
    cache <- if (is.null(budget$origin_name)) index$document_binding_cache
    persistent_key <- if (memo && !is.null(cache) && !is.null(item$cache_prefix) &&
            (!is.null(item$setter) || !member_head(item$expr, "function"))) {
        paste(index$package, cache_context, id, item$cache_prefix, sep = ":")
    }
    cached <- analysis_cache_get(cache, persistent_key)
    method_types <- cached$value$s7_method_types
    if (cached$hit && is.null(method_types)) {
        value <- cached$value
        index$document_cache[[memo_key]] <- value
        return(value)
    }
    env <- bindings
    # A top-level declaration uses the document's class names. A caller can
    # have a same-named argument while invoking the documented function.
    hint_names <- attr(item$expr, "member_type_names", exact = TRUE)
    if (length(hint_names)) env[intersect(names(env), hint_names)] <- NULL
    if (!is.null(budget$origin_name) && is.null(item$first_write)) item <- member_binding_declaration(item, budget)
    if (!member_head(item$expr, "function")) env$.__member_position__ <- item$start
    expr <- item$expr
    if (member_head(expr, ":=")) {
        bound <- member_s7_bind(expr, index, env)
        if (is.null(bound)) return(member_resolve_document(name, index, env, budget, depth + 1L, c(trail, id)))
        expr <- bound[[3L]]
    }
    if (is.null(item$first_write)) {
        value <- member_infer(expr, index, env, depth = depth + 1L, trail = c(trail, id), budget = budget)
    } else {
        first <- item$first_write
        env$.__member_position__ <- history[[first]]$start
        value <- member_resolve_document(name, index, env, budget, depth + 1L, c(trail, id))
        # Only exact dispatch keys survive an edit. The generic and method
        # formals come from the current source, including their lexical scope.
        cache_methods <- !is.null(persistent_key) && !is.null(value$s7_methods) && !length(value$s7_methods)
        if (cache_methods && length(method_types)) {
            recent <- seq.int(position - length(method_types) + 1L, position)
            value$s7_methods <- Map(function(types, i) list(types = types, formals = history[[i]]$expr[[2L]]),
                method_types, recent)
            index$document_cache[[memo_key]] <- value
            return(value)
        }
        for (i in seq.int(first, position)) {
            write <- history[[i]]
            env$.__member_position__ <- write$start
            if (identical(write$setter, "r6_control")) {
                if (!is.null(budget$origin_name) && !is.null(write$expr)) write <- member_binding_declaration(write, budget)
                value <- member_r6_apply_control(value, write$expr, index, env, budget)
                next
            }
            if (!is.null(write$setter) && identical(member_s7_method(write$setter, index, env), "method<-")) {
                source <- if (isTRUE(budget$s7_implementation) && is.null(write$declaration)) {
                    list(uri = budget$document$uri, start = write$start, end = write$end)
                }
                generic <- member_s7_register_signature(value, write$setter, write$expr, index, env, budget, source)
                if (isTRUE(budget$exhausted)) return(member_value(reason = "budget"))
                if (!is.null(generic)) {
                    if (cache_methods && !member_head(write$expr, "function")) cache_methods <- FALSE
                    value <- generic
                    next
                }
            }
            cache_methods <- FALSE
            if (!identical(name, write$writer) && (identical(value$type, "list") ||
                        (isTRUE(value$s7) && !member_s7_reference(value)))) next
            if (!is.null(write$setter) && isTRUE(value$s7)) {
                env[write$writer] <- list(value)
                updated <- member_s7_property_write(write$setter, value,
                    member_infer(write$expr, index, env, depth = depth + 1L, trail = c(trail, id), budget = budget),
                    index, env, budget)
                env[write$writer] <- NULL
                if (!is.null(updated)) {
                    value <- updated
                    next
                }
            }
            if (is.null(write$field) || (!isTRUE(write$slot) && !member_plain_container(value) &&
                        !isTRUE(value$s7_exact_class))) {
                value <- member_value(reason = "unknown_mutation")
                break
            }
            if (!is.null(budget$origin_name)) write <- member_binding_declaration(write, budget)
            env[write$writer] <- list(value)
            rhs <- member_infer(write$expr, index, env, depth = depth + 1L, trail = c(trail, id), budget = budget)
            if (isTRUE(budget$exhausted)) return(member_value(reason = "budget"))
            value <- member_write_container(value, write$field, rhs, write$writer,
                if (identical(budget$origin_name, write$field)) write$origin, index, env, isTRUE(write$subset), slot = isTRUE(write$slot))
            if (is.null(value)) return(member_value(reason = "unknown_mutation"))
            env[write$writer] <- NULL
        }
        if (cache_methods && length(value$s7_methods) && !isTRUE(budget$exhausted) && !isTRUE(budget$transient)) {
            # In a method-only history the retained registrations are a
            # contiguous suffix following the last opaque registration.
            analysis_cache_set(cache, persistent_key, list(s7_method_types = lapply(value$s7_methods, `[[`, "types")))
        }
    }
    cached <- if (memo) member_receiver_cache_value(value, index, budget) else NULL
    if (!is.null(cached)) {
        index$document_cache[[memo_key]] <- cached
        if (!is.null(persistent_key) && member_binding_cacheable(cached)) {
            analysis_cache_set(cache, persistent_key, cached)
        }
    }
    value
}

member_cursor_value <- function(parsed, sentinel, index, bindings, budget, name = NULL, accessor = "$", context = NULL,
    path = NULL, source = NULL, key_helper = NULL, s4_new = FALSE, s7_update_list = NULL, s7_call = NULL) {
    result <- NULL
    # Find the cursor once, including formal defaults. Replaying a lexical
    # scope must not recursively visit every earlier RHS or sibling method.
    visit <- function(node, env, path, ast_path = integer(), formal_types = NULL) {
        if (member_head(node, ":=") && length(node) == 3L) {
            # Cursor recovery can stop immediately before the call's opening
            # parenthesis. Both forms identify the direct RHS callee.
            rhs <- node[[3L]]
            callee <- if (is.call(rhs) && !member_head(rhs, "::")) rhs[[1L]] else rhs
            qualified <- member_head(callee, "::")
            token <- if (qualified) callee[[3L]] else callee
            if (is.null(accessor) && !is.null(name) && identical(token, as.name(sentinel)) &&
                    is.symbol(node[[2L]]) && isTRUE(member_s7_modes(index)$bind) &&
                    identical(member_s7_method(node, index, env), ":=")) {
                if (qualified) callee[[3L]] <- as.name(name) else callee <- as.name(name)
                result <<- member_infer(callee, index, env, budget = budget)
                result$s7_bound_name <<- TRUE
                return(invisible(NULL))
            }
            bound <- member_s7_bind(node, index, env)
            if (!is.null(bound)) node <- bound
        }
        if ((s4_new || !is.null(s7_update_list)) && is.call(node) && (identical(path, 1L) || identical(path, c(1L, 3L)))) {
            head <- node[[1L]]
            qualified <- member_head(head, "::") && identical(head[[3L]], as.name(sentinel))
            if (identical(head, as.name(sentinel)) || qualified) {
                helper <- as.name(if (s4_new) "new" else if (is.character(s7_update_list)) "props<-" else "set_props")
                if (qualified) node[[1L]][[3L]] <- helper else node[[1L]] <- helper
                if (!is.null(s7_update_list)) {
                    result <<- member_s7_update_receiver(node, name, s7_update_list, index, env, budget)
                    return(invisible(NULL))
                }
                if (identical(member_s4_method(node, index, env), "new")) {
                    target <- member_s4_arguments(node)$Class
                    if (!is.character(target)) {
                        inferred <- member_infer(target, index, env, budget = budget)
                        target <- if (isTRUE(inferred$known_literal)) inferred$literal
                    }
                    if (is.character(target) && length(target) == 1L && !is.na(target)) {
                        result <<- member_s4_constructor(list(name = target), index, env, budget, new = TRUE)
                    }
                    return(invisible(NULL))
                }
            }
        }
        if (!is.null(key_helper) && length(path) == 1L && is.call(node) &&
                identical(node[[path[[1L]]]], as.name(sentinel))) {
            replacement <- key_helper == "prop<-"
            if (replacement) node <- member_s7_property_setter(node, simple_receiver = FALSE)
            helper <- if (key_helper == "slot") member_s4_method(node, index, env) else member_s7_method(node, index, env)
            if (identical(helper, key_helper)) {
                key <- if (replacement) "name" else member_key_formals[[key_helper]]
                args <- member_s7_arguments(node, c("object", key, if (replacement) "check"))
                if (identical(args[[key]], as.name(sentinel)) && !is.null(args$object)) {
                    receiver <- member_infer(args$object, index, env, budget = budget)
                    if (key_helper == "slot") {
                        candidates <- if (identical(receiver$reason, "type_hint_union")) receiver$possible_returns else list(receiver)
                        if (!any(vapply(candidates, function(value) isTRUE(value$s7), logical(1L)))) result <<- receiver
                    } else if (!replacement || !identical(receiver$reason, "type_hint_union")) {
                        result <<- member_s7_property_receiver(receiver, env)
                    }
                }
            }
            return(invisible(NULL))
        }
        if (is.null(accessor) && is.symbol(node) && identical(member_name(node), sentinel)) {
            if (!is.null(key_helper)) return(invisible(NULL))
            result <<- if (is.null(name)) {
                internal <- c(".__member_position__", ".__s4_classes__", ".__member_properties__",
                    ".__member_context_bindings__", ".__member_context_locals__")
                value <- member_value(type = "environment", fields = env[setdiff(names(env), internal)])
                value$context_bindings <- env$.__member_context_bindings__
                value$context_locals <- unique(env$.__member_context_locals__)
                value
            } else {
                callee <- member_infer(as.name(name), index, env, budget = budget)
                if (length(callee$s7_methods) && !is.null(s7_call)) {
                    callee <- member_s7_call_signature(callee, s7_call, index, env, budget)
                }
                callee
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
            if (identical(accessor, "[[") && member_operator_masked(accessor, index, env)) {
                result <<- member_value(reason = "unresolved_subset_dispatch")
            } else if (identical(accessor, "[[") && !is.null(name)) {
                node[[3L]] <- name
                result <<- member_infer(node, index, env, budget = budget)
            } else if (is.null(name)) {
                result <<- member_infer(node[[2L]], index, env, budget = budget)
                if (isTRUE(result$s7_exact_class) && accessor %in% c("$", "[[")) {
                    storage <- member_s7_environment_storage(result, accessor, index, env)
                    if (!is.null(storage)) result <<- storage
                }
                if (identical(accessor, "$") && isTRUE(result$default_dollar)) {
                    context <- member_package_context(index, result$member_package)
                    if (is.null(context) || !member_default_dollar(result, context, env)) {
                        if (member_dataframe_columns(result)) {
                            result$column_names_only <<- TRUE
                            result$default_dollar <<- NULL
                            result$reason <<- "unresolved_dollar_dispatch"
                        } else {
                            result <<- member_value(reason = "unresolved_dollar_dispatch")
                        }
                    }
                }
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
            hints <- roxygen_inference_hints(node)$params
            if (length(hints)) lexical <- env
            for (formal in names(node[[2L]])) {
                value <- formal_types[[formal]]
                if (is.null(value) && !is.null(hints[[formal]])) {
                    value <- list(hint_types = hints[[formal]], binding_env = lexical,
                        binding_cache = new.env(hash = FALSE, parent = emptyenv()))
                }
                env[formal] <- list(if (is.null(value)) member_value(reason = "formal") else value)
            }
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
            sequence <- member_cursor_sequence(node, path, ast_path, source, !is.null(accessor) || !is.null(name), name)
            summary <- sequence$summary
            # Hash current values for dependency capture, while retaining
            # ordinary lists as immutable snapshots consumed by inference.
            lookup <- list2env(env, hash = TRUE, parent = emptyenv())
            for (i in sequence$selected) {
                child <- sequence$children[[i]]
                if (i != sequence$target && member_head(child, ":=")) {
                    bound <- member_s7_bind(child, index, env)
                    if (!is.null(bound)) child <- bound
                }
                env <- member_s4_effect(child, index, env)
                if (!is.null(env$.__s4_classes__)) lookup$.__s4_classes__ <- env$.__s4_classes__
                if (i == sequence$target) {
                    visit(child, env, sequence$path[-1L], c(sequence$ast_path, sequence$path[[1L]]))
                    break
                }
                control <- member_r6_control(child)
                changed <- if (is.null(control)) member_r6_conditional_controls(child) else control$receiver
                for (receiver in changed) {
                    generator <- member_infer(as.name(receiver), index, env, budget = budget)
                    if (!is.null(generator$r6_class_locked)) {
                        updated <- member_r6_apply_control(generator, control$call, index, env, budget)
                        for (alias in c(receiver, member_aliases(attr(env, "member_aliases"), receiver))) {
                            env[[alias]] <- lookup[[alias]] <- updated
                        }
                    }
                }
                preserved <- character()
                if ((member_head(child, "<-") || member_head(child, "=")) && is.symbol(child[[2L]])) {
                    assigned <- as.character(child[[2L]])
                    attr(env, "member_aliases") <- member_alias_assignment(attr(env, "member_aliases"), assigned, child[[3L]])
                    env[assigned] <- list(if (member_head(child[[3L]], "function")) {
                        member_infer(child[[3L]], index, env, budget = budget)
                    } else {
                        member_local_binding(child[[3L]], env,
                            if (!is.null(summary)) summary$reads[[i]] else member_syntax_names(child[[3L]]), lookup)
                    })
                } else {
                    assigned <- if (!is.null(summary)) summary$writes[[i]] else member_assigned_names(child)
                    lhs <- if (member_head(child, "<-") || member_head(child, "=")) child[[2L]]
                    field <- member_write_field(lhs)
                    for (name in assigned) {
                        value <- NULL
                        old <- member_lookup(env, name)
                        setter <- if (is.null(field)) member_s7_method_setter(lhs)
                        if (!is.null(setter) && identical(name, member_name(lhs[[2L]])) &&
                                identical(member_s7_method(setter, index, env), "method<-")) {
                            generic <- member_infer(lhs[[2L]], index, env, budget = budget)
                            generic <- member_s7_register_signature(generic, setter, child[[3L]], index, env, budget)
                            if (!is.null(generic)) {
                                for (alias in c(name, member_aliases(attr(env, "member_aliases"), name))) {
                                    env[[alias]] <- lookup[[alias]] <- generic
                                }
                                next
                            }
                        }
                        aliases <- member_aliases(attr(env, "member_aliases"), name)
                        direct <- !is.null(field) && identical(name, member_name(lhs[[2L]]))
                        property <- if (is.null(field)) member_s7_property_setter(lhs)
                        if (direct || !is.null(property) || length(aliases)) {
                            object <- member_infer(as.name(name), index, env, budget = budget)
                            if (direct) {
                                value <- member_write_container(object, field,
                                    member_infer(child[[3L]], index, env, budget = budget), name,
                                    if (identical(budget$origin_name, field)) attr(lhs, "member_origin", exact = TRUE),
                                    index, env, member_head(lhs, "[["), slot = member_head(lhs, "@"))
                            } else if (!is.null(property) && identical(name, member_name(lhs[[2L]]))) {
                                value <- member_s7_property_write(property, object,
                                    member_infer(child[[3L]], index, env, budget = budget), index, env, budget)
                            }
                            if ((identical(object$type, "environment") && member_plain_container(object)) ||
                                    (isTRUE(object$s7) && member_s7_reference(object))) {
                                for (alias in aliases) {
                                    env[alias] <- list(if (is.null(value)) member_value(reason = "unknown_local_write") else value)
                                    lookup[[alias]] <- env[[alias]]
                                }
                            }
                        }
                        if (is.null(value)) value <- member_r6_local_write(child, name, old, index, budget)
                        env[name] <- list(if (is.null(value)) member_value(reason = "unknown_local_write") else value)
                        if (!is.null(value)) preserved <- c(preserved, name)
                    }
                }
                for (name in assigned) lookup[[name]] <- env[[name]]
                assigned <- setdiff(assigned, preserved)
                env$.__member_context_bindings__ <- setdiff(env$.__member_context_bindings__, assigned)
                if (!is.null(env$.__member_context_locals__)) {
                    env$.__member_context_locals__ <- c(env$.__member_context_locals__, assigned)
                }
            }
        } else {
            # A member can be the callee of a call in the complete context.
            if (length(path)) {
                selected <- node[[path[[1L]]]]
                types <- if (is.call(node) && member_head(selected, "function")) {
                    if (member_head(node, "<-") || member_head(node, "=")) {
                        member_s7_method_formals(node, selected, index, env, budget)
                    } else {
                        s4 <- member_s4_method_formals(node, selected, index, env, budget)
                        if (!is.null(s4)) s4 else member_s7_callback_formals(node, path[[1L]], root, ast_path, index, env, budget)
                    }
                }
                visit(selected, env, path[-1L], c(ast_path, path[[1L]]), types)
            }
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
member_recover_scope <- function(document, point, cursor, data, budget = NULL) {
    origin_uri <- if (!is.null(budget$origin_name)) document$uri
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
        source_index <- member_item_source(document, data, lo)
        syntax <- NULL
        contains <- function(ref) {
            !is.null(ref) && member_before(c(ref[[1L]] - 1L, ref[[5L]] - 1L), at) &&
                member_before(at, c(ref[[3L]] - 1L, ref[[6L]]))
        }
        recover_node <- function(node, ref, ast_path) {
            if (!contains(ref)) return(node)
            lines <- document_cursor_lines(document, seq.int(ref[[1L]], ref[[3L]]), point$row)
            offset <- point$row - ref[[1L]] + 2L
            lines[[offset]] <- paste0(cursor$before, sentinel,
                substring(lines[[offset]], cursor$end + 1L))
            # Trim the source range after replacing the token: its last column
            # moves when the cursor and range end share a line.
            end <- ref[[6L]]
            if (point$row == ref[[3L]] - 1L) end <- end + nchar(sentinel) - (cursor$end - cursor$start)
            lines[[length(lines)]] <- substr(lines[[length(lines)]], 1L, end)
            lines[[1L]] <- substring(lines[[1L]], ref[[5L]])
            edit <- list(row = point$row, start = cursor$start, end = cursor$end, inserted = nchar(sentinel))
            parsed <- tryCatch(member_parse_recovered(lines, origin_uri, ref[[1L]] - 1L, ref[[5L]] - 1L, edit, data$type_hints),
                error = function(e) NULL)
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
                    srcfile <- attr(ref, "srcfile")
                    descriptor <- member_function_descriptor(node, ref, srcfile$.languageserver_function_parts)
                }
                if (is.null(descriptor)) {
                    if (is.null(syntax)) syntax <<- utils::getParseData(attr(ref, "srcfile"))
                    if (!is.null(syntax)) {
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
                ref <- member_brace_ref(node)
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
        expr <- if (is.null(origin_uri)) items[[lo]]$expr else member_declaration_expr(document, lo, budget)
        expr <- insert_cursor(expr)
        if (found) return(list(
            parsed = as.expression(list(expr)), sentinel = sentinel,
            context = NULL, start = items[[lo]]$start, path = found_path, source = source_index
        ))
    }
    member_recover(document, point, cursor, data, origin_uri)
}

member_resolve_cursor <- function(uri, workspace, document, point, cursor, name = NULL, origins = FALSE, origin_name = name,
    audit = NULL) {
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
    # Provenance is relevant only to this member definition request. Other
    # fields can have identical shapes even when declared on different lines.
    if (origins) {
        budget$origin_name <- origin_name
        budget$declarations <- new.env(parent = emptyenv())
        budget$document <- document
        budget$documents <- workspace$documents
        budget$s7_implementation <- isTRUE(cursor$s7_implementation)
    }
    metadata <- workspace$member_metadata
    cache <- document$member_receivers
    view <- member_workspace_view(workspace, document)
    revision <- member_document_cache_revision(document, metadata, view$key, workspace)
    # Named [[ calls resolve the full subset expression in its lexical scope,
    # including local subset methods. Other member prefixes share the receiver.
    accessor <- cursor$accessor
    key_helper <- cursor$key_helper
    # Substitute the string with a symbol in the call AST. The scope walker
    # verifies the helper and the exact argument before inferring its receiver.
    if (!is.null(key_helper)) cursor$accessor <- NULL
    named_subset <- identical(cursor$accessor, "[[") && !is.null(name)
    key <- paste(point$row, cursor$start, cursor$end, cursor$operator_row,
        cursor$operator, cursor$accessor, if (is.null(cursor$accessor) || named_subset) name, sep = ":")
    if (!is.null(key_helper)) key <- paste("slot", key_helper, key, sep = ":")
    if (isTRUE(cursor$s4_new)) key <- paste("constructor", key, sep = ":")
    if (!is.null(cursor$s7_update_list)) key <- paste("property-tag", cursor$s7_update_list, key, sep = ":")
    if (origins) key <- paste("definition", origin_name, name, key, sep = ":")
    if (isTRUE(cursor$s7_implementation)) key <- paste0("implementation:", key)
    cached <- if (!is.null(revision)) cache$get(key) else NULL
    if (!is.null(cached)) {
        cached <- unserialize(cached)
        index <- member_context_index(workspace, uri, document, cached$start, referenced = cached$referenced)
        bindings <- cached$bindings
        value <- cached$value
    } else {
        recovered <- member_recover_scope(document, point, cursor, data, budget)
        if (is.null(recovered)) return(NULL)
        if ((isTRUE(cursor$s4_new) || !is.null(cursor$s7_update_list)) && is.null(recovered$context)) {
            full <- s3_recover_call(document, list(point = point, cursor = cursor), recovered)
            if (!is.null(full)) {
                recovered$context <- full
                recovered$path <- recovered$source <- NULL
            }
        }
        source_path <- if (is.null(key_helper)) recovered$path else utils::head(recovered$path, -1L)
        referenced <- member_source_names(recovered$parsed, recovered$source, source_path, name, cursor$accessor)
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
            if (is.null(cursor$accessor) || named_subset) name, cursor$accessor, recovered$context,
            recovered$path, recovered$source, key_helper, isTRUE(cursor$s4_new), cursor$s7_update_list,
            s7_call = member_s7_cursor_call(document, point, cursor, recovered))
        cached_value <- if (!is.null(revision)) member_receiver_cache_value(value, index, budget) else NULL
        if (!is.null(cached_value)) {
            # Raw storage accounts for closure environments and makes each
            # hit independent of mutable request-local binding memos.
            record <- serialize(list(value = cached_value, bindings = bindings,
                    start = recovered$start, referenced = referenced), NULL)
            cache$set(key, record)
        }
    }
    cursor$accessor <- accessor
    if (!is.null(cursor$accessor) && !is.null(name) && !named_subset) {
        receiver <- value
        declared_union <- identical(receiver$reason, "type_hint_union")
        possible <- member_possible_receiver(receiver, index, bindings, cursor$accessor, inspect = name, budget = budget)
        if (!is.null(possible)) {
            if (!name %in% possible$members || name %in% possible$ambiguous) {
                return(list(value = member_value(reason = "ambiguous_possible_member"),
                    index = index, bindings = bindings, budget = budget))
            }
            receiver <- possible$value
        }
        value <- if (identical(cursor$accessor, "@")) member_slot(receiver, name, index, bindings, budget) else
            member_access(receiver, name, index, bindings)
        members <- member_members(receiver, index, bindings, cursor$accessor)
        key <- members[name]
        # Rename can use the complete field names already computed at this
        # site. Unknown/open or class-dispatched receivers need a fresh check.
        if (!is.null(audit) && identical(cursor$accessor, "$") && identical(receiver$type, "list") &&
                !isTRUE(receiver$open) && !length(receiver$classes) && !length(receiver$possible_returns) &&
                is.null(receiver$reason) && !isTRUE(receiver$possible_receiver) && is.null(possible) &&
                length(members) <= 64L && name %in% names(members)) audit$fields <- names(members)
        # Literal arguments cannot run project code. An ordinary source-list
        # closure returning an inert constant or a supplied positional formal
        # has no call effects, even when its other defaults are lazy promises.
        # Confirm the physical source origin separately before reusing it.
        arity <- audit$literal_arity
        if (!is.null(arity) && !is.null(audit$fields) && member_plain_container(receiver) &&
                !member_operator_masked("$", index, bindings) && is.null(value$r6_owner) &&
                member_head(value$function_expr, "function")) {
            formals <- names(value$function_expr[[2L]])
            body <- value$function_expr[[3L]]
            used <- if (is.symbol(body)) match(member_name(body), formals)
            constant <- (is.atomic(body) && !is.object(body) && length(body) == 1L) || is.null(body)
            audit$inert <- !("..." %in% formals) && arity <= length(formals) &&
                (constant || (length(used) && !is.na(used) && used <= arity))
        }
        if (is.null(value$function_key) && length(key) && !is.na(key[[1L]]) && !is.null(value$function_expr)) {
            value$function_key <- key[[1L]]
        }
        if (!is.null(possible) && !declared_union) value$possible_receiver <- TRUE
    }
    list(value = value, index = index, bindings = bindings, budget = budget)
}

member_request_budget <- function() {
    budget <- new.env(parent = emptyenv())
    budget$remaining <- 20000L
    # Assignment histories add depth without adding recursive function calls.
    # Keep a finite stack bound while allowing longer reassigned query chains.
    budget$depth_limit <- 128L
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
    clock <- proc.time()
    budget$started <- clock[[3L]]
    budget$cpu_started <- clock[[1L]] + clock[[2L]]
    budget$deadline <- budget$scheduled_deadline <- budget$started + budget$time_limit
    budget$cpu_limit <- max(2, budget$time_limit)
    budget$wall_deadline <- budget$started + max(5, budget$time_limit)
    budget
}

# Grow the allowance at existing clock checkpoints, without restarting the
# traversal or replenishing nodes. CPU time distinguishes slow analysis from
# time spent descheduled on a busy host; a wall deadline still bounds latency.
# Explicit caller deadlines remain strict and never receive an extension.
member_budget_expired <- function(budget, clock) {
    if (is.null(budget$scheduled_deadline) || !identical(budget$deadline, budget$scheduled_deadline)) {
        return(clock[[3L]] > budget$deadline)
    }
    cpu <- max(0, clock[[1L]] + clock[[2L]] - budget$cpu_started)
    if (clock[[3L]] >= budget$wall_deadline || cpu >= budget$cpu_limit) return(TRUE)
    if (clock[[3L]] <= budget$deadline) return(FALSE)
    budget$time_limit <- min(budget$cpu_limit, max(budget$time_limit * 2, cpu * 2))
    budget$deadline <- budget$scheduled_deadline <- min(budget$wall_deadline,
        clock[[3L]] + budget$time_limit - cpu)
    FALSE
}

# Format only the existing shape, without resolving children or calling object
# methods. Bound the input as well as the output of literal previews.
member_completion_tip <- function(value) {
    shorten <- function(text, max_chars = 80L) {
        if (nchar(text) > max_chars) paste0(substr(text, 1L, max_chars - 3L), "...") else text
    }
    literal <- value$literal
    if (isTRUE(value$known_literal) && !length(value$classes) &&
            !is.object(literal) && (is.null(literal) || is.atomic(literal))) {
        size <- length(literal)
        preview <- literal[seq_len(min(size, 6L))]
        attributes(preview) <- NULL
        if (is.character(preview)) preview <- substr(preview, 1L, 80L)
        summary <- deparse1(preview)
        if (size > 6L) summary <- paste0(summary, " ...")
        summary <- shorten(summary)
        return(list(summary = summary, certainty = "known value", suffix = paste0(" = ", summary)))
    }
    types <- if (!is.null(value$r6_class$name)) value$r6_class$name else
        if (length(value$classes)) value$classes else value$type
    if (!length(types) || all(types %in% c("function", ".never", ".missing"))) return(NULL)
    summary <- paste(substr(utils::head(types, 3L), 1L, 81L), collapse = " | ")
    if (length(types) > 3L) summary <- paste0(summary, " | ...")
    if (length(types) == 1L && types %in% c("list", "pairlist", "environment")) {
        fields <- names(value$fields)
        if (length(fields)) {
            field_names <- vapply(substr(utils::head(fields, 3L), 1L, 25L), shorten, character(1L), max_chars = 24L)
            field_names <- vapply(field_names, quote_completion_name, character(1L))
            if (length(fields) > 3L || isTRUE(value$open)) field_names <- c(field_names, "...")
            summary <- paste0(summary, "(", paste(field_names, collapse = ", "), ")")
        }
    }
    summary <- shorten(summary)
    list(summary = summary, certainty = "inferred", suffix = paste0(": ", summary))
}

member_completion <- function(uri, workspace, document, point, snippet_support, limit,
    label_details_support = FALSE) {
    cursor <- member_cursor(document, point)
    bracket <- identical(cursor$accessor, "[[")
    resolved <- member_resolve_cursor(uri, workspace, document, point, cursor)
    if (is.null(resolved)) {
        return(if (bracket) list() else NULL)
    }
    value <- resolved$value
    declared_union <- identical(value$reason, "type_hint_union")
    if (bracket && !member_plain_container(value) &&
            !(declared_union && all(vapply(value$possible_returns, member_plain_container, logical(1L))))) return(list())
    candidate <- member_possible_receiver(value, resolved$index, resolved$bindings, cursor$accessor, inspect = character())
    possible <- !is.null(candidate)
    common <- NULL
    ambiguous <- character()
    if (possible) {
        sets <- candidate$sets
        common <- candidate$members
        if (!length(common)) return(list())
        value <- candidate$value
    }
    index <- member_package_context(resolved$index, value$member_package)
    bindings <- resolved$bindings
    budget <- resolved$budget
    if (identical(value$reason, "unresolved_dollar_dispatch") && !member_dataframe_columns(value)) return(list())
    if (is.null(index) || is.null(value) || !length(value$type)) {
        return(if (!is.null(cursor$key_helper)) list() else NULL)
    }
    members <- if (possible) sets[[1L]][common] else member_members(value, index, bindings, cursor$accessor)
    labels <- as.character(names(members))
    if (!length(labels)) {
        return(list())
    }
    # Filter before pruning so unrelated labels cannot consume the limit.
    matches <- sort(labels[fuzzy_find(labels, cursor$token)])
    keep <- completion_select_indices(matches, matches, cursor$token, limit)
    labels <- matches[keep]
    if (possible) ambiguous <- member_ambiguous_names(resolved$value, sets, labels,
        resolved$index, bindings, cursor$accessor, budget)
    following <- substring(document$line0(point$row), cursor$end + 1L)
    call_snippet <- snippet_support && !bracket && is.null(cursor$key_helper) && !startsWith(trimws(following), "(")
    edit_range <- range(document$to_lsp_position(point$row, cursor$start),
        document$to_lsp_position(point$row, cursor$end))
    items <- lapply(labels, function(label) {
        shape <- if (identical(cursor$accessor, "@")) {
            if (!label %in% ambiguous) member_slot(value, label, index, bindings, budget)
        } else {
            value$fields[[label]]
        }
        if (!is.null(shape)) shape <- member_r6_member_info(shape, value)
        key <- members[[label]]
        symbol <- member_symbol_info(label, shape, key, index)
        if (label %in% ambiguous) {
            symbol$signature <- symbol$function_id <- NULL
            symbol$package <- symbol$description <- symbol$comments <- NULL
        }
        is_function <- !is.null(shape$function_expr) || !is.null(shape$result_shape) ||
            !is.null(shape$function_key) || (!is.null(key) && !is.na(key))
        if (label %in% ambiguous && any(vapply(sets, function(set) is.na(set[[label]]), logical(1L)))) is_function <- FALSE
        inserted <- if (!is.null(cursor[["quote"]])) encodeString(label, quote = cursor[["quote"]]) else quote_completion_name(label)
        snippet <- is_function && call_snippet
        text <- if (snippet) paste0(escape_completion_snippet(inserted), "($0)") else inserted
        signature <- symbol$signature
        documentation_id <- symbol$function_id
        # Conflicting candidates and data-frame columns do not establish the
        # accessed value from the first candidate's underlying storage.
        tip <- if (!is_function && !label %in% ambiguous && !member_dataframe_columns(value) &&
                !identical(value$reason, "unresolved_dollar_dispatch")) member_completion_tip(shape)
        item <- list(
            label = label, kind = if (is_function) CompletionItemKind$Method else CompletionItemKind$Field,
            detail = if (!is.null(signature)) {
                if (possible) paste0(signature, " [possible receiver]") else signature
            } else if (!is.null(tip) && (identical(tip$certainty, "known value") || cursor$accessor != "@")) {
                paste0(tip$summary, " [", if (possible) "possible receiver" else tip$certainty, "]")
            } else if (identical(cursor$accessor, "@") && !label %in% ambiguous) {
                paste0(label, ": ", paste(value$slot_types[[label]], collapse = " | "))
            } else {
                if (possible) "[possible member]" else "[static member]"
            }, sortText = label,
            documentation = if (!is.null(symbol$description)) list(kind = "markdown", value = symbol$description),
            filterText = label, insertTextFormat = if (snippet) InsertTextFormat$Snippet else InsertTextFormat$PlainText,
            textEdit = text_edit(edit_range, text),
            data = list(
                type = "member", package = symbol$package, function_id = documentation_id,
                signature = signature, context_uri = uri, generation = index$generation, comments = symbol$comments
            )
        )
        if (isTRUE(label_details_support) && (!is.null(signature) || !is.null(tip) || possible)) item <-
            completion_item_label_details(item, signature, tip,
                if (possible) "possible receiver" else if (!is.null(tip)) tip$certainty else "static")
        item
    })
    if (length(matches) > limit || budget$exhausted) attr(items, "truncated") <- TRUE
    if (possible && !declared_union) attr(items, "truncated") <- TRUE
    items
}

# Ordinary document/workspace items may represent a class generator, instance,
# or method alias. Enrich the already-pruned list using one bounded lexical
# inference context, leaving unknown symbols to their existing providers.
member_class_completion_info <- function(uri, workspace, document, point, token, items, resolved = NULL) {
    if (!length(items) || !is.null(token$package) || !identical(token$accessor, "")) return(items)
    if (isFALSE(resolved)) return(items)
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
        if (startsWith(label, "`")) label <- tryCatch(as.character(str2lang(label)), error = function(e) label)
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
