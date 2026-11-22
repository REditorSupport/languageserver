# Accepted-parse indexes contain only syntax. Scope records are keyed by AST
# paths, so they survive worker serialization without retaining XML pointers.
member_source_key <- function(path) paste(c("root", path), collapse = ":")

member_function_parts <- function(data) {
    parts <- new.env(hash = TRUE, parent = emptyenv())
    functions <- data$parent[data$token %in% c("FUNCTION", r"('\\')")]
    rows <- which(data$token == "expr" & data$parent %in% functions)
    children <- split(rows, data$parent[rows])
    for (row in which(data$id %in% functions)) {
        selected <- children[[as.character(data$id[[row]])]]
        selected <- selected[order(data$line1[selected], data$col1[selected])]
        key <- paste(data$line1[[row]], data$col1[[row]], data$line2[[row]], data$col2[[row]], sep = ":")
        parts[[key]] <- lapply(selected, function(i) {
            c(data$line1[[i]], 0L, data$line2[[i]], 0L, data$col1[[i]], data$col2[[i]])
        })
    }
    parts
}

# Source references share one srcfile environment. Retain the small function
# part table needed by recovery instead of its duplicate complete parse table.
member_compact_source <- function(parsed, parts) {
    srcfile <- attr(parsed, "srcfile")
    if (!is.environment(srcfile)) return(0)
    srcfile$.languageserver_function_parts <- parts
    if (exists("parseData", envir = srcfile, inherits = FALSE)) rm("parseData", envir = srcfile)
    as.numeric(utils::object.size(srcfile$lines)) + as.numeric(utils::object.size(as.list(parts)))
}

member_function_descriptor <- function(node, ref, parts) {
    if (is.null(parts) || is.null(ref)) return(NULL)
    key <- paste(ref[[1L]], ref[[5L]], ref[[3L]], ref[[6L]], sep = ":")
    ranges <- parts[[key]]
    defaults <- which(vapply(as.list(node[[2L]]), function(x) !identical(x, quote(expr = )), logical(1L)))
    if (length(ranges) == length(defaults) + 1L) list(defaults = defaults, refs = ranges) else NULL
}

# A direct write on a fresh local container can be sliced by receiver.
# Inherited receivers and explicit aliases keep the full effect path, including
# aliases established in enclosing blocks whose bindings are not indexed here.
member_scope_field_writes <- function(children, assigned, writes) {
    fields <- rep("", length(children))
    candidates <- !nzchar(assigned) & lengths(writes) == 1L
    if (!any(candidates)) return(fields)

    aliases <- lapply(which(nzchar(assigned)), function(i) {
        rhs <- children[[i]][[3L]]
        if (is.symbol(rhs)) c(assigned[[i]], as.character(rhs))
    })
    aliases <- unique(unlist(aliases, use.names = FALSE))
    fresh <- new.env(hash = TRUE, parent = emptyenv())
    for (i in seq_along(children)) {
        name <- assigned[[i]]
        child <- children[[i]]
        if (nzchar(name)) {
            rhs <- child[[3L]]
            head <- if (is.call(rhs)) rhs[[1L]]
            if (member_head(head, "::") && identical(head[[2L]], as.name("base"))) head <- head[[3L]]
            fresh[[name]] <- !name %in% aliases && is.symbol(head) &&
                as.character(head) %in% c("list", "new.env")
        } else if (candidates[[i]] && isTRUE(fresh[[writes[[i]]]]) &&
                (member_head(child, "<-") || member_head(child, "=")) &&
                !is.null(member_write_field(child[[2L]])) &&
                identical(member_name(child[[2L]][[2L]]), writes[[i]])) {
            fields[[i]] <- writes[[i]]
        } else {
            # A branch, dynamic replacement, or additional write can change
            # the receiver independently of its local constructor.
            for (name in writes[[i]]) fresh[[name]] <- FALSE
        }
    }
    fields
}

member_scope_index <- function(node) {
    scope <- member_statement_index(as.list(node)[-1L])
    refs <- attr(node, "srcref")
    if (length(refs) == length(node)) scope$refs <- lapply(refs[-1L], as.integer)
    scope
}

member_statement_index <- function(children) {
    assigned <- vapply(children, function(child) {
        if ((member_head(child, "<-") || member_head(child, "=")) && is.symbol(child[[2L]])) {
            as.character(child[[2L]])
        } else {
            ""
        }
    }, character(1L))
    reads <- lapply(seq_along(children), function(i) {
        member_syntax_names(if (nzchar(assigned[[i]])) children[[i]][[3L]] else children[[i]])
    })
    writes <- lapply(seq_along(children), function(i) {
        if (nzchar(assigned[[i]])) assigned[[i]] else member_assigned_names(children[[i]])
    })
    fields <- member_scope_field_writes(children, assigned, writes)
    tracked <- ifelse(nzchar(assigned), assigned, fields)
    mandatory <- which(!nzchar(tracked) | assigned %in% c("$<-", "[[<-") | startsWith(assigned, "$.") | startsWith(assigned, "[[.") | vapply(reads, function(names) {
        any(names %in% c("setClass", "setClassUnion"))
    }, logical(1L)))
    histories <- split(which(nzchar(tracked)), tracked[nzchar(tracked)])
    scope <- list(assigned = assigned, reads = reads, writes = writes, mandatory = mandatory,
        histories = list2env(histories, hash = TRUE, parent = emptyenv()))
    dotted <- grep(".", names(histories), fixed = TRUE, value = TRUE)
    if (length(dotted)) scope$s3_names <- dotted
    scope
}

# Deferred scope indexes are immutable and bounded to the accepted document.
# The document drops its cache on edits and when a new worker result arrives.
member_item_source <- function(document, data, i) {
    item <- data$items[[i]]
    if (!isTRUE(item$deferred_source)) return(item$source)
    cache <- document$member_source_cache
    if (is.null(cache)) {
        cache <- ByteLruCache$new(1024^2, max_entries = 16L)
        document$member_source_cache <- cache
    }
    key <- as.character(i)
    if (cache$has(key)) return(cache$get(key))
    index <- member_source_index(item$expr, data$deferred_parts)
    cache$set(key, index, additional_bytes = if (is.null(index)) 0 else index$bytes)
    index
}

member_source_index <- function(expr, parts) {
    scopes <- new.env(hash = TRUE, parent = emptyenv())
    routes <- new.env(hash = TRUE, parent = emptyenv())
    functions <- new.env(hash = TRUE, parent = emptyenv())
    contexts <- new.env(hash = TRUE, parent = emptyenv())
    bytes <- 0
    walk <- function(node, path, ref = NULL, in_block = FALSE) {
        if (!is.call(node)) return(NULL)
        key <- member_source_key(path)
        children <- list()
        if (member_head(node, "function")) {
            ref <- if (length(node) >= 4L) node[[4L]] else ref
            if (!is.null(ref)) {
                part_key <- paste(ref[[1L]], ref[[5L]], ref[[3L]], ref[[6L]], sep = ":")
                defaults <- which(vapply(as.list(node[[2L]]), function(x) !identical(x, quote(expr = )), logical(1L)))
                ranges <- parts[[part_key]]
                if (length(ranges) == length(defaults) + 1L) {
                    functions[[key]] <- list(defaults = defaults, refs = ranges)
                    for (i in seq_along(defaults)) walk(node[[2L]][[defaults[[i]]]], c(path, 2L, defaults[[i]]), ranges[[i]])
                    walk(node[[3L]], c(path, 3L), ranges[[length(ranges)]])
                } else {
                    walk(node[[3L]], c(path, 3L))
                }
            }
            return(ref)
        }
        if (member_head(node, "{")) {
            # Adjacent standalone blocks share one flattened binding history.
            # Keep only their physical child indexes for cursor recovery.
            scope <- member_scope_index(node)
            bytes <<- bytes + as.numeric(utils::object.size(as.list(scope$histories)))
            whole <- attr(node, "wholeSrcref")
            if (!is.null(whole)) ref <- whole
            for (i in seq_along(scope$assigned)) {
                if (any(scope$reads[[i]] %in% c("function", "{"))) {
                    child <- node[[i + 1L]]
                    nested <- member_head(child, "{")
                    if (nested && length(child) > 1L && !in_block && is.null(scope$flat)) {
                        flat <- top_level_statements(as.expression(as.list(node)[-1L]))
                        scope$flat <- member_statement_index(as.list(flat))
                        bytes <<- bytes + as.numeric(utils::object.size(as.list(scope$flat$histories)))
                    }
                    walk(child, c(path, i + 1L), scope$refs[[i]], in_block = nested)
                }
            }
            scopes[[key]] <- scope
            return(ref)
        }
        for (i in seq_along(node)) {
            if (identical(node[[i]], quote(expr = ))) next
            range <- walk(node[[i]], c(path, i))
            if (!is.null(range)) children[[as.character(i)]] <- range
        }
        if (length(children)) {
            routes[[key]] <- children
            # Constructor calls can use members outside the cursor method.
            # Summarize that context once, including aliases of R6Class, but
            # leave ordinary assignments, function bodies and section lists
            # to their narrower scope summaries.
            if (!member_head(node, "<-") && !member_head(node, "=") && !member_head(node, "list")) {
                contexts[[key]] <- member_syntax_names(node)
            }
            if (is.null(ref)) {
                first <- children[[1L]]
                last <- children[[length(children)]]
                ref <- c(first[[1L]], 0L, last[[3L]], 0L, first[[5L]], last[[6L]])
            }
        }
        ref
    }
    walk(expr, 1L)
    if (!length(scopes) && !length(functions)) return(NULL)
    # Account for hashed contents separately: utils::object.size() treats environments
    # as shallow handles. The parse cache reserves these bytes on insertion.
    bytes <- bytes + sum(vapply(list(scopes, routes, functions, contexts), function(records) {
        as.numeric(utils::object.size(as.list(records)))
    }, numeric(1L)))
    list(scopes = scopes, routes = routes, functions = functions, contexts = contexts, bytes = bytes)
}

# Package selection needs the syntax actually replayed along this path. Keep
# each statement's summary, rather than rediscovering all sibling expressions.
member_source_names <- function(parsed, source, path, name = NULL, accessor = "$") {
    if (is.null(source) || is.null(path)) return(member_syntax_names(parsed))
    node <- parsed
    ast_path <- integer()
    referenced <- character()
    narrow <- !is.null(name) || !is.null(accessor)
    while (length(path)) {
        summary <- source$scopes[[member_source_key(ast_path)]]
        if (!is.null(summary)) {
            flattened <- !is.null(summary$flat)
            if (flattened) {
                sequence <- member_cursor_sequence(node, path, ast_path, source, narrow, name)
                selected <- sequence$selected
                if (!narrow) selected <- selected[selected < sequence$target]
                summary <- sequence$summary
                ast_path <- c(sequence$ast_path, sequence$path[[1L]])
                path <- sequence$path[-1L]
                node <- sequence$children[[sequence$target]]
            } else {
                target <- path[[1L]] - 1L
                reads <- if (target <= length(summary$reads)) summary$reads[[target]] else character()
                selected <- if (narrow) member_scope_slice(summary, target, union(name, reads)) else seq_len(target - 1L)
            }
            selected <- selected[selected <= length(summary$reads)]
            referenced <- c(referenced, unlist(summary$reads[selected], use.names = FALSE))
            if (flattened) next
        }
        # R6 context includes members outside the cursor method. Their source
        # package roots must remain available when constructing that context.
        context <- source$contexts[[member_source_key(ast_path)]]
        if (!is.null(context)) return(unique(c(referenced, context)))
        ast_path <- c(ast_path, path[[1L]])
        node <- node[[path[[1L]]]]
        path <- path[-1L]
    }
    unique(c(referenced, member_syntax_names(node)))
}

# Start at the cursor and every preceding mandatory effect. Each read needs
# its latest preceding assignment or field write. A field write also reads
# its receiver, so following its reads retains earlier writes to that object.
member_scope_slice <- function(scope, target, needed) {
    if (target <= 1L) return(target)
    mandatory <- scope$mandatory[scope$mandatory < target]
    pending <- integer(target)
    count <- length(mandatory) + 1L
    pending[seq_len(count)] <- c(target, mandatory)
    selected <- rep(FALSE, target)
    selected[c(target, mandatory)] <- TRUE
    head <- 1L
    while (head <= count) {
        row <- pending[[head]]
        reads <- if (row == target) needed else scope$reads[[row]]
        for (name in reads) {
            history <- scope$histories[[name]]
            if (!length(history)) next
            position <- findInterval(row - 1L, history)
            if (!position) next
            previous <- history[[position]]
            if (!selected[[previous]]) {
                selected[[previous]] <- TRUE
                count <- count + 1L
                pending[[count]] <- previous
            }
        }
        head <- head + 1L
    }
    which(selected)
}

# Gather the preceding statements through consecutive brace children. Stop
# at the original cursor expression so calls, branches and function bodies
# retain their own traversal and their physical source paths.
member_brace_prefix <- function(children, target, path, ast_path) {
    prefix <- list()
    repeat {
        if (target > 1L) prefix[[length(prefix) + 1L]] <- children[seq_len(target - 1L)]
        child <- children[[target]]
        if (length(path) == 1L || !member_head(child, "{")) break
        ast_path <- c(ast_path, path[[1L]])
        path <- path[-1L]
        target <- path[[1L]] - 1L
        children <- as.list(child)[-1L]
    }
    before <- top_level_statements(as.expression(do.call(c, prefix)))
    list(children = c(as.list(before), list(child)), target = target, path = path, ast_path = ast_path)
}

member_cursor_sequence <- function(node, path, ast_path, source, narrow, name = NULL, call_reads = FALSE) {
    children <- if (is.expression(node)) as.list(node) else as.list(node)[-1L]
    target <- path[[1L]] - if (is.expression(node)) 0L else 1L
    summary <- source$scopes[[member_source_key(ast_path)]]
    if (!is.null(summary$flat) || (is.null(summary) && target > 1L)) {
        sequence <- member_brace_prefix(children, target, path, ast_path)
        children <- sequence$children
        path <- sequence$path
        ast_path <- sequence$ast_path
        target <- length(children)
        physical <- source$scopes[[member_source_key(ast_path)]]
        reads <- if (narrow && sequence$target <= length(physical$reads)) physical$reads[[sequence$target]]
        summary <- summary$flat
        if (narrow && is.null(summary)) {
            # Partial parses have no accepted scope index.
            summary <- member_statement_index(children)
            reads <- summary$reads[[target]]
        }
    } else {
        reads <- if (narrow && target <= length(summary$reads)) summary$reads[[target]]
    }
    selected <- if (narrow && !is.null(summary)) {
        if (call_reads) {
            # The recovered call can include arguments beyond the cursor.
            # S3 also reads the queried generic's method bindings implicitly.
            methods <- summary$s3_names
            if (length(methods)) methods <- methods[startsWith(methods, paste0(name, "."))]
            reads <- c(member_syntax_names(children[[target]]), methods, "UseMethod", "class<-")
        }
        member_scope_slice(summary, target, union(name, reads))
    } else seq_len(target)
    list(children = children, target = target, summary = summary, selected = selected,
        path = path, ast_path = ast_path)
}

member_cursor_path <- function(node, sentinel, accessor) {
    if ((is.null(accessor) && is.symbol(node) && identical(member_name(node), sentinel)) ||
            (member_head(node, accessor) && identical(member_name(node[[3L]]), sentinel))) return(integer())
    if (!is.call(node) && !is.expression(node) && !is.pairlist(node) && !is.list(node)) return(NULL)
    for (i in rev(seq_along(node))) {
        if (identical(node[[i]], quote(expr = ))) next
        path <- member_cursor_path(node[[i]], sentinel, accessor)
        if (!is.null(path)) return(c(i, path))
    }
    NULL
}
