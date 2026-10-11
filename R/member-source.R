# Accepted-parse indexes contain only syntax. Scope records are keyed by AST
# paths, so they survive worker serialization without retaining XML pointers.
member_source_key <- function(path) paste(c("root", path), collapse = ":")

member_function_parts <- function(data) {
    parts <- new.env(hash = TRUE, parent = emptyenv())
    functions <- data$parent[data$token %in% c("FUNCTION", "'\\\\'")]
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

member_scope_index <- function(node) {
    children <- as.list(node)[-1L]
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
    mandatory <- which(!nzchar(assigned) | vapply(reads, function(names) {
        any(names %in% c("setClass", "setClassUnion"))
    }, logical(1L)))
    histories <- split(which(nzchar(assigned)), assigned[nzchar(assigned)])
    refs <- attr(node, "srcref")
    list(assigned = assigned, reads = reads, writes = writes, mandatory = mandatory,
        histories = list2env(histories, hash = TRUE, parent = emptyenv()),
        refs = if (length(refs) == length(node)) lapply(refs[-1L], as.integer) else NULL)
}

member_source_index <- function(expr, parts) {
    scopes <- new.env(hash = TRUE, parent = emptyenv())
    routes <- new.env(hash = TRUE, parent = emptyenv())
    functions <- new.env(hash = TRUE, parent = emptyenv())
    contexts <- new.env(hash = TRUE, parent = emptyenv())
    bytes <- 0
    walk <- function(node, path, ref = NULL) {
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
            scope <- member_scope_index(node)
            bytes <<- bytes + as.numeric(object.size(as.list(scope$histories)))
            scopes[[key]] <- scope
            whole <- attr(node, "wholeSrcref")
            if (!is.null(whole)) ref <- whole
            for (i in seq_along(scope$assigned)) {
                if (any(scope$reads[[i]] %in% c("function", "{"))) {
                    walk(node[[i + 1L]], c(path, i + 1L), scope$refs[[i]])
                }
            }
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
    # Account for hashed contents separately: object.size() treats environments
    # as shallow handles. The parse cache reserves these bytes on insertion.
    bytes <- bytes + sum(vapply(list(scopes, routes, functions, contexts), function(records) {
        as.numeric(object.size(as.list(records)))
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
    while (length(path)) {
        summary <- source$scopes[[member_source_key(ast_path)]]
        if (!is.null(summary)) {
            target <- path[[1L]] - 1L
            reads <- if (target <= length(summary$reads)) summary$reads[[target]] else character()
            selected <- if (is.null(name) && is.null(accessor)) seq_len(target - 1L) else
                member_scope_slice(summary, target, union(name, reads))
            selected <- selected[selected <= length(summary$reads)]
            referenced <- c(referenced, unlist(summary$reads[selected], use.names = FALSE))
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

# Resolve the latest preceding definitions of needed names. Mandatory effects
# divide the history into ordered segments; their reads and writes retain the
# same conservative precedence as a backwards scan of every statement.
member_scope_slice <- function(scope, target, needed) {
    selected <- target
    before <- target - 1L
    mandatory <- scope$mandatory[scope$mandatory < target]
    while (before > 0L) {
        latest <- 0L
        for (name in needed) {
            history <- scope$histories[[name]]
            position <- findInterval(before, history)
            if (position) latest <- max(latest, history[[position]])
        }
        if (length(mandatory)) latest <- max(latest, mandatory[[length(mandatory)]])
        if (!latest) break
        selected <- c(selected, latest)
        needed <- union(setdiff(needed, scope$assigned[[latest]]), scope$reads[[latest]])
        before <- latest - 1L
        if (length(mandatory) && mandatory[[length(mandatory)]] == latest) mandatory <- mandatory[-length(mandatory)]
    }
    rev(selected)
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
