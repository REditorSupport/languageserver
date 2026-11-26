# Only an explicit, leading parenthesized code span denotes an instance.
# Arbitrary documentation, defaults and constructor calls are never evaluated.
roxygen_has_type_span <- function(content) {
    candidate <- grep("(`", content, fixed = TRUE)
    length(candidate) > 0L && any(grepl("^\\s*#'", content[candidate]))
}

roxygen_type_block <- function(comments) {
    lines <- trimws(sub("^#'", "", comments))
    starts <- which(startsWith(lines, "@"))
    params <- list()
    returns <- NULL
    name <- "(?!\\.[0-9])[[:alpha:].][[:alnum:]_.]*"
    pattern <- paste0("(*UCP)^@(?:(?:param\\s+([^[:space:]]+))|(returns?))\\s+\\(`(",
        name, "(?:::", name, ")?)`\\)(?=\\s|[.,:]|$)")
    for (i in seq_along(starts)) {
        end <- if (i < length(starts)) starts[[i + 1L]] - 1L else length(lines)
        tag <- paste(lines[starts[[i]]:end], collapse = " ")
        match <- regexec(pattern, tag, perl = TRUE)
        parts <- regmatches(tag, match)[[1L]]
        if (!length(parts)) next
        tail <- substring(tag, attr(match[[1L]], "match.length")[[1L]] + 1L)
        if (grepl(r"(^\s*,?\s*(?:[|/]|\(`|(?i:or|and)\b))", tail, perl = TRUE)) next
        if (nzchar(parts[[3L]])) {
            returns <- parts[[4L]]
        } else {
            names <- strsplit(parts[[2L]], ",", fixed = TRUE)[[1L]]
            params[names] <- rep(list(parts[[4L]]), length(names))
        }
    }
    if (length(params) || length(returns)) list(params = params, returns = returns)
}

# Key the block to the physical position of the function keyword. A nearby
# callback or nested function has its own position even on the same line.
roxygen_type_index <- function(content, data) {
    if (!roxygen_has_type_span(content) || !length(data$token)) return(NULL)
    terminals <- which(data$terminal)
    terminals <- terminals[order(data$line1[terminals], data$col1[terminals])]
    token <- data$token[terminals]
    line <- data$line1[terminals]
    text <- data$text[terminals]
    comments <- which(token == "COMMENT" & startsWith(text, "#'"))
    comments <- comments[grepl("^\\s*#'", content[line[comments]])]
    if (!length(comments)) return(NULL)
    groups <- split(comments, cumsum(c(TRUE, diff(line[comments]) != 1L)))
    result <- list()
    for (group in groups) {
        if (!roxygen_has_type_span(text[group])) next
        end <- group[[length(group)]]
        i <- end + 1L
        if (i > length(token) || line[[i]] != line[[end]] + 1L) next
        # Ordinary assignments, including ones split before the literal.
        while (i < length(token) && token[[i]] == "SYMBOL" && token[[i + 1L]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) {
            i <- i + 2L
        }
        if (i > length(token) || !token[[i]] %in% c("FUNCTION", r"('\\')")) next
        hints <- roxygen_type_block(text[group])
        if (!is.null(hints)) result[[paste(line[[i]], data$col1[terminals[[i]]], sep = ":")]] <- hints
    }
    if (length(result)) result
}

# All function literals in a parser fragment share the same srcfile. Keep
# the sparse table there instead of walking each function's body to attach it.
roxygen_hint_source <- function(parsed, hints, row = 0L, col = 0L) {
    if (!length(hints)) return()
    source <- attr(parsed, "srcfile", exact = TRUE)
    if (is.environment(source)) source$.languageserver_type_hints <- list(values = hints, row = row, col = col)
}

member_function_hints <- function(fn) {
    hints <- attr(fn, "member_type_hints", exact = TRUE)
    if (!is.null(hints) || length(fn) < 4L) return(hints)
    ref <- fn[[4L]]
    index <- attr(ref, "srcfile", exact = TRUE)$.languageserver_type_hints
    if (is.null(index)) return(NULL)
    col <- ref[[5L]] + if (ref[[7L]] == 1L) index$col else 0L
    index$values[[paste(ref[[7L]] + index$row, col, sep = ":")]]
}

# When a metadata worker is pending, a documented receiver can depend on
# an attached package even if the source never mentions its exported roots.
member_hint_packages <- function(recovered, cursor, referenced, expressions, data, request) {
    node <- if (is.null(recovered$context)) recovered$parsed else recovered$context
    path <- if (is.null(recovered$context)) recovered$path
    if (is.null(path)) path <- member_cursor_path(node, recovered$sentinel, cursor$accessor)
    params <- list()
    for (step in path) {
        if (member_head(node, "function")) {
            formals <- names(node[[2L]])
            params[formals] <- member_function_hints(node)$params[formals]
        }
        node <- node[[step]]
    }
    types <- unlist(params[intersect(names(params), referenced)], use.names = FALSE)
    for (expr in expressions[-1L]) {
        if (member_head(expr, "function")) types <- c(types, member_function_hints(expr)$returns)
    }
    qualified <- grepl("::", types, fixed = TRUE)
    packages <- sub("::.*", "", types[qualified])
    if (length(setdiff(types[!qualified], c(names(data$bindings), names(data$s4), names(data$refclasses))))) {
        packages <- union(packages, setdiff(request$packages, startup_packages))
    }
    packages
}

member_hint_binding <- function(type, lexical) {
    keys <- intersect(c(type, ".__member_position__", ".__s4_classes__"), names(lexical))
    list(hint_type = type, binding_env = lexical[keys])
}

member_hinted_value <- function(type, index, bindings, budget) {
    parts <- strsplit(type, "::", fixed = TRUE)[[1L]]
    name <- utils::tail(parts, 1L)
    package <- if (length(parts) == 2L) parts[[1L]]
    if (type %in% budget$resolving_type_hints || isTRUE(budget$exhausted)) return(member_value())
    previous <- budget$resolving_type_hints
    budget$resolving_type_hints <- c(previous, type)
    on.exit(budget$resolving_type_hints <- previous)
    generator <- if (is.null(package)) {
        member_infer(as.name(name), index, bindings, budget = budget)
    } else if (identical(package, index$document_package)) {
        member_resolve_document(name, index, list(.__member_position__ = c(Inf, Inf)), budget, 0L, character())
    } else {
        member_lookup(member_lookup(index$namespace_indices, package)$roots, name)
    }
    if (identical(generator$r6_role, "generator")) return(generator$fields$new$result_shape)
    if (!is.null(generator$refclass_instance)) return(generator$refclass_instance)
    if (!is.null(generator$s7_generator)) return(member_s7_shape(generator$s7_generator, generator$closure))
    if (!is.null(generator$s4_generator)) return(member_s4_construct(generator$s4_generator, list(), index, bindings, budget))
    if (!is.null(member_s4_descriptor(name, index, bindings, package, budget))) return(member_s4_shape(name, index, bindings, package, budget = budget))
    owners <- if (!is.null(package)) package else unique(index$hint_packages)
    matches <- Filter(function(owner) !is.null(member_lookup(member_lookup(index$namespace_indices, owner)$classes, name)), owners)
    if (length(matches) == 1L) return(member_value(type = name, member_package = matches[[1L]]))
    member_value()
}

member_needs_type_hint <- function(value) {
    !length(value$type) && !length(value$possible_returns) && is.null(value$function_expr) &&
        is.null(value$function_key) && !isTRUE(value$known_literal) && !identical(value$reason, "unknown_branch")
}
