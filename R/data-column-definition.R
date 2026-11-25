# Locate an explicit column in a constructor without evaluating a mask or
# replaying its data pipeline. Unknown inputs have no definition target.
data_column_definition <- function(workspace, document, site, proof = FALSE) {
    parsed <- document$parse_data
    syntax <- parsed$syntax_data
    receiver <- site$receiver
    if (!receiver || isTRUE(parsed$parse_error) || length(parsed$partial_incomplete_lines)) return(NULL)
    token <- class_literal_row(syntax, receiver, "SYMBOL")
    if (token && !is.null(data_mask_receiver(parsed$data_mask, token))) return(NULL)
    constructor <- if (token) data_column_binding(parsed, token) else receiver
    if (!constructor) return(NULL)
    owns <- function(call, package, row) data_column_call_owner(workspace, document, row, call, package)
    call <- data_column_constructor(syntax, constructor, owns)
    if (is.null(call) || (site$row %in% parsed$data_mask$bracket && call$name != "data.table")) return(NULL)
    name <- class_token_text(syntax$text[[site$row]])
    field <- match(name, names(call$values), nomatch = 0L)
    if (!field) return(NULL)
    target <- location(document$uri, navigation_range(document, syntax, call$tags[[field]]))
    if (proof) list(target = target, constructor = constructor) else target
}

# A fully named literal constructor supplies both a declaration target and
# a closed set of columns for distinguishing masked and lexical reads.
data_column_constructor <- function(syntax, row, owns) {
    call <- syntax_call_arguments(syntax, row)
    packages <- c(data.frame = "base", data.table = "data.table", tibble = "tibble")
    if (is.null(call) || !call$name %in% names(packages) || !owns(call, packages[[call$name]], row)) return(NULL)
    keys <- names(call$values)
    controls <- switch(call$name, data.frame = names(formals(base::data.frame)),
        data.table = c("keep.rownames", "check.names", "key", "stringsAsFactors", "env"),
        tibble = c(".rows", ".name_repair"))
    if (!length(keys) || any(!nzchar(keys)) || anyDuplicated(keys) || any(keys %in% controls)) return(NULL)
    if (call$name == "data.frame" && !identical(make.names(keys, unique = TRUE), keys)) return(NULL)
    if (!all(vapply(call$values, data_column_atomic, logical(1L), syntax = syntax, owns = owns))) return(NULL)
    call
}

data_column_call_owner <- function(workspace, document, row, call, package) {
    if (!is.null(call$package)) return(call$package == package)
    parsed <- document$parse_data
    s <- parsed$syntax_data
    head <- s$children[[s$child_start[[row]]]]
    fn <- class_literal_row(s, head, "SYMBOL_FUNCTION_CALL")
    if (!fn || length(navigation_definitions(parsed, s$line1[[fn]], s$col1[[fn]], call$name, token = fn, last = TRUE))) return(FALSE)
    owner <- if (is.function(workspace$guess_namespace)) workspace$guess_namespace(call$name, isf = TRUE, uri = document$uri)
    if (length(owner) && owner != package) return(FALSE)
    if (package == "base") return(TRUE)
    at <- c(s$line1[[fn]] - 1L, s$col1[[fn]] - 1L)
    any(vapply(parsed$member_data$imports, function(item) {
        member_before(item$end, at) && length(item$expr) > 1L && identical(member_name(item$expr[[2L]]), package)
    }, logical(1L))) || (package %in% workspace$imported_packages && identical(owner, package))
}

data_column_atomic <- function(row, syntax, owns) {
    atom <- c("NUM_CONST", "STR_CONST")
    if (class_literal_row(syntax, row, atom)) return(TRUE)
    parts <- syntax_children(syntax, row)
    if (length(parts) == 3L && syntax$text[[parts[[2L]]]] == ":") {
        return(class_literal_row(syntax, parts[[1L]], "NUM_CONST") > 0L &&
                class_literal_row(syntax, parts[[3L]], "NUM_CONST") > 0L)
    }
    call <- syntax_call_arguments(syntax, row)
    identical(call$name, "c") && length(call$values) > 0L && !any(nzchar(names(call$values))) &&
        owns(call, "base", row) &&
        all(vapply(call$values, function(arg) class_literal_row(syntax, arg, atom) > 0L, logical(1L)))
}

data_column_binding <- function(parsed, token) {
    s <- parsed$syntax_data
    name <- class_token_text(s$text[[token]])
    line <- s$line1[[token]]
    col <- s$col1[[token]]
    declaration <- navigation_definitions(parsed, line, col, name, token = token, last = TRUE)
    if (!length(declaration)) return(0L)
    parts <- syntax_children(s, declaration)
    parts <- parts[s$token[parts] != "COMMENT"]
    if (length(parts) != 3L || !s$text[[parts[[2L]]]] %in% c("<-", "=", "->")) return(0L)
    block <- s$parent_row[[declaration]]
    if (block && s$token[[s$children[[s$child_start[[block]]]]]] != "'{'") return(0L)
    # The declaration must precede this read in the same executing scope.
    # A conditional declaration in a sibling block and a global assignment
    # outside a function cannot establish the receiver at execution time.
    parent <- s$parent_row[[token]]
    while (parent && parent != block) {
        if (s$function_close[[parent]]) return(0L)
        parent <- s$parent_row[[parent]]
    }
    if (parent != block || !member_before(c(s$line2[[declaration]], s$col2[[declaration]]), c(line, col))) return(0L)
    # Look only at mentions of this receiver between construction and use.
    # Passing it to another function or aliasing a data.table can change
    # its schema by reference, even without another assignment to its name.
    index <- parsed$reference_index
    if (!is.environment(index$by_name)) return(0L)
    # These operations can replace bindings by string or by loading code,
    # without a symbol occurrence for that binding in the parse tree.
    writers <- mget(c("assign", "rm", "remove", "load", "source", "sys.source"), index$by_name,
        ifnotfound = list(integer()), inherits = FALSE)
    writers <- unlist(writers, use.names = FALSE)
    ids <- c(reference_indices(index, name), writers[index$token[writers] == "SYMBOL_FUNCTION_CALL"])
    lines <- index$line[ids] + 1L
    cols <- index$code_point_col[ids] + 1L
    prior <- ids[(lines > s$line2[[declaration]] | (lines == s$line2[[declaration]] & cols > s$col2[[declaration]])) &
            (lines < line | (lines == line & cols < col))]
    if (length(prior)) {
        earlier <- vapply(prior, function(i) {
            navigation_token(parsed, index$line[[i]] + 1L, index$code_point_col[[i]] + 1L, "SYMBOL")
        }, integer(1L))
        if (any(!earlier)) return(0L)
        receivers <- s$parent_row[earlier]
        if (!all(receivers %in% parsed$data_mask$receiver) ||
                !all(vapply(receivers, data_column_read_only, logical(1L), s = s, block = block))) return(0L)
    }
    parts[[if (s$text[[parts[[2L]]]] == "->") 1L else 3L]]
}

# A completed, standalone mask query with simple operands leaves the table's
# declaration valid. Any use with unknown effects ends this source proof.
data_column_read_only <- function(receiver, s, block) {
    row <- s$parent_row[[receiver]]
    if (!row || s$parent_row[[row]] != block) return(FALSE)
    children <- syntax_children(s, row)
    args <- children[s$token[children] == "expr"]
    if (!length(args) || !receiver %in% args) return(FALSE)
    values <- setdiff(args[-1L], receiver)
    atom <- c("SYMBOL", "NUM_CONST", "STR_CONST")
    all(vapply(values, function(arg) {
        if (class_literal_row(s, arg, atom)) return(TRUE)
        parts <- syntax_children(s, arg)
        length(parts) == 3L && s$token[[parts[[2L]]]] %in% c("LT", "LE", "GT", "GE", "EQ", "NE") &&
            class_literal_row(s, parts[[1L]], atom) > 0L && class_literal_row(s, parts[[3L]], atom) > 0L
    }, logical(1L)))
}
