#' Compare internal code-point positions
#' @noRd
refactor_compare_point <- function(left, right) {
    if (left$row < right$row ||
            left$row == right$row && left$col < right$col) {
        -1L
    } else if (left$row > right$row ||
            left$row == right$row && left$col > right$col) {
        1L
    } else {
        0L
    }
}

#' Convert an XML parse node to an internal code-point range
#' @noRd
refactor_node_range <- function(node) {
    list(
        start = list(
            row = as.integer(xml_attr(node, "line1")) - 1L,
            col = as.integer(xml_attr(node, "col1")) - 1L
        ),
        end = list(
            row = as.integer(xml_attr(node, "line2")) - 1L,
            col = as.integer(xml_attr(node, "col2"))
        )
    )
}

#' Test internal range relationships
#' @noRd
refactor_range_equal <- function(left, right) {
    refactor_compare_point(left$start, right$start) == 0L &&
        refactor_compare_point(left$end, right$end) == 0L
}

#' @noRd
refactor_range_contains <- function(outer, inner) {
    refactor_compare_point(outer$start, inner$start) <= 0L &&
        refactor_compare_point(outer$end, inner$end) >= 0L
}

#' Extract text from an internal code-point range
#' @noRd
refactor_range_text <- function(document, item_range) {
    start <- item_range$start
    end <- item_range$end
    if (start$row == end$row) {
        return(substr(
            document$line0(start$row), start$col + 1L, end$col
        ))
    }
    lines <- document$content[(start$row:end$row) + 1L]
    lines[[1L]] <- substr(lines[[1L]], start$col + 1L, nchar(lines[[1L]]))
    lines[[length(lines)]] <- substr(
        lines[[length(lines)]], 1L, end$col
    )
    paste(lines, collapse = "\n")
}

#' Move one code-point position forward
#' @noRd
refactor_next_point <- function(document, point) {
    line_length <- nchar(document$line0(point$row))
    if (point$col < line_length) {
        list(row = point$row, col = point$col + 1L)
    } else if (point$row + 1L < document$nline) {
        list(row = point$row + 1L, col = 0L)
    } else {
        point
    }
}

#' Move one code-point position backward
#' @noRd
refactor_previous_point <- function(document, point) {
    if (point$col > 0L) {
        list(row = point$row, col = point$col - 1L)
    } else if (point$row > 0L) {
        row <- point$row - 1L
        list(row = row, col = nchar(document$line0(row)))
    } else {
        point
    }
}

#' Trim whitespace around an internal range
#' @noRd
refactor_trim_range <- function(document, item_range) {
    start <- item_range$start
    end <- item_range$end
    while (refactor_compare_point(start, end) < 0L) {
        line <- document$line0(start$row)
        character <- if (start$col < nchar(line)) {
            substr(line, start$col + 1L, start$col + 1L)
        } else {
            "\n"
        }
        if (!grepl("\\s", character, perl = TRUE)) break
        next_point <- refactor_next_point(document, start)
        if (refactor_compare_point(next_point, start) == 0L) break
        start <- next_point
    }
    while (refactor_compare_point(start, end) < 0L) {
        previous <- refactor_previous_point(document, end)
        line <- document$line0(previous$row)
        character <- if (previous$col < nchar(line)) {
            substr(line, previous$col + 1L, previous$col + 1L)
        } else {
            "\n"
        }
        if (!grepl("\\s", character, perl = TRUE)) break
        if (refactor_compare_point(previous, end) == 0L) break
        end <- previous
    }
    list(start = start, end = end)
}

#' Convert an internal range to an LSP range
#' @noRd
refactor_lsp_range <- function(document, item_range) {
    range(
        document$to_lsp_position(
            item_range$start$row, item_range$start$col
        ),
        document$to_lsp_position(item_range$end$row, item_range$end$col)
    )
}

#' Test whether a node is a sequential statement container
#' @noRd
refactor_sequence_container <- function(node) {
    name <- xml_name(node)
    identical(name, "exprlist") ||
        identical(name, "expr") &&
            length(xml_find_all(node, "OP-LEFT-BRACE")) > 0L
}

#' Find the statement and sequential container enclosing an expression
#' @noRd
refactor_statement_context <- function(node) {
    current <- node
    repeat {
        parent <- xml_parent(current)
        if (inherits(parent, "xml_missing") || !length(parent)) {
            return(NULL)
        }
        if (refactor_sequence_container(parent)) {
            return(list(statement = current, container = parent))
        }
        parent_tokens <- xml_name(xml_children(parent))
        if (any(parent_tokens %in% c(
            "IF", "ELSE", "FOR", "WHILE", "REPEAT", "FUNCTION"
        ))) {
            return(NULL)
        }
        current <- parent
    }
}

#' Test whether two XML nodes are the same node
#' @noRd
refactor_same_node <- function(left, right) {
    identical(xml_path(left), xml_path(right))
}

#' Test whether an expression can be replaced by another expression
#' @noRd
refactor_expression_movable <- function(node, statement) {
    current <- node
    repeat {
        if (refactor_same_node(current, statement)) {
            return(TRUE)
        }
        parent <- xml_parent(current)
        if (inherits(parent, "xml_missing") || !length(parent)) {
            return(FALSE)
        }
        children <- xml_children(parent)
        paths <- vapply(children, xml_path, character(1L))
        current_index <- match(xml_path(current), paths)
        if (is.na(current_index)) {
            return(FALSE)
        }
        child_names <- xml_name(children)

        # The whole RHS needs its pipe argument, but independent expressions
        # inside its arguments may move. Keep placeholders and data-mask
        # expressions in the context that supplies their bindings.
        pipes <- which(child_names %in% c("PIPE", "OP-PIPE") |
                child_names == "SPECIAL" & xml_text(children) %in% c("%>%", "%<>%", "%T>%", "%$%"))
        if (length(pipes) && current_index > pipes[[1L]]) {
            pipe <- pipes[[1L]]
            if (refactor_same_node(node, current) || xml_text(children[[pipe]]) == "%$%") return(FALSE)
            placeholder <- if (child_names[[pipe]] == "SPECIAL") {
                ".//*[self::PLACEHOLDER or (self::SYMBOL or self::SYMBOL_FUNCTION_CALL) and (text() = '.' or text() = '`.`')]"
            } else {
                ".//PLACEHOLDER"
            }
            if (length(xml_find_all(node, placeholder))) return(FALSE)
        }

        assignments <- which(child_names %in% c(
            "LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN"
        ))
        if (length(assignments)) {
            operator_index <- assignments[[1L]]
            right_assignment <- identical(
                child_names[[operator_index]], "RIGHT_ASSIGN"
            )
            if ((!right_assignment && current_index < operator_index) ||
                    (right_assignment && current_index > operator_index)) {
                return(FALSE)
            }
        }

        open_parenthesis <- which(child_names == "OP-LEFT-PAREN")
        if (length(open_parenthesis) &&
                current_index < open_parenthesis[[1L]]) {
            return(FALSE)
        }

        member_operators <- which(child_names %in% c(
            "OP-DOLLAR", "OP-AT", "NS_GET", "NS_GET_INT"
        ))
        if (length(member_operators) &&
                current_index > member_operators[[1L]]) {
            return(FALSE)
        }
        current <- parent
    }
}

#' Find an expression exactly matching a selected range
#' @noRd
refactor_exact_expression <- function(xdoc, selected_range) {
    nodes <- xml_find_all(xdoc, "//expr[@line1]")
    matches <- vapply(nodes, function(node) {
        refactor_range_equal(refactor_node_range(node), selected_range)
    }, logical(1L))
    nodes <- nodes[matches]
    if (!length(nodes)) {
        return(NULL)
    }
    nodes[[length(nodes)]]
}

#' Find contiguous statement expressions matching a selected range
#' @noRd
refactor_statement_selection <- function(xdoc, selected_range) {
    containers <- xml_find_all(xdoc, "/exprlist | //expr[OP-LEFT-BRACE]")
    result <- NULL
    for (container in containers) {
        children <- xml_children(container)
        children <- children[xml_name(children) == "expr"]
        if (!length(children)) next
        child_ranges <- lapply(children, refactor_node_range)
        selected <- which(vapply(child_ranges, function(item_range) {
            refactor_range_contains(selected_range, item_range)
        }, logical(1L)))
        if (!length(selected) || any(diff(selected) != 1L)) next
        combined <- list(
            start = child_ranges[[selected[[1L]]]]$start,
            end = child_ranges[[selected[[length(selected)]]]]$end
        )
        if (!refactor_range_equal(combined, selected_range)) next
        result <- list(
            nodes = children[selected],
            container = container,
            range = combined
        )
    }
    result
}

#' Validate that a range lies in one R source region
#' @noRd
refactor_range_in_r_region <- function(document, item_range) {
    if (!check_r_region(document, item_range$start)) {
        return(FALSE)
    }
    end_point <- refactor_previous_point(document, item_range$end)
    if (!check_r_region(document, end_point)) {
        return(FALSE)
    }
    if (!document$is_rmarkdown) {
        return(TRUE)
    }
    start_cell <- literate_r_cell_at(document$regions, item_range$start$row)
    end_cell <- literate_r_cell_at(document$regions, end_point$row)
    !is.null(start_cell) && !is.null(end_cell) &&
        identical(start_cell$start_line, end_cell$start_line)
}

#' Build the shared context for selection-based refactorings
#' @noRd
refactor_selection_context <- function(uri, workspace, document, item_range) {
    if (is.null(workspace)) {
        return(NULL)
    }
    parse_data <- current_parse_data(uri, workspace, document)
    if (is.null(parse_data) || isTRUE(parse_data$parse_error) ||
            is.null(parse_data_xml(parse_data))) {
        return(NULL)
    }
    selected_range <- refactor_trim_range(document, item_range)
    if (refactor_compare_point(
        selected_range$start, selected_range$end
    ) >= 0L ||
        !refactor_range_in_r_region(document, selected_range)) {
        return(NULL)
    }
    expression <- refactor_exact_expression(
        parse_data_xml(parse_data), selected_range
    )
    statements <- refactor_statement_selection(
        parse_data_xml(parse_data), selected_range
    )
    if (is.null(expression) && is.null(statements)) {
        return(NULL)
    }
    list(
        parse_data = parse_data,
        xdoc = parse_data_xml(parse_data),
        range = selected_range,
        expression = expression,
        statements = statements
    )
}

#' Return direct token names contained by a parse node
#' @noRd
refactor_token_names <- function(node) {
    xml_name(xml_find_all(node, ".//*[not(*)]"))
}

#' Return function-call names contained by a parse node
#' @noRd
refactor_call_names <- function(node) {
    xml_text(xml_find_all(node, ".//SYMBOL_FUNCTION_CALL"))
}

#' Detect syntax whose evaluation context cannot safely move
#' @noRd
refactor_unsafe_syntax <- function(node, extracting_function = FALSE) {
    token_names <- refactor_token_names(node)
    token_text <- xml_text(xml_find_all(
        node,
        ".//*[self::LEFT_ASSIGN or self::RIGHT_ASSIGN]"
    ))
    calls <- refactor_call_names(node)
    unsafe_calls <- c(
        "quote", "substitute", "bquote", "expression", "alist",
        "missing", "match.call", "parent.frame", "sys.call", "sys.calls",
        "sys.frame", "sys.function"
    )
    if (extracting_function) {
        unsafe_calls <- c(unsafe_calls, "return", "on.exit", "nargs")
    }
    any(token_names %in% c("OP-TILDE", "BREAK", "NEXT")) ||
        extracting_function && any(token_names %in% c("FUNCTION", "OP-LAMBDA")) ||
        any(token_text %in% c("<<-", "->>")) ||
        any(calls %in% unsafe_calls)
}

#' Parse a simple standalone assignment expression
#' @noRd
refactor_assignment_parts <- function(node) {
    if (!identical(xml_name(node), "expr")) {
        return(NULL)
    }
    children <- xml_children(node)
    children <- children[xml_name(children) != "COMMENT"]
    names <- xml_name(children)
    operators <- which(names %in% c(
        "LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN"
    ))
    if (length(operators) != 1L) {
        return(NULL)
    }
    operator_index <- operators[[1L]]
    operator <- children[[operator_index]]
    operator_text <- xml_text(operator)
    if (operator_text %in% c("<<-", "->>")) {
        return(NULL)
    }
    if (names[[operator_index]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) {
        if (operator_index <= 1L || operator_index >= length(children)) {
            return(NULL)
        }
        target <- children[[operator_index - 1L]]
        value <- children[[operator_index + 1L]]
    } else {
        if (operator_index <= 1L || operator_index >= length(children)) {
            return(NULL)
        }
        value <- children[[operator_index - 1L]]
        target <- children[[operator_index + 1L]]
    }
    target_children <- xml_children(target)
    if (length(target_children) != 1L ||
            !identical(xml_name(target_children[[1L]]), "SYMBOL")) {
        return(NULL)
    }
    list(
        node = node,
        target = target,
        target_node = target_children[[1L]],
        name = xml_text(target_children[[1L]]),
        value = value,
        operator = operator_text
    )
}

#' Find an assignment ancestor defining a symbol occurrence
#' @noRd
refactor_definition_assignment <- function(token_node) {
    current <- token_node
    repeat {
        current <- xml_parent(current)
        if (inherits(current, "xml_missing") || !length(current)) {
            return(NULL)
        }
        assignment <- refactor_assignment_parts(current)
        if (!is.null(assignment) &&
            refactor_range_contains(
                refactor_node_range(assignment$target),
                refactor_node_range(token_node)
            )) {
            return(assignment)
        }
        if (identical(xml_name(current), "exprlist")) {
            return(NULL)
        }
    }
}

#' Collect names visible to generated refactoring bindings
#' @noRd
refactor_bound_names <- function(workspace, uri) {
    result <- character()
    for (document_uri in workspace_document_uris(workspace, uri)) {
        parse_data <- workspace$get_parse_data(document_uri)
        if (is.null(parse_data)) next
        result <- c(
            result,
            names(parse_data$definitions),
            parse_data$reference_index$name
        )
    }
    unique(result[nzchar(result)])
}

#' Generate a deterministic collision-free binding name
#' @noRd
refactor_unique_name <- function(base, workspace, uri) {
    existing <- refactor_bound_names(workspace, uri)
    if (!base %in% existing) {
        return(base)
    }
    suffix <- 2L
    repeat {
        candidate <- paste0(base, "_", suffix)
        if (!candidate %in% existing) {
            return(candidate)
        }
        suffix <- suffix + 1L
    }
}

#' Test whether the workspace index is complete enough for a top-level binding
#' @noRd
refactor_index_ready <- function(workspace) {
    index <- workspace$index
    if (is.null(index)) return(TRUE)
    status <- index$indexing_status()
    if (isTRUE(index$truncated) || isTRUE(index$worker_running) ||
            !isTRUE(status$done) || status$failed > 0L) return(FALSE)
    all(vapply(workspace$documents$values(), function(document) {
        summary <- index$summaries$get(index_canonical_uri(document$uri), NULL)
        !is.null(document$parse_data) &&
            identical(document$content, document$parsed_content) &&
            identical(document$version, document$parse_data$version) &&
            (is.null(summary) || identical(summary$content_hash, document$index_content_hash))
    }, logical(1L)))
}

#' Reindent source text from its original column
#' @noRd
refactor_reindent_text <- function(text, original_col, prefix,
    trim_first = FALSE, protected_lines = integer()) {
    lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
    for (i in seq_along(lines)) {
        if (i %in% protected_lines) next
        if ((i > 1L || isTRUE(trim_first)) && original_col > 0L) {
            leading <- attr(regexpr("^[ \\t]*", lines[[i]]), "match.length")
            remove <- min(leading, original_col)
            lines[[i]] <- substr(lines[[i]], remove + 1L, nchar(lines[[i]]))
        }
        lines[[i]] <- paste0(prefix, lines[[i]])
    }
    paste(lines, collapse = "\n")
}

#' Find multiline literals and quoted names whose interior whitespace is data
#' @noRd
refactor_multiline_quoted_ranges <- function(context) {
    syntax <- context$parse_data$syntax_data
    if (!is.null(syntax)) {
        quoted <- which((syntax$token == "STR_CONST" | startsWith(syntax$token, "SYMBOL")) &
                syntax$line1 < syntax$line2)
        return(lapply(quoted, function(i) {
            list(
                start = list(row = syntax$line1[[i]] - 1L, col = syntax$col1[[i]] - 1L),
                end = list(row = syntax$line2[[i]] - 1L, col = syntax$col2[[i]])
            )
        }))
    }
    quoted <- xml_find_all(context$xdoc,
        "//*[@line1 < @line2 and (self::STR_CONST or starts-with(name(), 'SYMBOL'))]")
    lapply(quoted, refactor_node_range)
}

#' Protect leading characters belonging to a multiline literal or quoted name
#' @noRd
refactor_protected_quoted_lines <- function(context) {
    rows <- unique(unlist(lapply(refactor_multiline_quoted_ranges(context), function(item_range) {
        first <- max(item_range$start$row + 1L, context$range$start$row)
        last <- min(item_range$end$row, context$range$end$row)
        if (first <= last) seq.int(first, last) else integer()
    }), use.names = FALSE))
    as.integer(rows - context$range$start$row + 1L)
}

#' Avoid editor indentation changes inside multiline literals and quoted names
#' @noRd
refactor_snippet_preserves_quoted_text <- function(context, insertion_point) {
    !any(vapply(refactor_multiline_quoted_ranges(context), function(item_range) {
        refactor_compare_point(item_range$start, context$range$end) < 0L &&
            refactor_compare_point(item_range$end, insertion_point) > 0L
    }, logical(1L)))
}

#' Build non-overlapping insertion and replacement edits
#' @noRd
refactor_insert_and_replace <- function(document, insertion_point,
    insertion_text, replacement_range, replacement_text, snippet = FALSE) {
    if (snippet) {
        # One snippet links the declaration and its use. Separate snippet
        # edits do not share tab stops, so include the unchanged text between
        # the insertion point and the selection in the same bounded edit.
        between <- refactor_range_text(document, list(
            start = insertion_point, end = replacement_range$start
        ))
        return(list(list(
            range = refactor_lsp_range(document, list(
                start = insertion_point, end = replacement_range$end
            )),
            snippet = list(kind = "snippet", value = paste0(
                insertion_text, escape_completion_snippet(between), replacement_text
            ))
        )))
    }
    if (refactor_compare_point(
        insertion_point, replacement_range$start
    ) == 0L) {
        return(list(text_edit(
            refactor_lsp_range(document, replacement_range),
            paste0(insertion_text, replacement_text)
        )))
    }
    list(
        text_edit(
            range(
                document$to_lsp_position(
                    insertion_point$row, insertion_point$col
                ),
                document$to_lsp_position(
                    insertion_point$row, insertion_point$col
                )
            ),
            insertion_text
        ),
        text_edit(
            refactor_lsp_range(document, replacement_range),
            replacement_text
        )
    )
}

#' Return indentation and insertion metadata for a statement
#' @noRd
refactor_insertion_context <- function(document, statement) {
    statement_range <- refactor_node_range(statement)
    row <- statement_range$start$row
    line <- document$line0(row)
    prefix <- substr(line, 1L, statement_range$start$col)
    if (nzchar(prefix) && !grepl("^[ \\t]*$", prefix)) {
        return(NULL)
    }
    list(
        point = list(row = row, col = 0L),
        indent = prefix,
        statement_range = statement_range
    )
}

#' Test whether an expression is only a single atomic token
#' @noRd
refactor_atomic_expression <- function(node) {
    terminals <- xml_find_all(node, ".//*[not(*)]")
    length(terminals) == 1L && xml_name(terminals[[1L]]) %in% c(
        "SYMBOL", "NUM_CONST", "STR_CONST", "NULL_CONST"
    )
}

#' Create an extract-variable code action
#' @noRd
refactor_extract_variable_action <- function(uri, workspace, document,
    context, client_capabilities = NULL) {
    node <- context$expression
    if (is.null(node) || refactor_atomic_expression(node) ||
        refactor_unsafe_syntax(node) ||
        any(refactor_token_names(node) %in% c(
            "LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN", "FUNCTION"
        ))) {
        return(NULL)
    }
    statement_context <- refactor_statement_context(node)
    if (is.null(statement_context)) {
        return(NULL)
    }
    if (!refactor_expression_movable(node, statement_context$statement) ||
            refactor_unsafe_syntax(statement_context$statement)) {
        return(NULL)
    }
    insertion <- refactor_insertion_context(
        document, statement_context$statement
    )
    if (is.null(insertion)) {
        return(NULL)
    }
    if (identical(xml_name(statement_context$container), "exprlist") &&
            !refactor_index_ready(workspace)) {
        return(NULL)
    }

    name <- refactor_unique_name("extracted_value", workspace, uri)
    selected_text <- refactor_range_text(document, context$range)
    continuation <- paste0(
        insertion$indent,
        strrep(" ", nchar(name) + nchar(" <- "))
    )
    lines <- strsplit(selected_text, "\n", fixed = TRUE)[[1L]]
    if (length(lines) > 1L) {
        protected <- refactor_protected_quoted_lines(context)
        tail_text <- paste(lines[-1L], collapse = "\n")
        tail_text <- refactor_reindent_text(
            tail_text, context$range$start$col, continuation,
            trim_first = TRUE, protected_lines = protected[protected > 1L] - 1L
        )
        selected_text <- paste(lines[[1L]], tail_text, sep = "\n")
    }
    insertion_text <- paste0(
        insertion$indent, name, " <- ", selected_text, "\n"
    )
    # Snippet clients can normalize indentation, including the unchanged
    # source enclosed by the linked edit. Ordinary edits avoid that adjustment.
    snippet <- code_action_snippet_support(client_capabilities) &&
        refactor_snippet_preserves_quoted_text(context, insertion$point)
    replacement <- name
    if (snippet) {
        insertion_text <- paste0(
            escape_completion_snippet(insertion$indent),
            "${1:", escape_completion_snippet(name), "}",
            escape_completion_snippet(paste0(" <- ", selected_text, "\n"))
        )
        replacement <- "$1$0"
    }
    edits <- refactor_insert_and_replace(
        document,
        insertion$point,
        insertion_text,
        context$range,
        replacement,
        snippet = snippet
    )
    list(
        title = "Extract expression to variable",
        kind = CodeActionKind$RefactorExtract,
        edit = code_action_workspace_edit(
            uri, edits, document, client_capabilities
        )
    )
}

#' Return indexed occurrence ranges as internal code-point ranges
#' @noRd
refactor_occurrence_range <- function(index, i) {
    list(
        start = list(
            row = index$line[[i]], col = index$code_point_col[[i]]
        ),
        end = list(
            row = index$end_line[[i]], col = index$code_point_end_col[[i]]
        )
    )
}

#' Test whether a selection contains a binding definition
#' @noRd
refactor_selection_defines_binding <- function(context) {
    index <- context$parse_data$reference_index
    if (is.null(index) || !length(index$name)) return(FALSE)
    any(vapply(which(index$is_definition), function(i) {
        refactor_range_contains(
            context$range, refactor_occurrence_range(index, i))
    }, logical(1L)))
}

#' Test whether a selected definition is available to a selected read
#' @noRd
refactor_definition_precedes_read <- function(context, index, definition, read) {
    definition_range <- refactor_occurrence_range(index, definition)
    read_range <- refactor_occurrence_range(index, read)
    if (refactor_compare_point(
        definition_range$end, read_range$start) > 0L) return(FALSE)

    token <- xdoc_find_token(
        context$xdoc,
        definition_range$start$row + 1L,
        definition_range$start$col + 1L
    )
    if (inherits(token, "xml_missing")) return(FALSE)
    assignment <- refactor_definition_assignment(token)
    if (!is.null(assignment) && refactor_range_contains(
        refactor_node_range(assignment$node), read_range)) return(FALSE)
    TRUE
}

#' Analyze free variables and live-out bindings for function extraction
#' @noRd
refactor_function_bindings <- function(context) {
    index <- context$parse_data$reference_index
    if (is.null(index) || !length(index$name)) {
        return(list(free = character(), outputs = character()))
    }
    inside <- vapply(seq_along(index$name), function(i) {
        refactor_range_contains(
            context$range, refactor_occurrence_range(index, i)
        )
    }, logical(1L))
    selected <- which(inside)
    selected_definitions <- unique(index$definition_key[
        selected[index$is_definition[selected]]
    ])
    reads <- selected[!index$is_definition[selected]]
    free <- reads[vapply(reads, function(read) {
        definition_key <- index$definition_key[[read]]
        if (!startsWith(definition_key, "local:")) return(FALSE)
        definitions <- selected[
            index$is_definition[selected] &
                index$definition_key[selected] == definition_key
        ]
        !length(definitions) || !any(vapply(definitions, function(definition) {
            refactor_definition_precedes_read(
                context, index, definition, read)
        }, logical(1L)))
    }, logical(1L))]
    free_names <- unique(index$name[free])

    output_indices <- integer()
    for (definition_key in selected_definitions) {
        definitions <- selected[
            index$is_definition[selected] &
                index$definition_key[selected] == definition_key
        ]
        if (!length(definitions)) next
        later_reads <- which(
            !index$is_definition &
                index$definition_key == definition_key &
                vapply(seq_along(index$name), function(i) {
                    refactor_compare_point(
                        refactor_occurrence_range(index, i)$start,
                        context$range$end
                    ) >= 0L
                }, logical(1L))
        )
        if (length(later_reads)) {
            output_indices <- c(
                output_indices, definitions[[length(definitions)]]
            )
        }
    }
    list(
        free = free_names,
        outputs = unique(index$name[output_indices])
    )
}

#' Create an extract-function code action
#' @noRd
refactor_extract_function_action <- function(uri, workspace, document,
    context, client_capabilities = NULL) {
    selection <- context$statements
    if (is.null(selection)) {
        node <- context$expression
        if (is.null(node)) {
            return(NULL)
        }
        statement_context <- refactor_statement_context(node)
        if (is.null(statement_context)) {
            return(NULL)
        }
        if (refactor_atomic_expression(node) ||
            !refactor_expression_movable(
                node, statement_context$statement
            ) ||
            refactor_unsafe_syntax(
                statement_context$statement,
                extracting_function = TRUE
            )) {
            return(NULL)
        }
        selection <- list(
            nodes = list(node),
            container = statement_context$container,
            range = context$range
        )
        statement <- statement_context$statement
    } else {
        statement <- selection$nodes[[1L]]
    }
    unsafe <- vapply(
        selection$nodes,
        refactor_unsafe_syntax,
        logical(1L),
        extracting_function = TRUE
    )
    if (any(unsafe)) {
        return(NULL)
    }
    uses_dots <- vapply(selection$nodes, function(node) {
        "..." %in% xml_text(xml_find_all(
            node,
            ".//*[self::SYMBOL or self::SYMBOL_FORMALS]"
        ))
    }, logical(1L))
    if (any(uses_dots)) {
        return(NULL)
    }

    insertion <- refactor_insertion_context(document, statement)
    if (is.null(insertion)) {
        return(NULL)
    }
    if (identical(xml_name(selection$container), "exprlist") &&
            !refactor_index_ready(workspace)) {
        return(NULL)
    }
    if (identical(xml_name(selection$container), "exprlist") &&
            refactor_selection_defines_binding(context)) return(NULL)

    bindings <- refactor_function_bindings(context)
    if (length(bindings$outputs) > 1L) {
        return(NULL)
    }
    if (length(bindings$outputs)) {
        final_assignment <- refactor_assignment_parts(
            selection$nodes[[length(selection$nodes)]]
        )
        if (is.null(final_assignment) ||
                !identical(final_assignment$name, bindings$outputs[[1L]])) {
            return(NULL)
        }
    }

    name <- refactor_unique_name("extracted_function", workspace, uri)
    if (name %in% bindings$free) {
        return(NULL)
    }
    arguments <- paste(bindings$free, collapse = ", ")
    selected_text <- refactor_range_text(document, context$range)
    body_indent <- paste0(insertion$indent, "  ")
    body <- refactor_reindent_text(
        selected_text, context$range$start$col, body_indent,
        protected_lines = refactor_protected_quoted_lines(context)
    )
    insertion_text <- paste0(
        insertion$indent, name, " <- function(", arguments, ") {\n",
        body, "\n", insertion$indent, "}\n"
    )
    call <- paste0(name, "(", arguments, ")")
    replacement <- if (length(bindings$outputs)) {
        paste0(bindings$outputs[[1L]], " <- ", call)
    } else {
        call
    }
    snippet <- code_action_snippet_support(client_capabilities) &&
        refactor_snippet_preserves_quoted_text(context, insertion$point)
    if (snippet) {
        insertion_text <- paste0(
            escape_completion_snippet(insertion$indent),
            "${1:", escape_completion_snippet(name), "}",
            escape_completion_snippet(paste0(
                " <- function(", arguments, ") {\n", body, "\n", insertion$indent, "}\n"
            ))
        )
        replacement <- paste0(
            if (length(bindings$outputs)) {
                escape_completion_snippet(paste0(bindings$outputs[[1L]], " <- "))
            } else {
                ""
            },
            "$1", escape_completion_snippet(paste0("(", arguments, ")")), "$0"
        )
    }
    edits <- refactor_insert_and_replace(
        document,
        insertion$point,
        insertion_text,
        context$range,
        replacement,
        snippet = snippet
    )
    list(
        title = "Extract selection to function",
        kind = CodeActionKind$RefactorExtract,
        edit = code_action_workspace_edit(
            uri, edits, document, client_capabilities
        )
    )
}

#' Find an indexed symbol occurrence at a requested range
#' @noRd
refactor_occurrence_at_range <- function(index, item_range) {
    if (is.null(index) || !length(index$name)) {
        return(NULL)
    }
    point <- item_range$start
    candidates <- which(vapply(seq_along(index$name), function(i) {
        occurrence <- refactor_occurrence_range(index, i)
        refactor_compare_point(occurrence$start, point) <= 0L &&
            refactor_compare_point(occurrence$end, point) >= 0L
    }, logical(1L)))
    if (!length(candidates)) {
        return(NULL)
    }
    candidates[[1L]]
}

#' Compute the full-line range used to remove a declaration
#' @noRd
refactor_declaration_removal_range <- function(document, assignment_range) {
    start_row <- assignment_range$start$row
    end_row <- assignment_range$end$row
    prefix <- substr(
        document$line0(start_row), 1L, assignment_range$start$col
    )
    suffix <- substr(
        document$line0(end_row), assignment_range$end$col + 1L,
        nchar(document$line0(end_row))
    )
    if (!grepl("^[ \\t]*$", prefix) || !grepl("^[ \\t]*$", suffix)) {
        return(NULL)
    }
    end <- if (end_row + 1L < document$nline) {
        list(row = end_row + 1L, col = 0L)
    } else {
        list(row = end_row, col = nchar(document$line0(end_row)))
    }
    list(start = list(row = start_row, col = 0L), end = end)
}

#' Create an inline-local-variable code action
#' @noRd
refactor_inline_variable_action <- function(uri, workspace, document,
    item_range, client_capabilities = NULL) {
    if (is.null(workspace) || !check_r_region(document, item_range$start)) {
        return(NULL)
    }
    parse_data <- current_parse_data(uri, workspace, document)
    if (is.null(parse_data) || isTRUE(parse_data$parse_error) ||
            is.null(parse_data_xml(parse_data))) {
        return(NULL)
    }
    index <- parse_data$reference_index
    selected <- refactor_occurrence_at_range(index, item_range)
    if (is.null(selected)) {
        return(NULL)
    }
    definition_key <- index$definition_key[[selected]]
    if (!startsWith(definition_key, paste0("local:", uri, ":"))) {
        return(NULL)
    }
    same_binding <- which(index$definition_key == definition_key)
    definitions <- same_binding[index$is_definition[same_binding]]
    reads <- same_binding[!index$is_definition[same_binding]]
    if (length(definitions) != 1L || length(reads) != 1L ||
            !identical(index$definition_kind[[definitions]], "variable")) {
        return(NULL)
    }
    definition_range <- refactor_occurrence_range(index, definitions)
    read_range <- refactor_occurrence_range(index, reads)
    if (refactor_compare_point(read_range$start, definition_range$end) <= 0L) {
        return(NULL)
    }

    token_node <- xdoc_find_token(
        parse_data_xml(parse_data),
        definition_range$start$row + 1L,
        definition_range$start$col + 1L
    )
    if (inherits(token_node, "xml_missing")) {
        return(NULL)
    }
    assignment <- refactor_definition_assignment(token_node)
    if (is.null(assignment)) {
        return(NULL)
    }
    parent <- xml_parent(assignment$node)
    if (!refactor_sequence_container(parent) ||
            refactor_unsafe_syntax(assignment$value)) {
        return(NULL)
    }

    removal_range <- refactor_declaration_removal_range(
        document, refactor_node_range(assignment$node)
    )
    if (is.null(removal_range)) {
        return(NULL)
    }
    read_node <- xdoc_find_token(
        parse_data_xml(parse_data),
        read_range$start$row + 1L,
        read_range$start$col + 1L
    )
    if (inherits(read_node, "xml_missing")) {
        return(NULL)
    }
    read_statement <- refactor_statement_context(xml_parent(read_node))
    if (is.null(read_statement) ||
            refactor_unsafe_syntax(read_statement$statement)) {
        return(NULL)
    }

    value_text <- refactor_range_text(
        document, refactor_node_range(assignment$value)
    )
    edits <- list(
        text_edit(refactor_lsp_range(document, removal_range), ""),
        text_edit(
            refactor_lsp_range(document, read_range),
            paste0("(", value_text, ")")
        )
    )
    list(
        title = sprintf("Inline local variable `%s`", index$name[[selected]]),
        kind = CodeActionKind$RefactorInline,
        edit = code_action_workspace_edit(
            uri, edits, document, client_capabilities
        )
    )
}

#' Return all refactoring actions for a code-action request
#' @noRd
refactor_code_actions <- function(uri, workspace, document, item_range, only,
    client_capabilities = NULL) {
    result <- list()
    if (code_action_kind_requested(CodeActionKind$RefactorExtract, only)) {
        context <- refactor_selection_context(
            uri, workspace, document, item_range
        )
        if (!is.null(context)) {
            variable <- refactor_extract_variable_action(
                uri, workspace, document, context, client_capabilities
            )
            funct <- refactor_extract_function_action(
                uri, workspace, document, context, client_capabilities
            )
            if (!is.null(variable)) result[[length(result) + 1L]] <- variable
            if (!is.null(funct)) result[[length(result) + 1L]] <- funct
        }
    }
    if (code_action_kind_requested(CodeActionKind$RefactorInline, only)) {
        inline <- refactor_inline_variable_action(
            uri, workspace, document, item_range, client_capabilities
        )
        if (!is.null(inline)) result[[length(result) + 1L]] <- inline
    }
    result
}
