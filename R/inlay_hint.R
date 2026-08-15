#' Split the direct children of a call expression into argument groups
#' @noRd
call_argument_groups <- function(call_node) {
    children <- xml_children(call_node)
    child_names <- xml_name(children)
    open_indices <- which(child_names == "OP-LEFT-PAREN")
    close_indices <- which(child_names == "OP-RIGHT-PAREN")
    if (!length(open_indices) || !length(close_indices)) return(list())
    open <- open_indices[[1L]]
    close <- max(close_indices)
    if (close <= open) return(list())

    indices <- if (close > open + 1L) {
        seq.int(open + 1L, close - 1L)
    } else {
        integer()
    }
    argument_nodes <- children[indices]
    argument_names <- xml_name(argument_nodes)
    comma <- which(argument_names == "OP-COMMA")
    boundaries <- c(0L, comma, length(argument_nodes) + 1L)

    groups <- vector("list", length(boundaries) - 1L)
    for (i in seq_along(groups)) {
        first <- boundaries[[i]] + 1L
        last <- boundaries[[i + 1L]] - 1L
        nodes <- if (first <= last) argument_nodes[seq.int(first, last)] else NULL
        names <- if (length(nodes)) xml_name(nodes) else character()
        subscript <- which(
            names == "SYMBOL_SUB" &
                c(names[-1L], "") == "EQ_SUB"
        )
        groups[[i]] <- list(
            nodes = nodes,
            name = if (length(subscript)) xml_text(nodes[[subscript[[1L]]]]) else NULL
        )
    }
    groups
}

#' Match an R named argument using exact and unique partial matching
#' @noRd
match_named_formal <- function(name, formal_names) {
    exact <- match(name, formal_names)
    if (!is.na(exact)) return(exact)
    partial <- which(startsWith(formal_names, name))
    if (length(partial) == 1L) partial else NA_integer_
}

#' Extract parameter-name inlay hints for calls in a requested range
#' @noRd
inlay_hint_reply <- function(id, uri, workspace, document, request_range) {
    parse_data <- current_parse_data(uri, workspace, document)
    if (is.null(parse_data)) return(NULL)
    xdoc <- parse_data$xml_doc
    if (is.null(xdoc)) return(Response$new(id, result = list()))

    start_line <- request_range$start$line + 1L
    end_line <- request_range$end$line + 1L
    if (request_range$end$character == 0L && end_line > start_line) {
        end_line <- end_line - 1L
    }
    calls <- xml_find_all(
        xdoc,
        paste0(
            "//expr[@line2 >= ", start_line,
            " and @line1 <= ", end_line,
            " and expr[1][following-sibling::*[1]",
            "[self::OP-LEFT-PAREN]] and ",
            "expr[1]//SYMBOL_FUNCTION_CALL]"
        )
    )
    if (!length(calls)) return(Response$new(id, result = list()))

    minimum_arguments <- lsp_settings$get("inlay_hints_minimum_arguments")
    if (!is.numeric(minimum_arguments) || length(minimum_arguments) != 1L ||
        is.na(minimum_arguments) || minimum_arguments < 0L) {
        minimum_arguments <- 2L
    }
    minimum_argument_length <- lsp_settings$get(
        "inlay_hints_minimum_argument_length"
    )
    if (!is.numeric(minimum_argument_length) ||
        length(minimum_argument_length) != 1L ||
        is.na(minimum_argument_length) || minimum_argument_length < 0L) {
        minimum_argument_length <- 2L
    }

    formals_cache <- new.env(parent = emptyenv())
    hints <- list()
    for (call_node in calls) {
        callee <- xml_find_first(call_node, "expr[1]")
        member_operator <- xml_find_first(
            callee,
            ".//*[self::OP-DOLLAR or self::OP-AT]"
        )
        if (!inherits(member_operator, "xml_missing")) next

        function_node <- xml_find_first(callee, ".//SYMBOL_FUNCTION_CALL[1]")
        if (inherits(function_node, "xml_missing")) next

        function_name <- xml_text(function_node)
        package_node <- xml_find_first(callee, ".//SYMBOL_PACKAGE[1]")
        package <- if (inherits(package_node, "xml_missing")) NULL else xml_text(package_node)
        cache_key <- paste(if (is.null(package)) "" else package, function_name, sep = "::")
        if (exists(cache_key, envir = formals_cache, inherits = FALSE)) {
            function_formals <- get(cache_key, envir = formals_cache, inherits = FALSE)
        } else {
            function_formals <- tryCatch(call_with_optional_uri(
                workspace$get_formals, function_name, package, uri = uri),
                error = function(e) NULL)
            assign(cache_key, function_formals, envir = formals_cache)
        }

        formal_names <- names(function_formals)
        if (!length(formal_names)) next
        groups <- call_argument_groups(call_node)
        if (!length(groups)) next

        supplied_arguments <- sum(vapply(
            groups, function(group) length(group$nodes) > 0L, logical(1L)))
        if (supplied_arguments < minimum_arguments) next

        named_formals <- vapply(groups, function(group) {
            if (is.null(group$name)) return(NA_integer_)
            match_named_formal(group$name, formal_names)
        }, integer(1L))
        unavailable <- unique(named_formals[!is.na(named_formals)])
        available <- setdiff(seq_along(formal_names), unavailable)
        next_available <- 1L

        for (group in groups) {
            if (!is.null(group$name)) next
            if (next_available > length(available)) break

            formal_index <- available[[next_available]]
            next_available <- next_available + 1L
            formal_name <- formal_names[[formal_index]]
            if (identical(formal_name, "...")) break
            if (!length(group$nodes)) next
            argument_name_for_length <- sub("^\\.", "", formal_name)
            if (nchar(argument_name_for_length) < minimum_argument_length) next

            first_node <- group$nodes[[1L]]
            row <- as.integer(xml_attr(first_node, "line1")) - 1L
            col <- as.integer(xml_attr(first_node, "col1")) - 1L
            if (!position_is_in_lsp_range(document, row, col, request_range)) next

            argument_text <- trimws(paste(xml_text(group$nodes), collapse = ""))
            if (identical(argument_text, formal_name)) next

            hints[[length(hints) + 1L]] <- list(
                position = document$to_lsp_position(row, col),
                label = paste0(formal_name, " ="),
                kind = 2L,
                paddingRight = TRUE,
                data = list(
                    uri = uri,
                    functionName = function_name,
                    package = package,
                    parameter = formal_name
                )
            )
            if (length(hints) >= 200L) break
        }
        if (length(hints) >= 200L) break
    }

    Response$new(id, result = hints)
}

#' Resolve explanatory text for an inlay hint
#' @noRd
inlay_hint_resolve_reply <- function(id, workspace, hint) {
    function_name <- hint$data$functionName
    parameter <- hint$data$parameter
    package <- hint$data$package
    if (is.null(function_name) || is.null(parameter)) {
        return(Response$new(id, result = hint))
    }

    qualified_name <- if (is.null(package) || !nzchar(package)) {
        function_name
    } else {
        paste0(package, "::", function_name)
    }
    contents <- function_argument_hover_contents(
        workspace, function_name, package, parameter, uri = hint$data$uri)
    if (is.null(contents)) {
        contents <- sprintf(
            "Parameter `%s` of `%s()`.", parameter, qualified_name)
    }
    hint$tooltip <- list(
        kind = "markdown",
        value = paste(contents, collapse = "\n\n")
    )
    Response$new(id, result = hint)
}
