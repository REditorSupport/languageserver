#' Split the direct children of a call expression into argument groups
#' @noRd
call_argument_groups <- function(call_node) {
    children <- xml_children(call_node)
    child_names <- xml_name(children)
    keep <- child_names != "COMMENT"
    children <- children[keep]
    child_names <- child_names[keep]
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

#' Whether a pipe supplies an implicit positional argument to this call
#' @noRd
call_has_implicit_argument <- function(call_node) {
    operator <- xml_find_first_(call_node, "preceding-sibling::*[not(self::COMMENT)][1]")
    if (inherits(operator, "xml_missing")) return(FALSE)
    if (xml_name(operator) %in% c("PIPE", "OP-PIPE")) {
        return(xml_find_lgl_(call_node, "not(expr[count(*) = 1]/PLACEHOLDER)"))
    }
    if (xml_name(operator) == "SPECIAL" && xml_text(operator) %in% c("%>%", "%<>%", "%T>%")) {
        return(xml_find_lgl_(call_node, "not(expr[count(*) = 1]/SYMBOL[text() = '.'])"))
    }
    FALSE
}

#' Match an R named argument using exact and unique partial matching
#' @noRd
inlay_positional_formals <- function(formal_names, tags) {
    available <- seq_along(formal_names)
    tags <- tags[!is.na(tags) & nzchar(tags)]
    if (!length(tags)) return(available)
    tags <- reference_symbol_names(tags)
    # R reserves all exact names before attempting any partial names.
    # Ignore unknown/extra arguments while preserving established positional
    # hints in incomplete calls, and never partially match past real dots.
    keys <- union(formal_names, "...")
    matched <- s3_match_arguments(stats::setNames(seq_along(keys), keys), stats::setNames(seq_along(tags), tags))
    if (is.null(matched)) return(integer())
    setdiff(available, matched[keys[matched] != "..."])
}

#' Move untouched calls with an incremental edit, discarding intersecting calls
#' @noRd
inlay_hint_last_call_end <- function(calls) {
    if (!length(calls$name)) return(NULL)
    line <- max(calls$end_line)
    c(line, max(calls$end_col[calls$end_line == line]))
}

#' @noRd
inlay_hint_apply_change <- function(indexed, change, content) {
    if (is.null(indexed) || is.null(change$range)) return(NULL)
    calls <- indexed$calls
    if (!length(calls$name)) return(indexed)
    start <- change$range$start
    end <- change$range$end
    last <- indexed$last_call_end
    if (!is.null(last) && (start$line > last[[1L]] ||
                start$line == last[[1L]] && start$character > last[[2L]])) {
        return(indexed)
    }
    before <- calls$end_line < start$line |
        calls$end_line == start$line & calls$end_col <= start$character
    # A pipe on a preceding line is part of the argument-matching context.
    after <- calls$context_line > end$line |
        calls$context_line == end$line & calls$context_col >= end$character
    line_text <- function(row) if (row < length(content)) content[[row + 1L]] else ""
    # Inserts, replacements and deletions ending at a callee can change its
    # name or turn it into a member call, even with spaces or tabs in between.
    # Only retain whitespace replacements here; an empty replacement can
    # join the callee to preceding text.
    if (!nzchar(change$text) || grepl("[^[:space:]]", change$text)) {
        end_text <- line_text(end$line)
        end_col <- code_point_from_unit(end_text, end$character)
        following <- substring(end_text, end_col + 1L)
        first_token <- regexpr("[^[:space:]]", following)[[1L]]
        boundary <- if (first_token > 0L) {
            code_point_to_unit(end_text, end_col + first_token - 1L)
        } else {
            end$character
        }
        at_start <- calls$context_line == end$line & calls$context_col == boundary
        after[at_start] <- FALSE
        # Completing a pipe before a previously ordinary multiline call also
        # changes its matching, even if another incomplete edit prevents parsing.
        prefix <- substr(line_text(start$line), 1L,
            code_point_from_unit(line_text(start$line), start$character))
        if (grepl("(\\|>|%>%|%<>%|%T>%)[[:space:]]*(#[^\n]*)?$", paste0(prefix, change$text))) {
            boundary_line <- end$line
            repeat {
                code <- sub("#.*$", "", following)
                first_token <- regexpr("[^[:space:]]", code)[[1L]]
                if (first_token > 0L || boundary_line + 1L >= length(content)) break
                boundary_line <- boundary_line + 1L
                following <- line_text(boundary_line)
            }
            if (first_token > 0L) {
                boundary_col <- if (boundary_line == end$line) boundary else
                    code_point_to_unit(following, first_token - 1L)
                after[calls$line == boundary_line & calls$col == boundary_col] <- FALSE
            }
        }
    }
    # An edit outside a call can still put it in a comment, string or
    # backtick name. Drop following calls when lexical context may change.
    removed <- get_range_text(
        content, start$line + 1L,
        code_point_from_unit(line_text(start$line), start$character) + 1L,
        end$line + 1L,
        code_point_from_unit(line_text(end$line), end$character)
    )
    if (any(grepl(r"([#"'`\\])", c(change$text, removed), useBytes = TRUE), na.rm = TRUE)) {
        after[] <- FALSE
    }
    keep <- before | after
    if (!any(keep)) return(NULL)

    replacement <- split_text_lines(change$text)
    new_end_line <- start$line + length(replacement) - 1L
    new_end_col <- code_point_to_unit(utils::tail(replacement, 1L), Inf)
    if (length(replacement) == 1L) new_end_col <- new_end_col + start$character
    shift <- function(lines, cols, move) {
        same_line <- move & !is.na(lines) & lines == end$line
        cols[same_line] <- cols[same_line] + new_end_col - end$character
        lines[move] <- lines[move] + new_end_line - end$line
        list(line = lines, col = cols)
    }
    positions <- list(c("line", "col"), c("end_line", "end_col"), c("context_line", "context_col"))
    if (!is.null(calls$opening_line)) positions <- c(positions, list(c("opening_line", "opening_col")))
    for (fields in positions) {
        moved <- shift(calls[[fields[[1L]]]], calls[[fields[[2L]]]], after)
        calls[[fields[[1L]]]] <- moved$line
        calls[[fields[[2L]]]] <- moved$col
    }
    arguments <- indexed$arguments
    moved <- shift(arguments$line, arguments$col, after[arguments$call])
    arguments$line <- moved$line
    arguments$col <- moved$col

    kept_arguments <- which(keep[arguments$call])
    arguments <- lapply(arguments, `[`, kept_arguments)
    arguments$call <- match(arguments$call, which(keep))
    calls <- lapply(calls, `[`, which(keep))
    calls$first_argument <- match(calls$first_argument, kept_arguments)
    calls$last_argument <- match(calls$last_argument, kept_arguments)
    indexed$calls <- calls
    indexed$arguments <- arguments
    indexed$argument_order <- which(arguments$present)
    indexed$argument_order <- indexed$argument_order[order(
        arguments$line[indexed$argument_order], arguments$col[indexed$argument_order]
    )]
    indexed$argument_lines <- arguments$line[indexed$argument_order]
    indexed$last_call_end <- inlay_hint_last_call_end(calls)
    indexed
}

#' Extract parameter-name inlay hints for calls in a requested range
#' @noRd
inlay_hint_reply <- function(id, uri, workspace, document, request_range) {
    parse_data <- current_parse_data(uri, workspace, document)
    if (is.null(parse_data)) return(NULL)
    incomplete <- isTRUE(parse_data$parse_error) || isTRUE(parse_data$inlay_hint_incomplete)
    if (!is.null(parse_data$range_data)) {
        current <- indexed_inlay_hint_reply(
            id, uri, workspace, parse_data$range_data, request_range,
            document$inlay_hint_formals, use_previous_formals = incomplete, document = document
        )
        if (!incomplete || is.null(document$inlay_hint_data) ||
                length(current$result) >= 200L) return(current)
        # Rebased calls fill only gaps in the current partial parse. A recovered
        # call can legitimately have fewer hints after its declaration changes.
        previous <- indexed_inlay_hint_reply(
            id, uri, workspace, document$inlay_hint_data, request_range,
            document$inlay_hint_formals, use_previous_formals = TRUE,
            regions = document$regions, fallback_lines = parse_data$inlay_hint_incomplete_lines,
            recovered = parse_data$range_data, document = document
        )
        return(Response$new(id, result = utils::head(c(current$result, previous$result), 200L)))
    }

    xdoc <- parse_data_xml(parse_data)
    if (is.null(xdoc)) return(Response$new(id, result = list()))
    if (!is.null(parse_data$syntax_data)) {
        syntax <- parse_data$syntax_data
        # Older snapshots can omit the viewport index. The compact tree
        # stores parent rows; the native index builder expects parser IDs.
        data <- list2DF(syntax[c("id", "token", "text", "terminal", "line1", "col1", "line2", "col2")])
        data$parent <- c(0L, syntax$id)[syntax$parent_row + 1L]
        indexed <- range_provider_parse_data(data, document$content)
        return(indexed_inlay_hint_reply(id, uri, workspace, indexed, request_range, document = document))
    }

    start_line <- request_range$start$line + 1L
    end_line <- request_range$end$line + 1L
    if (request_range$end$character == 0L && end_line > start_line) {
        end_line <- end_line - 1L
    }
    fn_nodes <- xml_find_all_(
        xdoc,
        paste0(
            "//SYMBOL_FUNCTION_CALL[parent::expr[",
            "not(preceding-sibling::expr or ./OP-DOLLAR or ./OP-AT)",
            " and following-sibling::*[not(self::COMMENT)][1][self::OP-LEFT-PAREN]",
            " and parent::expr[@line2 >= ", start_line, " and @line1 <= ", end_line, "]]]"
        )
    )
    if (!length(fn_nodes)) return(Response$new(id, result = list()))
    calls <- xml_parent(xml_parent(fn_nodes))
    function_names <- xml_text(fn_nodes)
    packages <- xml_find_chr_(fn_nodes, "string(preceding-sibling::SYMBOL_PACKAGE[1])")

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
    for (i in seq_along(calls)) {
        call_node <- calls[[i]]
        function_name <- function_names[[i]]
        if (!nzchar(function_name)) next

        package <- packages[[i]]
        if (!nzchar(package)) package <- NULL
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

        implicit_argument <- call_has_implicit_argument(call_node)
        supplied_arguments <- sum(vapply(
            groups, function(group) length(group$nodes) > 0L, logical(1L))) + implicit_argument
        if (supplied_arguments < minimum_arguments) next

        group_names <- vapply(groups, function(group) group$name %s3_or% NA_character_, "")
        available <- inlay_positional_formals(formal_names, group_names)
        if (implicit_argument) {
            if (!length(available) || formal_names[[available[[1L]]]] == "...") next
            available <- available[-1L]
        }
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

#' Resolve hints only for calls with an argument visible in the viewport
#' @noRd
indexed_inlay_hint_reply <- function(id, uri, workspace, indexed, request_range,
    previous_formals = NULL, use_previous_formals = FALSE, regions = NULL,
    fallback_lines = NULL, recovered = NULL, document = NULL) {
    arguments <- indexed$arguments
    visible <- indexed$argument_order[range_line_indices(
        indexed$argument_lines,
        request_range$start$line, request_range$end$line
    )]
    visible <- visible[range_position_selected(
        arguments$line[visible], arguments$col[visible], request_range
    )]
    if (!is.null(regions)) {
        visible <- visible[regions$line_type[arguments$line[visible] + 1L] == "r"]
    }
    if (!is.null(fallback_lines)) {
        visible <- visible[arguments$line[visible] %in% fallback_lines]
    }
    if (length(visible) && !is.null(recovered)) {
        rows <- range_line_indices(recovered$argument_lines,
            request_range$start$line, request_range$end$line)
        covered <- unique(recovered$arguments$call[recovered$argument_order[rows]])
        call_keys <- function(calls, i) paste(calls$line[i], calls$col[i], sep = ":")
        covered_keys <- call_keys(recovered$calls, covered)
        visible <- visible[!call_keys(indexed$calls, arguments$call[visible]) %in% covered_keys]
    }
    if (!length(visible)) return(Response$new(id, result = list()))
    calls <- indexed$calls
    if (is.null(document) && !is.null(workspace$documents)) document <- workspace$documents$get(uri, NULL)
    parsed <- if (!is.null(document)) document$parse_data
    member_enabled <- !is.null(document) && isTRUE(lsp_settings$get("member_completion"))
    candidates <- NULL
    use_s3 <- !is.null(document) && isTRUE(lsp_settings$get("s3_dispatch"))
    call_indices <- sort(unique(arguments$call[visible]))
    named_bind <- member_enabled && any(calls$named_bind[call_indices]) &&
        inlay_s7_bind_possible(workspace, document)
    visible_by_call <- list2env(split(visible, arguments$call[visible]),
        parent = emptyenv())

    minimum_arguments <- lsp_settings$get("inlay_hints_minimum_arguments")
    if (!is.numeric(minimum_arguments) || length(minimum_arguments) != 1L ||
            is.na(minimum_arguments) || minimum_arguments < 0L) {
        minimum_arguments <- 2L
    }
    minimum_argument_length <- lsp_settings$get("inlay_hints_minimum_argument_length")
    if (!is.numeric(minimum_argument_length) || length(minimum_argument_length) != 1L ||
            is.na(minimum_argument_length) || minimum_argument_length < 0L) {
        minimum_argument_length <- 2L
    }
    formals_cache <- new.env(parent = emptyenv())
    hints <- vector("list", 200L)
    hint_count <- 0L
    for (call_index in call_indices) {
        groups <- seq.int(calls$first_argument[[call_index]], calls$last_argument[[call_index]])
        implicit_argument <- isTRUE(calls$implicit_argument[[call_index]])
        if (sum(arguments$present[groups]) + implicit_argument < minimum_arguments) next
        function_name <- calls$name[[call_index]]
        package <- calls$package[[call_index]]
        if (is.na(package)) package <- NULL
        member <- isTRUE(calls$member[call_index])
        generic <- use_s3 && (!is.null(parsed$member_data$s3$functions[[function_name]]) ||
                (is.null(package) && function_name %in% parsed$member_data$s3$aliases) ||
                (is.function(workspace$s3_metadata$may_dispatch) && workspace$s3_metadata$may_dispatch(function_name)))
        context <- NULL
        if (member && !member_enabled) next
        symbol <- FALSE
        deferred <- FALSE
        bound <- named_bind && isTRUE(calls$named_bind[call_index])
        if (!member && member_enabled) {
            if (is.null(candidates)) candidates <- inlay_symbol_candidates(workspace, document)
            candidate <- candidates(function_name, package, calls$line[[call_index]], bound, ordinary = !generic)
            symbol <- isTRUE(candidate)
            deferred <- is.na(candidate) && !is.null(calls$opening_line)
        }
        if ((member || symbol || generic || deferred) && !is.null(calls$opening_line)) {
            opening <- position(calls$opening_line[[call_index]], calls$opening_col[[call_index]])
            if (!deferred) context <- inlay_call_context(workspace, document, opening, member, symbol, generic)
            if (member && is.null(context$formals)) next
        }
        cache_key <- paste(if (is.null(package)) "" else package, function_name, sep = "::")
        if (!is.null(context)) {
            formal_names <- names(context$formals)
        } else if (exists(cache_key, envir = formals_cache, inherits = FALSE)) {
            formal_names <- get(cache_key, envir = formals_cache, inherits = FALSE)
        } else {
            function_formals <- tryCatch(call_with_optional_uri(
                workspace$get_formals, function_name, package, uri = uri),
            error = function(e) NULL)
            formal_names <- names(function_formals)
            if (!is.null(previous_formals)) {
                if (!use_previous_formals) {
                    assign(cache_key, formal_names, envir = previous_formals)
                } else if (is.null(formal_names)) {
                    local <- if (is.null(package)) workspace$get_parse_data(uri)$functions[[function_name]]
                    formal_names <- if (is.function(local)) names(formals(local)) else
                        get0(cache_key, envir = previous_formals, inherits = FALSE)
                }
            }
            assign(cache_key, formal_names, envir = formals_cache)
        }
        if (!length(formal_names)) next
        group_names <- arguments$name[groups]
        named <- !is.na(group_names)
        available <- inlay_positional_formals(formal_names, group_names)
        if (implicit_argument) {
            if (!length(available) || formal_names[[available[[1L]]]] == "...") next
            available <- available[-1L]
        }
        unnamed <- groups[!named]
        n <- min(length(unnamed), length(available))
        if (!n) next
        unnamed <- unnamed[seq_len(n)]
        matched_names <- formal_names[available[seq_len(n)]]
        dots <- match("...", matched_names)
        if (!is.na(dots)) {
            unnamed <- unnamed[seq_len(dots - 1L)]
            matched_names <- matched_names[seq_len(dots - 1L)]
        }
        visible_arguments <- get(as.character(call_index), envir = visible_by_call,
            inherits = FALSE)
        keep <- arguments$present[unnamed] & unnamed %in% visible_arguments &
            nchar(sub("^\\.", "", matched_names)) >= minimum_argument_length &
            (is.na(arguments$symbol[unnamed]) | arguments$symbol[unnamed] != matched_names)
        unnamed <- unnamed[keep]
        matched_names <- matched_names[keep]
        for (j in seq_along(unnamed)) {
            argument <- unnamed[[j]]
            formal_name <- matched_names[[j]]
            hint_count <- hint_count + 1L
            hints[[hint_count]] <- list(
                position = position(arguments$line[[argument]], arguments$col[[argument]]),
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
            if (!is.null(context) || deferred) {
                hints[[hint_count]]$data$opening <- opening
                hints[[hint_count]]$data$version <- document$version
                if (deferred) hints[[hint_count]]$data$deferred <- TRUE else hints[[hint_count]]$data$member <- member
            }
            if (hint_count >= 200L) break
        }
        if (hint_count >= 200L) break
    }
    Response$new(id, result = hints[seq_len(hint_count)])
}

# Most uses of := in data.table expressions need only ordinary formals.
# The full lexical lookup still verifies masking and ordering at each S7 call.
inlay_s7_bind_possible <- function(workspace, document) {
    data <- document$parse_data$member_data
    view <- member_workspace_view(workspace, document)
    imports <- if (is.null(view$bindings)) data$imports else view$imports
    packages <- unique(c(workspace$startup_packages, vapply(imports, function(item) {
        if (length(item$expr) < 2L) return("")
        member_name(item$expr[[2L]]) %s3_or% ""
    }, character(1L))))
    binding <- function(package, target) {
        if (package == "S7" && target == ":=") return(TRUE)
        metadata <- workspace$member_metadata
        catalog <- if (is.function(metadata$catalog)) metadata$catalog(package) else if (is.function(metadata$get)) metadata$get(package, NULL)
        root <- member_lookup(catalog$roots, target)
        identical(root$metadata, "S7") && identical(root$function_key, ":=")
    }
    for (package in packages) if (binding(package, ":=")) return(TRUE)
    for (import in data$namespace_bindings) {
        for (target in import$targets[import$names == ":="]) if (binding(import$package, target)) return(TRUE)
    }
    FALSE
}

# A bare call can be a class constructor, a local alias or a method visible
# inside a class. Test the compact parse and package catalogs before trying
# receiver inference for every ordinary call in a viewport.
# TRUE requests the member signature, FALSE uses ordinary hints, and NA
# reuses verified parameter names while deferring contextual tooltip lookup.
inlay_symbol_candidates <- function(workspace, document) {
    parsed <- document$parse_data
    data <- parsed$member_data
    metadata <- workspace$member_metadata
    packages <- unique(vapply(data$imports, function(item) {
        if (length(item$expr) < 2L) return("")
        member_name(item$expr[[2L]]) %s3_or% ""
    }, character(1L)))
    view <- member_workspace_view(workspace, document)
    histories <- if (is.null(view$bindings)) data$bindings else view$bindings
    scope <- member_symbol_required(workspace, document) || length(view$bindings) > 0L
    classes <- parsed$class_data$declarations
    # The names of inherited non-portable R6 and RefClass methods can be
    # supplied by another file. Their method bodies still need full resolution.
    inherited <- Filter(function(cls) cls$type %in% c("R6", "RefClass") && length(cls$parents) > 0L, classes)
    first <- vapply(inherited, function(cls) cls$range$start$line, integer(1L))
    last <- vapply(inherited, function(cls) cls$range$end$line, integer(1L))
    metadata_keys <- if (!is.null(metadata)) metadata$keys()
    package_peers <- isTRUE(data$package_classes) && isTRUE(workspace$index$enabled)
    decisions <- new.env(hash = TRUE, parent = emptyenv())
    import_aliases <- NULL
    local <- parsed$inlay_member_candidate_names
    if (is.null(local) && scope) {
        nav <- parsed$navigation_data
        syntax <- parsed$syntax_data
        # Ordinary function literals already have their formals in the lexical
        # index. Other bindings and formal defaults may hold extracted methods.
        rows <- c(nav$name[nav[["function"]] == 0L], nav$scope_name)
        members <- unlist(lapply(classes, function(cls) {
            vapply(cls$children, `[[`, character(1L), "name")
        }), use.names = FALSE)
        local <- unique(c(reference_symbol_names(syntax$text[rows]), members))
        if (is.environment(parsed)) parsed$inlay_member_candidate_names <- local
    }
    # Share the ordinary provider only for an independently verified signature.
    # Prefixes retain full lookup for S7's exact and partial name= matching.
    same_formals <- function(fn, name, owner) {
        if (!member_head(fn, "function") || !is.function(workspace$get_formals)) return(FALSE)
        keys <- names(fn[[2L]])
        if (!length(keys) || any(startsWith(keys, "name"))) return(FALSE)
        if (!identical(owner, WORKSPACE) && (!is.function(workspace$namespaces$has) ||
                    !workspace$namespaces$has(owner))) return(FALSE)
        actual <- tryCatch(call_with_optional_uri(workspace$get_formals, name, owner, uri = document$uri),
            error = function(e) NULL)
        identical(keys, names(actual))
    }
    catalog_candidate <- function(pkg, name, qualified, bound = FALSE, owner = NULL) {
        if (!pkg %in% metadata_keys) return(FALSE)
        catalog <- if (is.function(metadata$catalog)) metadata$catalog(pkg) else metadata$get(pkg)
        if (is.null(catalog)) return(FALSE)
        # A missing member callable cannot supply an adjusted signature.
        # Reexports and external class snapshots retain the full lookup.
        if (bound && length(catalog$s7_dependencies)) return(TRUE)
        value <- if (qualified) member_lookup(catalog$namespace_roots, paste(pkg, name, sep = "::"))
        if (is.null(value)) value <- member_lookup(catalog$roots, name)
        if (bound) {
            if (!is.null(value)) return(TRUE)
            if (!name %in% catalog$exports) return(FALSE)
            if (!name %in% catalog$functions || !identical(pkg, owner)) return(TRUE)
            model <- metadata$get(pkg)
            if (name %in% model$native_factories || !same_formals(model$definitions[[name]], name, pkg)) return(TRUE)
            # NA uses ordinary labels and defers contextual help until resolve.
            return(NA)
        }
        !is.null(value) && (!is.null(value$s7_generator) || !is.null(value$s7_dispatch_args) || !is.null(value$s4_generator) ||
                !is.null(value$refclass_instance) || !is.null(value$r6_role) || !is.null(value$r6_owner) ||
                isTRUE(value$extracted_callable))
    }
    function(name, package, line, bound = FALSE, ordinary = TRUE) {
        if (identical(name, "new") && (is.null(package) || package == "methods")) return(TRUE)
        if (is.null(package) && any(first <= line & line <= last)) return(TRUE)
        if (bound && (is.null(parsed$navigation_data) || isTRUE(parsed$parse_error) ||
                    !identical(document$version, parsed$version))) return(TRUE)
        if (!scope && is.null(package) && !bound) return(FALSE)
        history <- if (bound && is.null(package)) histories[[name]]
        if (length(history) == 1L && line <= history[[1L]]$end[[1L]]) return(TRUE)
        key <- paste(package %s3_or% "", name, sep = "::")
        if (bound) key <- paste0("bound:", key)
        cached <- decisions[[key]]
        if (!is.null(cached)) return(cached)
        candidate <- if (bound) {
            # A same-named local function or formal can mask the workspace
            # signature. Nonportable R6 methods also enclose synthetic bindings.
            if (is.null(import_aliases)) {
                renamed <- lapply(data$namespace_bindings, function(x) x$names[x$names != x$targets])
                import_aliases <<- as.character(unlist(renamed, use.names = FALSE))
            }
            owner <- if (ordinary) {
                if (!is.null(package)) package else if (is.function(workspace$guess_namespace)) {
                    tryCatch(call_with_optional_uri(workspace$guess_namespace, name, isf = TRUE, uri = document$uri),
                        error = function(e) NULL)
                }
            }
            records <- parsed$navigation_data$by_name[[name]]
            source <- is.null(package) && (isTRUE(data$class_scope) || name %in% local || name %in% import_aliases ||
                    (package_peers && workspace$index$has_definition(name, document$uri)))
            if (!source && is.null(package) && (length(records) || length(history))) {
                verified <- identical(owner, WORKSPACE) && length(history) == 1L && length(records) == 1L &&
                    length(data$bindings[[name]]) == 1L &&
                    class_top_level_statement(parsed$syntax_data, parsed$navigation_data$node[[records]]) &&
                    same_formals(history[[1L]]$expr, name, owner)
                source <- if (verified) NA else TRUE
            }
            source || any(vapply(metadata_keys, catalog_candidate, logical(1L), name = name,
                    qualified = !is.null(package), bound = TRUE, owner = owner))
        } else if (!is.null(package)) {
            catalog_candidate(package, name, TRUE)
        } else {
            name %in% local || any(vapply(view$bindings[[name]], function(item) {
                !member_head(item$expr, "function")
            }, logical(1L))) || any(vapply(packages, catalog_candidate, logical(1L), name = name, qualified = FALSE))
        }
        decisions[[key]] <- candidate
        candidate
    }
}

# Recover the specific callee using the same opening parenthesis as signature
# help. An unresolved member must never borrow a same-named global's formals.
inlay_call_context <- function(workspace, document, opening, member, symbols, generic) {
    point <- document$from_lsp_position(opening)
    point$col <- point$col + 1L
    call <- document$detect_call(point)
    if (member || symbols) {
        location <- member_call_location(document, point, call, symbols = symbols)
        symbol <- member_call_symbol(document$uri, workspace, document, location)
        if (!is.null(symbol)) return(list(formals = symbol$formals, symbol = symbol, signature = symbol$signature))
    }
    if (generic && !member) {
        s3 <- resolve_s3_call(document$uri, workspace, document, point, call = call)
        if (!is.null(s3)) return(list(formals = s3$formals, s3 = s3, signature = s3$signature))
    }
    NULL
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
    if (!is.null(hint$data$opening)) {
        document <- workspace$documents$get(hint$data$uri, NULL)
        if (is.null(document) || !isTRUE(document$version == hint$data$version)) return(Response$new(id, result = hint))
        context <- inlay_call_context(workspace, document, hint$data$opening, isTRUE(hint$data$member),
            isTRUE(lsp_settings$get("member_completion")), isTRUE(lsp_settings$get("s3_dispatch")))
        if (!is.null(context$signature) && parameter %in% names(context$formals)) {
            doc <- if (!is.null(context$s3)) s3_documentation(workspace, context$s3) else
                member_symbol_documentation(workspace, context$symbol, document$uri)
            contents <- argument_hover_contents(doc, context$signature, parameter)
            if (is.null(contents)) contents <- c(sprintf("```r\n%s\n```", context$signature),
                if (is.character(doc)) doc else doc$description)
            hint$tooltip <- list(kind = "markdown", value = paste(contents, collapse = "\n\n"))
            return(Response$new(id, result = hint))
        }
        if (!isTRUE(hint$data$deferred) || !is.null(context$signature)) return(Response$new(id, result = hint))
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
