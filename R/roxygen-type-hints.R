max_type_hint_alternatives <- 32L
roxygen_type_identifier <- "(?!\\.[0-9])[[:alpha:].][[:alnum:]_.]*"
roxygen_nominal_type <- paste0("(*UCP)^", roxygen_type_identifier, "(?:::", roxygen_type_identifier, ")?$")
roxygen_param_spelling <- r"(`(?:\\.|[^`\\])*`|\S+)"

# Consider only a description or continuation start. The full parser decides
# what the marker contains and whether a phrase really describes a class.
roxygen_type_candidate_pattern <- local({
    named <- "@(?:param|field|slot|prop)"
    tag <- paste0("(?:", named, "\\h+(?:", roxygen_param_spelling, ")\\h+|@returns?\\h+)?")
    marker <- r"(\(`|\\?\[`|\[\\code\{)"
    class_phrase <- r"((?i:an\h+object)(?:\h+of\b|\h*$))"
    article <- r"((?i:a|an|the)(?:\h+\[`|\h*$))"
    # A tag or article can end the physical line before its name or class.
    lead <- paste(marker, class_phrase, article, sep = "|")
    paste0("(?m)^\\h*#'\\h*(?:", tag, "(?:", lead, ")|", named, "\\h*$)")
})

roxygen_type_candidate_rows <- function(lines) {
    comments <- grep("#'", lines, fixed = TRUE)
    if (!length(comments)) return(integer())
    comments[grepl(roxygen_type_candidate_pattern, lines[comments], perl = TRUE)]
}

roxygen_has_type_span <- function(lines) length(roxygen_type_candidate_rows(lines)) > 0L

# Most source files have neither an inheritance edge nor a donor. Only a
# standalone roxygen tag with subsequent source can introduce a topic record.
roxygen_has_topic_tags <- function(content, all_blocks = FALSE) {
    if (length(content) < 2L) return(FALSE)
    tagged <- grep(if (all_blocks) "#'" else "@", content, fixed = TRUE)
    tagged <- tagged[tagged < length(content)]
    pattern <- if (all_blocks) "^\\h*#'" else roxygen_topic_tag_pattern
    length(tagged) > 0L && any(grepl(pattern, content[tagged], perl = TRUE))
}

roxygen_has_inheritance <- function(lines) {
    matches <- grep("@inheritParams", lines, fixed = TRUE)
    length(matches) > 0L && any(grepl(r"((?m)^\h*#'\h*@inheritParams(?:\s|$))", lines[matches], perl = TRUE))
}

# Package R/ files can donate documentation to a different source file. An
# ordinary script only needs prose donors when it actually uses inheritance.
roxygen_package_topic_file <- function(uri) {
    is.null(uri) || grepl("/R/[^/]+\\.[rR]$", uri)
}

roxygen_topic_tag_pattern <- r"(^\h*#'\h*@(param|inheritParams|name|rdname|describeIn)\b)"
roxygen_topic_name_pattern <- r"((*UCP)^[[:alnum:]_.-]+$)"

roxygen_param_names <- function(spelling) {
    if (startsWith(spelling, "`")) reference_symbol_names(spelling) else strsplit(spelling, ",", fixed = TRUE)[[1L]]
}

# Keep direct documentation separately from direct types: ordinary prose also
# fills a parameter and blocks a later donor. Preserve grouped documentation
# and the source order of inheritance directives for the topic resolver.
roxygen_topic_tags <- function(comments, formals) {
    lines <- sub("^#'\\s*", "", comments, perl = TRUE)
    starts <- which(startsWith(lines, "@"))
    ends <- c(starts[-1L] - 1L, length(lines))
    groups <- inherit <- inherit_ranges <- list()
    group_labels <- names <- rdnames <- documented <- character()
    group_tags <- integer()
    hidden <- FALSE
    for (i in seq_along(starts)) {
        line <- lines[[starts[[i]]]]
        tag <- sub("^@([^[:space:]]+).*$", "\\1", line)
        if (!tag %in% c("param", "inheritParams", "name", "rdname", "noRd", "describeIn")) next
        continuation <- lines[seq.int(starts[[i]], ends[[i]])[-1L]]
        value <- trimws(paste(c(substring(line, nchar(tag) + 2L), continuation), collapse = " "))
        if (tag == "param") {
            # A name alone is not a documented argument. The description may
            # start on a subsequent roxygen line.
            match <- regmatches(value, regexec(paste0("^(", roxygen_param_spelling, ")\\s+\\S"), value, perl = TRUE))[[1L]]
            if (length(match)) {
                spelling <- match[[2L]]
                label <- if (startsWith(spelling, "`")) reference_symbol_names(spelling) else spelling
                # The Rd parameter record has the whole, unquoted label. Keep
                # its full group even if this function has only some of the
                # formals: all names must match in the *recipient*.
                group_labels <- c(group_labels, label)
                group_tags <- c(group_tags, i)
                groups[[length(groups) + 1L]] <- strsplit(label, ",\\s*", perl = TRUE)[[1L]]
                documented <- c(documented, roxygen_param_names(spelling))
            }
        } else if (tag == "inheritParams") {
            match <- regmatches(value, regexec(r"((*UCP)^((?:[[:alpha:]][[:alnum:].]*::)?[[:alnum:]_.-]+)(?:\s+(.*))?$)",
                    value, perl = TRUE))[[1L]]
            if (length(match)) {
                donor <- match[[2L]]
                inherit[[length(inherit) + 1L]] <- list(topic = donor, selector = match[[3L]])
                # Locate the source name independently of a package qualifier
                # and any selector. It can start on a continuation line.
                j <- starts[[i]]
                offset <- nchar(tag) + 1L
                while (!nzchar(trimws(substring(lines[[j]], offset + 1L))) && j < ends[[i]]) {
                    j <- j + 1L
                    offset <- 0L
                }
                at <- regexpr(donor, substring(lines[[j]], offset + 1L), fixed = TRUE)[[1L]]
                end <- nchar(comments[[j]]) - nchar(lines[[j]]) + offset + at - 1L + nchar(donor)
                inherit_ranges[[length(inherit)]] <- c(j, end - nchar(sub("^.*::", "", donor)), end)
            }
        } else if (tag == "noRd") {
            hidden <- TRUE
        } else if (tag == "describeIn") {
            page <- sub("[[:space:]].*$", "", value)
            if (grepl(roxygen_topic_name_pattern, page, perl = TRUE)) rdnames <- c(rdnames, page) else hidden <- TRUE
        } else if (!grepl(roxygen_topic_name_pattern, value, perl = TRUE)) {
            hidden <- TRUE
        } else if (tag == "name") {
            names <- c(names, value)
        } else {
            rdnames <- c(rdnames, value)
        }
    }
    if (!is.null(formals)) documented <- intersect(documented, formals)
    # An exactly repeated @param label replaces its earlier description.
    # Overlapping labels (e.g. `a,b` and `a`) remain distinct Rd items.
    keep <- !duplicated(group_labels, fromLast = TRUE)
    list(documented = unique(documented), groups = groups[keep], group_tags = group_tags[keep], inherit = inherit,
        inherit_ranges = inherit_ranges, names = unique(names), rdnames = unique(rdnames), hidden = hidden)
}

# A type declaration starts the tag's description. Keep the physical spans of
# each alternative and preserve the distinct evidence required by help links.
roxygen_type_block <- function(comments, formals = NULL, rows = NULL, columns = NULL, collect_groups = FALSE,
    directives = c("param", "return", "returns"), field = NULL, class_name = NULL) {
    prefix <- attr(regexpr("^#'\\s*", comments, perl = TRUE), "match.length")
    lines <- substring(comments, prefix + 1L)
    starts <- which(startsWith(lines, "@"))
    ends <- c(starts[-1L] - 1L, length(lines))
    params <- group_types <- explicit_groups <- members <- list()
    returns <- character()
    occurrences <- list()
    pattern <- r"(^@(?|(param|field|slot|prop)\s+(`(?:\\.|[^`\\])*`|\S+)|(returns?)())\s+)"
    for (i in seq_along(starts)) {
        section <- seq.int(starts[[i]], ends[[i]])
        tag <- paste(lines[section], collapse = "\n")
        match <- regexec(pattern, tag, perl = TRUE)
        parts <- regmatches(tag, match)[[1L]]
        if (!length(parts) || !parts[[2L]] %in% directives) next
        tag_name <- parts[[2L]]
        named <- nzchar(parts[[3L]])
        header <- attr(match[[1L]], "match.length")[[1L]]
        declared <- roxygen_leading_types(substring(tag, header + 1L))
        if (is.null(declared)) next
        types <- declared$types
        unique_types <- declared$unique
        parameter <- NA_character_
        if (tag_name == "param") {
            parameter <- parts[[3L]]
            names <- roxygen_param_names(parameter)
            if (!is.null(formals)) names <- intersect(names, formals)
            if (!length(names)) next
            advisory <- length(attr(unique_types, "roxygen_class_links", exact = TRUE)) > 0L
            if (collect_groups) {
                group <- if (startsWith(parameter, "`")) reference_symbol_names(parameter) else parameter
                if (!advisory) explicit_groups[[group]] <- unique_types
                group_types[[as.character(i)]] <- explicit_groups[[group]] %s3_or% unique_types
            }
            use <- if (advisory) names[!vapply(params[names], roxygen_authoritative_types, logical(1L))] else names
            params[use] <- rep(list(unique_types), length(use))
        } else if (named) {
            parameter <- parts[[3L]]
            name <- reference_symbol_names(parts[[3L]])
            if (tag_name == "prop" && grepl("@", name, fixed = TRUE)) {
                if (is.null(class_name) || !startsWith(name, paste0(class_name, "@"))) next
                name <- substring(name, nchar(class_name) + 2L)
            }
            keys <- if (tag_name == "field") roxygen_param_names(parameter) else name
            if (!length(keys) || any(!nzchar(keys)) || (!is.null(field) && !field %in% keys)) next
            for (key in keys) {
                previous <- members[[tag_name]][[key]]
                members[[tag_name]][[key]] <- roxygen_merge_member_types(previous, unique_types)
            }
        } else {
            if (roxygen_authoritative_types(unique_types) || !roxygen_authoritative_types(returns)) returns <- unique_types
        }
        if (!is.null(rows)) {
            # Match offsets count code points in the joined description. Map
            # the terminal identifier back to its original comment and column.
            class_names <- sub("^.*::", "", types)
            offsets <- header + declared$starts
            line_starts <- c(0L, cumsum(nchar(lines[section]) + 1L))
            which_line <- findInterval(offsets, line_starts)
            source <- section[which_line]
            start <- columns[source] + prefix[source] + offsets - line_starts[which_line]
            parameter_row <- parameter_start <- NA_integer_
            if (!is.na(parameter)) {
                offset <- match[[1L]][[3L]] - 1L
                line <- findInterval(offset, line_starts)
                source_line <- section[[line]]
                parameter_row <- rows[[source_line]]
                parameter_start <- columns[[source_line]] + prefix[[source_line]] + offset - line_starts[[line]]
            }
            occurrences[[length(occurrences) + 1L]] <- data.frame(type = types, row = rows[source],
                start = start, end = start + nchar(class_names), advisory = declared$advisory, directive = tag_name, parameter = parameter,
                parameter_row = parameter_row, parameter_start = parameter_start, stringsAsFactors = FALSE)
        }
    }
    if (!length(params) && !length(returns) && !length(members)) return(NULL)
    hints <- c(list(params = params, returns = returns), members)
    if (length(group_types)) attr(hints, "group_types") <- group_types
    if (length(occurrences)) attr(hints, "occurrences") <- do.call(rbind, occurrences)
    hints
}

roxygen_declaration_row <- function(syntax, row) {
    parent <- syntax$parent_row
    while (parent[[row]] && syntax$line1[[parent[[row]]]] == syntax$line1[[row]] &&
            syntax$col1[[parent[[row]]]] == syntax$col1[[row]]) row <- parent[[row]]
    row
}

# Follow assignments and parentheses to their value. Named arguments include
# R6 methods and need the value following their argument tag.
roxygen_function_row <- function(syntax, row, declaration = roxygen_declaration_row(syntax, row), memo = NULL) {
    token <- syntax$token
    if (token[[row]] %in% c("SYMBOL_SUB", "SYMBOL_FORMALS") || (token[[row]] == "STR_CONST" && row == declaration)) {
        # Follow two siblings, even in a class with thousands of members.
        eq <- syntax$next_sibling[[row]]
        while (eq && token[[eq]] == "COMMENT") eq <- syntax$next_sibling[[eq]]
        if (!eq || !token[[eq]] %in% c("EQ_SUB", "EQ_FORMALS")) return(0L)
        row <- syntax$next_sibling[[eq]]
        while (row && token[[row]] == "COMMENT") row <- syntax$next_sibling[[row]]
        if (!row) return(0L)
    } else {
        row <- declaration
    }
    repeat {
        parts <- syntax_children(syntax, row)
        parts <- parts[token[parts] != "COMMENT"]
        if (!length(parts)) return(0L)
        kinds <- token[parts]
        if (kinds[[1L]] %in% c("FUNCTION", r"('\\')")) return(parts[[1L]])
        if (length(parts) >= 3L && kinds[[2L]] == "'('") {
            # Roxygen documents the registered function at the outer call.
            # Other callback arguments belong to their enclosing R call.
            return(roxygen_registered_function(syntax, row, declaration, parts[[1L]], memo))
        }
        if (length(parts) != 3L) return(0L)
        if (kinds[[2L]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) row <- parts[[3L]]
        else if (kinds[[2L]] == "RIGHT_ASSIGN") row <- parts[[1L]]
        else if (identical(kinds[c(1L, 3L)], c("'('", "')'"))) row <- parts[[2L]]
        else return(0L)
    }
}

# Identify a published top-level assignment or documentation-only NULL. A
# nested function/method can consume a hint without publishing its own name
# as a package-wide donor. Resolve simple left and right assignments only.
roxygen_topic_declaration <- function(syntax, row) {
    if (!class_top_level_statement(syntax, row)) return(list())
    parts <- syntax_children(syntax, row)
    parts <- parts[syntax$token[parts] != "COMMENT"]
    if (length(parts) == 1L && syntax$token[parts] == "NULL_CONST") return(list(doc_only = TRUE))
    if (length(parts) != 3L) return(list())
    op <- syntax$text[[parts[[2L]]]]
    target <- if (op %in% c("<-", "=")) parts[[1L]] else if (op == "->") parts[[3L]]
    if (is.null(target)) return(list())
    symbol <- syntax_children(syntax, target)
    if (length(symbol) == 1L && syntax$token[[symbol]] == "SYMBOL") {
        return(list(binding = reference_symbol_names(syntax$text[[symbol]])))
    }
    list()
}

# Both package indexes already have the parsed top-level statements. A
# whole line between consecutive, verified statement spans is outside strings
# and nested scopes. Use those gaps to index ordinary title/@param blocks in
# bulk without materializing the document's navigation tree or walking bodies.
# FALSE asks for the full syntax path when a block needs richer interpretation.
roxygen_shallow_prose_index <- function(content, parsed, uri) {
    statements <- top_level_statements(parsed)
    refs <- attr(statements, "srcref", exact = TRUE)
    if (!length(refs) || length(refs) != length(statements) || any(grepl("\t", content, fixed = TRUE)) ||
            any(startsWith(content, "#line"))) return(FALSE)
    starts <- vapply(refs, `[[`, integer(1L), 7L)
    ends <- vapply(refs, `[[`, integer(1L), 8L)
    comments <- grep(r"(^\h*#')", content, perl = TRUE)
    next_statement <- findInterval(comments, starts) + 1L
    keep <- next_statement <= length(starts) & comments > c(0L, ends)[next_statement]
    comments <- comments[keep]
    next_statement <- next_statement[keep]
    code <- which(!grepl(r"(^\h*(?:#|$))", content, perl = TRUE))
    keep <- findInterval(comments, code) == findInterval(starts[next_statement] - 1L, code)
    if (!any(keep)) return(NULL)
    next_statement <- next_statement[keep]
    blocks <- content[comments[keep]]
    hidden <- next_statement[grepl(r"(^\h*#'\h*@noRd(?:\h|$))", blocks, perl = TRUE)]
    if (length(hidden)) {
        visible <- !next_statement %in% hidden
        if (!any(visible)) return(NULL)
        blocks <- blocks[visible]
        next_statement <- next_statement[visible]
    }
    if (any(grepl(r"(^\h*#'\h*@(name|rdname|describeIn)\b)", blocks, perl = TRUE))) return(FALSE)
    params <- grepl(r"(^\h*#'\h*@param\b)", blocks, perl = TRUE)
    simple <- r"((*UCP)^\h*#'\h*@param\h+([[:alnum:]_.]+(?:,[[:alnum:]_.]+)*)\h+\S.*$)"
    if (any(!grepl(simple, blocks[params], perl = TRUE))) return(FALSE)
    labels <- sub(simple, "\\1", blocks[params], perl = TRUE)
    groups <- strsplit(labels, ",", fixed = TRUE)
    owners <- unique(next_statement)
    by_statement <- split(seq_along(labels), factor(next_statement[params], levels = owners))
    topics <- vector("list", length(owners))
    keys <- character(length(owners))
    for (i in seq_along(owners)) {
        expr <- statements[[owners[[i]]]]
        if (length(expr) != 3L || !(member_head(expr, "<-") || member_head(expr, "=")) || !is.symbol(expr[[2L]])) next
        fn <- expr[[3L]]
        if (member_head(fn, "(") || member_head(fn, "<-") || member_head(fn, "=")) return(FALSE)
        if (!member_head(fn, "function")) next
        if (length(fn) != 4L || !inherits(fn[[4L]], "srcref")) return(FALSE)
        name <- as.character(expr[[2L]])
        formals <- names(fn[[2L]])
        if (is.null(formals)) formals <- character()
        tags <- by_statement[[i]]
        grouped <- groups[tags[!duplicated(labels[tags], fromLast = TRUE)]]
        documented <- intersect(as.character(unlist(groups[tags], use.names = FALSE)), formals)
        line <- fn[[4L]][[7L]]
        col <- fn[[4L]][[5L]]
        keys[[i]] <- paste(line, col, sep = ":")
        topics[[i]] <- list(topic = name, page = name, formals = formals,
            documented = documented, groups = grouped, inherit = list(), row = line - 1L, col = col - 1L)
    }
    keep <- nzchar(keys)
    if (any(keep)) structure(list(), rows = integer(), cols = integer(),
        topics = stats::setNames(topics[keep], keys[keep]), uri = uri, has_inheritance = FALSE)
}

# Whitespace and ordinary comments can separate roxygen from its declaration.
# The first real token stops the search. A fence or a discarded statement in a
# partial projection is also a boundary, even if the projection blanked it out.
member_roxygen_type_index <- function(content, data, row_offset = 0L, barriers = integer(), allowed_comments = NULL,
    value_source = NULL, uri = NULL, col_offset = 0L, parsed = NULL) {
    inherits <- roxygen_has_inheritance(content)
    plain_topics <- inherits || roxygen_package_topic_file(uri)
    candidates <- roxygen_type_candidate_rows(if (is.null(value_source)) content else value_source)
    typed_document <- length(candidates) > 0L
    # Prose suppresses inherited R6 types on overrides. A parentless class
    # in an untyped file has no such member defaults to mask.
    r6_docs <- (typed_document || inherits || any(grepl("inherit", content, fixed = TRUE))) &&
        any(grepl("R6Class", content, fixed = TRUE)) && any(grepl("@(field|param)\\b", content, perl = TRUE))
    if (!inherits && !typed_document && !r6_docs) {
        if (!plain_topics || !roxygen_has_topic_tags(content, all_blocks = !is.null(uri))) return(NULL)
        if (!is.null(parsed) && !length(barriers) && is.null(allowed_comments) && !row_offset && !col_offset) {
            fast <- roxygen_shallow_prose_index(content, parsed, uri)
            if (!identical(fast, FALSE)) return(fast)
        }
    }
    if (!length(data$token)) return(NULL)
    if (is.null(data$terminals)) data <- syntax_parse_data(data)
    terminals <- data$terminals
    kind <- data$token[terminals]
    line <- data$line1[terminals]
    text <- data$text[terminals]
    comments <- which(kind == "COMMENT" & startsWith(text, "#'"))
    comments <- comments[trimws(content[line[comments]]) == trimws(text[comments])]
    if (!is.null(allowed_comments)) comments <- comments[line[comments] %in% allowed_comments]
    if (!length(comments)) return(NULL)
    code <- which(kind != "COMMENT")
    next_code <- code[findInterval(comments, code) + 1L]
    keep <- !is.na(next_code)
    # Inline named arguments cannot publish package documentation topics.
    if (!typed_document && !inherits && !r6_docs) keep <- keep & kind[next_code] != "SYMBOL_SUB"
    if (!any(keep)) return(NULL)
    if (length(barriers)) {
        barriers <- sort(unique(barriers))
        keep <- keep & findInterval(line[comments], barriers) == findInterval(line[next_code], barriers)
    }
    groups <- split(comments[keep], next_code[keep])
    typed_lines <- logical(length(content))
    typed_lines[candidates] <- TRUE
    classes <- list()
    r6_lists <- new.env(parent = emptyenv())
    batches <- if (r6_docs && length(groups) >= 8L) {
        roxygen_field_batches(data, groups, terminals, text, content, value_source, row_offset, col_offset, r6_lists, typed_lines)
    }
    if (length(batches$skip)) groups[batches$skip] <- NULL
    result <- topics <- vector("list", length(groups))
    locations <- new.env(hash = TRUE, parent = emptyenv())
    rows <- cols <- integer(length(groups))
    positions <- character(length(groups))
    keys <- names(groups)
    for (i in seq_along(groups)) {
        key <- keys[[i]]
        group <- groups[[i]]
        tagged <- text[group]
        typed <- typed_document && any(typed_lines[line[group]])
        # A title-only primary block can share an Rd page with an explicitly
        # named secondary block, even when it has no @param tags of its own.
        r6_block <- r6_docs && any(grepl("@(field|param)\\b", tagged, perl = TRUE))
        if (!typed && !plain_topics && !r6_block) next
        first <- terminals[[as.integer(key)]]
        statement <- roxygen_declaration_row(data, first)
        fn <- roxygen_function_row(data, first, statement, r6_lists)
        declaration <- roxygen_topic_declaration(data, statement)
        inline <- r6_block || (typed && any(grepl("@(field|slot|prop)\\b", tagged, perl = TRUE)))
        target <- if ((typed || r6_block) && (!fn || inline)) roxygen_class_target(data, first, statement, inline, r6_lists)
        if (!fn && !isTRUE(declaration$doc_only) && is.null(target)) next
        marker <- if (fn) fn else first
        if (fn) {
            parts <- syntax_children(data, data$parent_row[[fn]])
            formals <- reference_symbol_names(data$text[parts[data$token[parts] == "SYMBOL_FORMALS"]])
        } else {
            formals <- NULL
        }
        comment_rows <- line[group]
        comment_cols <- as.integer(regexpr("#'", content[comment_rows], fixed = TRUE)) - 1L
        comments <- if (is.null(value_source)) tagged else substring(value_source[comment_rows], comment_cols + 1L)
        tags <- roxygen_topic_tags(comments, formals)
        at_rows <- comment_rows + row_offset - 1L
        at_cols <- comment_cols + ifelse(comment_rows == 1L, col_offset, 0L)
        if (!is.null(target)) {
            source_row <- data$line1[[target$row]]
            class_row <- source_row + row_offset
            class_col <- data$col1[[target$row]] + if (source_row == 1L) col_offset else 0L
            owner <- paste(class_row, class_col, sep = ":")
            batch <- batches$records[[key]]
            block <- if (is.null(batch)) {
                roxygen_class_block(data, target, comments, tags, typed, at_rows, at_cols, classes[[owner]])
            } else {
                roxygen_class_record(target$name, classes[[owner]], batch$values, fields = batch$fields)
            }
            if (!is.null(block)) {
                if (is.null(block$record$anchors)) block$record$anchors <- roxygen_class_route(data, target$row, row_offset, col_offset)
                classes[[owner]] <- block$record
                sites <- block$sites
                if (!is.null(sites)) {
                    sites$function_row <- class_row - 1L
                    sites$function_col <- class_col - 1L
                    sites$doc_only <- FALSE
                    locations[[paste0("class:", key)]] <- sites
                }
            }
            if (!fn) next
        }
        hints <- if (typed) roxygen_type_block(comments, formals, at_rows, at_cols, collect_groups = TRUE)
        group_types <- attr(hints, "group_types", exact = TRUE)
        if (length(group_types)) {
            group_types <- unname(group_types[as.character(tags$group_tags)])
            attr(hints, "group_types") <- NULL
        }
        source_row <- data$line1[[marker]]
        fn_row <- source_row + row_offset
        fn_col <- data$col1[[marker]] + if (source_row == 1L) col_offset else 0L
        column <- data$col1[[marker]] - 1L
        source <- content[[source_row]]
        if (grepl("\t", source, fixed = TRUE)) column <- index_physical_columns(column, 1L, source, TRUE)
        if (source_row == 1L) column <- column + col_offset
        position <- paste(fn_row, fn_col, sep = ":")
        positions[[i]] <- position
        binding <- declaration$binding
        topic <- page <- NULL
        if ((length(binding) || isTRUE(declaration$doc_only)) && !tags$hidden && length(tags$names) <= 1L && length(tags$rdnames) <= 1L) {
            # @inheritParams searches the Rd topic name. @rdname and
            # @describeIn select its output file; @aliases are not donors.
            topic <- if (length(tags$names)) tags$names[[1L]] else binding
            if (!is.null(topic)) page <- if (length(tags$rdnames)) tags$rdnames[[1L]] else topic
        }
        if ((plain_topics && length(topic)) || length(tags$inherit) || length(hints)) {
            record <- list(topic = topic, page = page, formals = formals, documented = tags$documented,
                groups = tags$groups, inherit = tags$inherit, row = fn_row - 1L, col = column)
            if (length(tags$names)) record$fixed_name <- TRUE
            if (length(tags$rdnames)) record$fixed_page <- TRUE
            if (length(tags$inherit_ranges)) {
                spans <- do.call(rbind, tags$inherit_ranges)
                record$inherit_ranges <- cbind(at_rows[spans[, 1L]], at_cols[spans[, 1L]] + spans[, 2L],
                    at_cols[spans[, 1L]] + spans[, 3L])
            }
            if (any(lengths(group_types) > 0L)) record$group_types <- group_types
            topics[[i]] <- record
        }
        occurrences <- attr(hints, "occurrences", exact = TRUE)
        if (!fn && !is.null(occurrences)) occurrences <- occurrences[!is.na(occurrences$parameter), , drop = FALSE]
        if (!is.null(occurrences) && nrow(occurrences) && (fn || !is.null(topic))) {
            occurrences$doc_only <- !fn
            occurrences$function_row <- fn_row - 1L
            occurrences$function_col <- column
            locations[[position]] <- occurrences
        }
        method_docs <- identical(target$name, "R6Class") && !is.null(target$field) && length(tags$documented) > 0L
        if (!fn || (is.null(hints) && !length(tags$inherit) && !method_docs)) next
        if (is.null(hints)) hints <- list(params = list(), returns = character())
        attr(hints, "occurrences") <- NULL
        attr(hints, "roxygen_key") <- position
        if (length(tags$inherit)) attr(hints, "roxygen_inherits") <- TRUE
        if (length(tags$inherit) || method_docs) attr(hints, "roxygen_documented") <- tags$documented
        if (!is.null(uri)) attr(hints, "roxygen_uri") <- uri
        result[[i]] <- hints
        rows[[i]] <- fn_row
        cols[[i]] <- fn_col
    }
    # Only accepted blocks participate in each table. If a multiline
    # declaration has multiple blocks for one position, its last entry wins.
    keep_topics <- which(lengths(topics) > 0L)
    keep_topics <- keep_topics[!duplicated(positions[keep_topics], fromLast = TRUE)]
    topics <- if (length(keep_topics)) stats::setNames(topics[keep_topics], positions[keep_topics]) else list()
    keep <- which(rows > 0L)
    keep <- keep[!duplicated(positions[keep], fromLast = TRUE)]
    result <- if (length(keep)) stats::setNames(result[keep], positions[keep]) else list()
    rows <- rows[keep]
    cols <- cols[keep]
    if (!length(result) && !length(topics) && !length(classes)) return(NULL)
    order <- order(rows, cols)
    occurrences <- do.call(rbind, unname(as.list(locations, all.names = TRUE)))
    if (!is.null(occurrences)) {
        occurrences <- occurrences[order(occurrences$row, occurrences$start), , drop = FALSE]
        rownames(occurrences) <- NULL
    }
    has_inheritance <- any(vapply(topics, function(x) length(x$inherit) > 0L, logical(1L)))
    structure(result[order], rows = as.integer(rows[order]), cols = as.integer(cols[order]),
        occurrences = occurrences, topics = topics, classes = if (length(classes)) classes,
        uri = uri, has_inheritance = has_inheritance)
}

# Most documents have no hints. Only repaired indexes assembled before the
# document URI was known need their function entries stamped afterwards.
member_type_hint_uri <- function(index, uri) {
    if (is.null(index) || is.null(uri) || identical(attr(index, "uri", exact = TRUE), uri)) return(index)
    if (!isTRUE(attr(index, "has_inheritance")) && !roxygen_package_topic_file(uri)) {
        topics <- attr(index, "topics", exact = TRUE)
        keep <- names(topics) %in% names(index)
        sites <- attr(index, "occurrences", exact = TRUE)
        doc_only <- sites$doc_only
        if (any(doc_only)) {
            anchors <- paste(sites$function_row[doc_only], sites$function_col[doc_only])
            keep <- keep | vapply(topics, function(record) paste(record$row, record$col) %in% anchors, logical(1L))
        }
        if (!length(index) && !any(keep) && !length(attr(index, "classes", exact = TRUE))) return(NULL)
        attr(index, "topics") <- topics[keep]
    }
    for (i in seq_along(index)) attr(index[[i]], "roxygen_uri") <- uri
    attr(index, "uri") <- uri
    index
}

member_merge_type_hints <- function(partial, current) {
    if (is.null(partial)) return(current)
    if (is.null(current)) return(partial)
    old_topics <- attr(partial, "topics", exact = TRUE)
    new_topics <- attr(current, "topics", exact = TRUE)
    owner_keys <- union(names(current), names(new_topics))
    topics <- c(old_topics[!names(old_topics) %in% owner_keys], new_topics)
    keep_hints <- !names(partial) %in% owner_keys
    rows <- c(attr(partial, "rows")[keep_hints], attr(current, "rows"))
    cols <- c(attr(partial, "cols")[keep_hints], attr(current, "cols"))
    order <- order(rows, cols)
    a <- attr(partial, "occurrences", exact = TRUE)
    b <- attr(current, "occurrences", exact = TRUE)
    old_classes <- attr(partial, "classes", exact = TRUE)
    new_classes <- attr(current, "classes", exact = TRUE)
    classes <- c(old_classes[!names(old_classes) %in% names(new_classes)], new_classes)
    if (!is.null(a)) {
        owners <- c(vapply(new_topics, function(x) paste(x$row + 1L, x$col + 1L, sep = ":"), ""), names(new_classes))
        a <- a[!paste(a$function_row + 1L, a$function_col + 1L, sep = ":") %in% owners, , drop = FALSE]
    }
    occurrences <- rbind(a, b)
    if (!is.null(occurrences)) occurrences <- occurrences[order(occurrences$row, occurrences$start), , drop = FALSE]
    has_inheritance <- any(vapply(topics, function(x) length(x$inherit) > 0L, logical(1L)))
    structure(c(partial[keep_hints], current)[order], rows = rows[order], cols = cols[order],
        occurrences = occurrences, topics = topics, classes = classes, has_inheritance = has_inheritance)
}

# Failed parses can leave the function keyword and its formals as lexer tokens
# without a reduced function expression. Such a block cannot provide inferred
# types, but Rename must not silently leave a matching type spelling behind.
roxygen_damaged_type_blocks <- function(data, comments, row_offset) {
    if (!length(comments)) return(NULL)
    terminals <- which(data$terminal)
    terminals <- terminals[order(data$line1[terminals], data$col1[terminals])]
    kind <- data$token[terminals]
    code <- which(kind != "COMMENT")
    groups <- split(comments, findInterval(match(comments, terminals), code) + 1L)
    typed <- logical(length(data$text))
    typed[comments[roxygen_type_candidate_rows(data$text[comments])]] <- TRUE
    result <- list()
    code_rows <- terminals[code]
    tokens <- data$text[code_rows]
    pending <- which(tokens == "(" & data$parent[code_rows] <= 0L)
    factories <- c("R6Class", "setClass", "new_class", "new_generic", "setMethod", "setReplaceMethod", "setGeneric", "setGroupGeneric", "setAs")
    pending <- pending[pending > 1L & tokens[pmax(1L, pending - 1L)] %in% factories]
    for (key in names(groups)) {
        group <- groups[[key]]
        if (!any(typed[group])) next
        j <- as.integer(key)
        while (j <= length(code)) {
            if (kind[code[[j]]] == "'('") {
                j <- j + 1L
            } else if (j < length(code) && kind[code[[j]]] %in% c("SYMBOL", "SYMBOL_SUB", "SYMBOL_FORMALS") &&
                    kind[code[[j + 1L]]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "EQ_SUB", "EQ_FORMALS")) {
                j <- j + 2L
            } else {
                break
            }
        }
        if (j > length(code)) next
        if (!kind[code[[j]]] %in% c("FUNCTION", r"('\\')")) {
            # A class/registration call can be unreduced while each literal
            # field is already tokenized. Keep its source rows so Rename
            # declines only mentions the repaired index could not verify.
            inline <- any(pending < as.integer(key)) && any(grepl("^#'\\s*@(field|slot|prop)\\b", data$text[group], perl = TRUE))
            if (inline || any(pending %in% c(j + 1L, j + 3L))) {
                block <- roxygen_type_block(data$text[group], rows = data$line1[group] + row_offset - 1L,
                    columns = integer(length(group)), directives = c("field", "slot", "prop", "param", "return", "returns"))
                sites <- attr(block, "occurrences", exact = TRUE)
                sites <- sites[!sites$advisory, , drop = FALSE]
                for (line in unique(sites$row)) result[[paste0("tag:", line)]] <- sites$type[sites$row == line]
            }
            next
        }
        fn <- terminals[code[[j]]]
        # Partial recovery already cleared parent IDs that were never reduced.
        if (data$parent[[fn]] > 0L) next
        # A closed header identifies the valid @param names even when the
        # function body is unfinished. Only count the outer parameter list.
        formals <- NULL
        if (j < length(code) && kind[code[[j + 1L]]] == "'('") {
            depth <- 0L
            names <- character()
            for (k in seq.int(j + 1L, length(code))) {
                token <- kind[[code[[k]]]]
                if (token == "'('") depth <- depth + 1L
                if (token == "')'") depth <- depth - 1L
                if (!depth) {
                    formals <- names
                    break
                }
                if (depth == 1L && token == "SYMBOL_FORMALS") {
                    names <- c(names, reference_symbol_names(data$text[terminals[code[[k]]]]))
                }
            }
        }
        hints <- roxygen_type_block(data$text[group], formals, rows = data$line1[group], columns = integer(length(group)))
        sites <- attr(hints, "occurrences", exact = TRUE)
        if (any(!sites$advisory)) {
            position <- paste(data$line1[[fn]] + row_offset, data$col1[[fn]], sep = ":")
            result[[position]] <- unique(sites$type[!sites$advisory])
        }
    }
    result
}

roxygen_unresolved_type_hint <- function(data, spellings) {
    damaged <- data$damaged_type_blocks
    if (!length(damaged)) return(FALSE)
    missing <- setdiff(names(damaged), names(data$type_hints))
    tags <- startsWith(missing, "tag:")
    if (any(tags)) {
        sites <- attr(data$type_hints, "occurrences", exact = TRUE)
        for (key in missing[tags]) {
            documented <- sites$type[sites$row == as.integer(substring(key, 5L))]
            if (any(spellings %in% setdiff(damaged[[key]], documented))) return(TRUE)
        }
        missing <- missing[!tags]
    }
    any(spellings %in% unlist(damaged[missing], use.names = FALSE))
}

member_type_dependencies <- function(hints) {
    types <- unique(unlist(hints, use.names = FALSE))
    qualified <- types[grepl("::", types, fixed = TRUE)]
    unique(c(types, sub("::.*", "", qualified)))
}

# Copy only the paths leading to annotated functions. The native traversal is
# iterative; wide calls are converted to an indexed list once, and deep paths
# are rebuilt from leaves to roots without recursing on the R stack.
member_attach_type_hints <- function(parsed, index, row = 0L, col = 0L) {
    classes <- attr(index, "classes", exact = TRUE)
    if (length(classes) && length(parsed)) parsed <- member_attach_class_hints(parsed, classes, row, col)
    if (!length(index) || !length(parsed)) return(parsed)
    plan <- .Call("member_hint_paths_c", parsed, attr(index, "rows"), attr(index, "cols"),
        as.integer(c(row, col)), PACKAGE = "languageserver")
    n <- nrow(plan)
    if (!n) return(parsed)
    children <- split(seq_len(n)[-1L], factor(plan[-1L, 1L], levels = seq_len(n)))
    nodes <- parts <- vector("list", n)
    nodes[[1L]] <- parsed
    for (i in seq_len(n)) {
        kids <- children[[i]]
        if (length(kids)) {
            parts[[i]] <- as.list(nodes[[i]])
            nodes[kids] <- parts[[i]][plan[kids, 2L]]
        }
    }
    for (i in rev(seq_len(n))) {
        node <- nodes[[i]]
        kids <- children[[i]]
        dependencies <- attr(node, "member_type_names", exact = TRUE)
        if (length(kids)) {
            values <- parts[[i]]
            values[plan[kids, 2L]] <- nodes[kids]
            rebuilt <- if (is.call(node)) as.call(values) else if (is.expression(node)) as.expression(values) else as.pairlist(values)
            attributes(rebuilt) <- attributes(node)
            node <- rebuilt
            dependencies <- c(dependencies, unlist(lapply(nodes[kids], attr, "member_type_names", exact = TRUE), use.names = FALSE))
        }
        hint <- plan[i, 3L]
        if (hint) {
            attr(node, "member_type_hints") <- index[[hint]]
            dependencies <- c(member_type_dependencies(index[[hint]]), dependencies)
        }
        attr(node, "member_type_names") <- unique(dependencies)
        nodes[[i]] <- node
    }
    nodes[[1L]]
}
