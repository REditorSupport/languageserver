# Locate a host declaration in the same lexical scope as an embedded read.
# Completion records retain the scope bounds and the reference index supplies
# the identity shared by highlights, references and rename.
glue_lexical_target <- function(document, name, at, skip = character()) {
    parsed <- document$parse_data
    index <- parsed$reference_index
    matches <- reference_indices(index, name)
    matches <- matches[index$is_definition[matches]]
    if (length(skip)) matches <- matches[!index$definition_key[matches] %in% skip]
    if (!length(matches)) return(NULL)
    syntax <- parsed$syntax_data
    line <- syntax$line1[[at]]
    col <- syntax$col1[[at]]
    records <- parsed$completion_data[c("symbols", "functions", "formals")]
    candidates <- lapply(records, function(group) {
        i <- which(reference_symbol_names(group$name) == name &
                (group$line1 < line | (group$line1 == line & group$col1 <= col)) &
                (group$line2 > line | (group$line2 == line & group$col2 >= col)))
        lapply(i, function(j) {
            hit <- matches[index$line[matches] == group$definition_line1[[j]] - 1L &
                    index$code_point_col[matches] == group$definition_col1[[j]] - 1L]
            if (!length(hit)) return(NULL)
            c(hit = hit[[1L]], line = group$line1[[j]], col = group$col1[[j]])
        })
    })
    locals <- do.call(rbind, unlist(candidates, recursive = FALSE))
    if (length(locals)) {
        locals <- locals[startsWith(index$definition_key[locals[, "hit"]], "local:"), , drop = FALSE]
    }
    if (length(locals)) {
        hits <- locals[, "hit"]
        order <- order(locals[, "line"], locals[, "col"], index$line[hits], index$col[hits], decreasing = TRUE)
        selected <- hits[order[[1L]]]
    } else {
        globals <- matches[startsWith(index$definition_key[matches], "global:")]
        if (!length(globals)) return(NULL)
        previous <- globals[index$line[globals] < line - 1L |
                (index$line[globals] == line - 1L & index$code_point_col[globals] < col - 1L)]
        selected <- if (length(previous)) utils::tail(previous, 1L) else globals[[1L]]
    }
    row <- member_navigation_token(parsed, list(row = index$line[[selected]], col = index$code_point_col[[selected]]))
    nav <- parsed$navigation_data
    assignments <- get0(name, nav$by_name, inherits = FALSE, ifnotfound = integer())
    assignment <- assignments[nav$name[assignments] == row]
    value <- if (length(assignment)) nav$value[[assignment[[1L]]]] else 0L
    list(kind = "lexical", name = name, uri = document$uri, row = row, value_row = value,
        range = indexed_reference_locations(index, document$uri, selected)[[1L]]$range,
        key = index$definition_key[[selected]], formal = identical(index$definition_kind[[selected]], "formal"),
        writable = TRUE)
}

# A shallow workspace index can identify a definition before the owner file's
# full syntax is available. Refine to its declaration token after it is parsed.
glue_workspace_target <- function(workspace, document, name) {
    loc <- if (is.function(workspace$get_definition)) {
        call_with_optional_uri(workspace$get_definition, name, WORKSPACE, uri = document$uri)
    } else if (!is.null(workspace$documents)) {
        for (uri in setdiff(workspace_document_uris(workspace, document$uri), document$uri)) {
            definition <- workspace$documents$get(uri, NULL)$parse_data$definitions[[name]]
            if (!is.null(definition$range)) break
        }
        if (exists("definition", inherits = FALSE) && !is.null(definition$range)) location(uri, definition$range)
    }
    if (is.null(loc$range) || !is.null(attr(loc, "namespace", exact = TRUE))) return(NULL)
    target <- list(kind = "lexical", name = name, uri = loc$uri, range = loc$range,
        key = paste0("global:", name), writable = member_navigation_writable(workspace, loc))
    owner <- workspace$documents$get(loc$uri, NULL)
    if (is.null(owner$parse_data$syntax_data)) return(target)
    pos <- owner$from_lsp_position(loc$range$start)
    row <- member_navigation_token(owner$parse_data, pos)
    if (row) {
        local <- glue_lexical_target(owner, name, row)
        if (identical(local$key, target$key)) return(local)
    }
    target
}

glue_tag_target <- function(document, kind, name, row, value, writable) {
    selection <- navigation_range(document, document$parse_data$syntax_data, row)
    list(kind = kind, name = name, uri = document$uri, range = selection, row = row, value_row = value,
        key = paste("glue", kind, document$uri, selection$start$line, selection$start$character, sep = ":"),
        writable = writable)
}

# Recognize fields with an actual declaration site. Unresolved inputs leave
# the mask open because they may shadow a name from the caller.
glue_data_fields <- function(workspace, document, row, at) {
    syntax <- document$parse_data$syntax_data
    if (!length(row) || class_literal_row(syntax, row, "NULL_CONST")) return(list(open = FALSE))
    variable <- class_literal_row(syntax, row, "SYMBOL")
    if (variable) {
        name <- class_token_text(syntax$text[[variable]])
        declaration <- glue_lexical_target(document, name, at)
        if (is.null(declaration)) {
            declaration <- glue_workspace_target(workspace, document, name)
            if (!is.null(declaration$uri) && !identical(declaration$uri, document$uri)) {
                owner <- workspace$documents$get(declaration$uri, NULL)
                if (is.null(owner)) return(list(open = TRUE))
                document <- owner
                syntax <- document$parse_data$syntax_data
                at <- utils::tail(syntax$terminals, 1L)
            }
        }
        row <- declaration$value_row
        if (!length(row) || !row) return(list(open = TRUE))
        # A later subassignment may add, replace or remove fields.
        uses <- reference_indices(document$parse_data$reference_index, name, declaration$key)
        pos <- document$parse_data$reference_index
        uses <- uses[pos$line[uses] >= syntax$line1[[row]] - 1L & pos$line[uses] <= syntax$line1[[at]] - 1L]
        for (i in uses) {
            token <- member_navigation_token(document$parse_data, list(row = pos$line[[i]], col = pos$code_point_col[[i]]))
            node <- syntax$parent_row[[token]]
            while (node && syntax$order_rank[[node]] >= syntax$order_rank[[row]] &&
                    syntax$order_rank[[node]] < syntax$order_rank[[at]]) {
                if (member_replacement_rhs(syntax, node)) return(list(open = TRUE))
                node <- syntax$parent_row[[node]]
            }
        }
    }
    ctor <- syntax_call_arguments(syntax, row)
    if (!length(ctor) || !ctor$name %in% c("list", "data.frame") ||
            (!is.null(ctor$package) && !identical(ctor$package, "base"))) return(list(open = TRUE))
    if (is.null(ctor$package) && !source_base_binding(workspace, document, ctor$name,
            syntax$line1[[row]] - 1L, syntax$col1[[row]])) return(list(open = TRUE))
    keys <- names(ctor$values)
    config <- if (ctor$name == "data.frame") names(formals(base::data.frame)) else character()
    fields <- which(nzchar(keys) & !keys %in% config)
    open <- any(!nzchar(keys)) || anyDuplicated(keys[fields]) > 0L
    if (ctor$name == "data.frame") {
        # data.frame() can repair names, expand matrix/data-frame inputs, or
        # omit NULL columns. Only simple named scalar columns are certain.
        fixed <- vapply(ctor$values[fields], function(v) {
            length(v) && v && class_literal_row(syntax, v, c("NUM_CONST", "STR_CONST")) > 0L
        }, logical(1L)) & make.names(keys[fields]) == keys[fields]
        open <- open || any(!fixed)
        fields <- fields[fixed]
    }
    fields <- fields[!duplicated(keys[fields]) & !duplicated(keys[fields], fromLast = TRUE)]
    targets <- lapply(fields, function(i) {
        field <- glue_tag_target(document, "field", keys[[i]], ctor$tags[[i]], ctor$values[[i]], writable = !open)
        field$shared <- variable > 0L
        field
    })
    list(open = open, targets = stats::setNames(targets, keys[fields]))
}

glue_call_bindings <- function(workspace, document, fragment) {
    syntax <- document$parse_data$syntax_data
    row <- fragment$call_row
    if (!length(row) || !row) return(list(open = TRUE))
    call <- syntax_call_arguments(syntax, row)
    if (is.null(call) || !call$name %in% names(glue_template_packages)) return(list(open = TRUE))
    if (is.null(call$package)) {
        owner <- if (is.function(workspace$guess_namespace)) workspace$guess_namespace(call$name, isf = TRUE, uri = document$uri)
        if (!is.null(glue_lexical_target(document, call$name, row))) {
            target <- glue_call_target(workspace, document, list(call_row = row), call$name, list(open = FALSE))
            if (length(target$callable)) target <- target$callable
            if (!is.null(target) && (!identical(target$kind, "package") || !identical(target$name, call$name))) {
                return(list(open = TRUE, shadowed = TRUE))
            }
            owner <- target$package
        }
        if (length(owner) && !identical(owner, glue_template_packages[[call$name]])) return(list(open = TRUE, shadowed = TRUE))
    }
    piped <- syntax_pipe_arguments(syntax, row, call)
    keys <- names(call$values)
    before <- fragment$before
    if (is.null(before)) before <- Inf
    control <- c(".sep", ".envir", ".na", ".trim", ".null", ".open", ".close", ".literal", ".comment", ".transformer")
    args <- class_matched_arguments(piped, c(".x", "..."))
    fields <- list(open = FALSE)
    if (endsWith(call$name, "_data")) {
        control <- c(control, ".x")
        fields <- if (length(args$.x)) glue_data_fields(workspace, document, args$.x, row) else list(open = TRUE)
    }
    tags <- which(nzchar(keys) & !keys %in% control)
    named <- lapply(tags, function(i) {
        glue_tag_target(document, "named", keys[[i]], call$tags[[i]], call$values[[i]],
            writable = syntax$token[[call$tags[[i]]]] == "SYMBOL_SUB")
    })
    names(named) <- keys[tags]
    # Each delayed named RHS sees only the preceding argument bindings.
    visible <- rev(which(tags < before))
    targets <- c(named[visible], fields$targets)
    env <- unname(call$values[keys == ".envir"])
    env_call <- if (length(env) == 1L && env) syntax_call_arguments(syntax, env)
    current_env <- identical(env_call$name, "environment") &&
        (is.null(env_call$package) || identical(env_call$package, "base")) && !length(env_call$values) &&
        source_base_binding(workspace, document, "environment", syntax$line1[[row]] - 1L, syntax$col1[[row]])
    dots <- vapply(call$values, function(v) {
        token <- class_literal_row(syntax, v, "SYMBOL")
        token && identical(syntax$text[[token]], "...")
    }, logical(1L))
    if (any(dots)) {
        # Forwarded dots can contain additional named bindings. Later explicit
        # arguments still shadow them.
        last <- max(which(dots & seq_along(dots) < before), 0L)
        if (last) targets <- named[rev(which(tags < before & tags > last))]
    }
    list(targets = targets, open = fields$open || (length(env) && !current_env) || any(dots),
        tags = tags, named = named, fields = fields$targets, call = call)
}

# R looks for a function, skipping bound non-functions in enclosing frames.
# Bindings in one frame share an identity, so skipping a local name must also
# skip older assignments to the same variable.
glue_package_function <- function(workspace, document, name, package = NULL, value_lookup = FALSE) {
    if (is.null(package) && is.function(workspace$guess_namespace)) {
        package <- workspace$guess_namespace(name, isf = !value_lookup, uri = document$uri)
    }
    if (!length(package) && exists(name, baseenv(), mode = "function", inherits = FALSE)) package <- "base"
    if (!length(package) || identical(package, WORKSPACE)) return(NULL)
    if (identical(package, "base")) {
        if (!exists(name, baseenv(), mode = "function", inherits = FALSE)) return(NULL)
    } else {
        ns <- if (is.function(workspace$get_namespace)) workspace$get_namespace(package, uri = document$uri)
        if (is.null(ns) || !ns$exists_funct(name)) return(NULL)
    }
    list(kind = "package", name = name, package = package, key = paste("package", package, name, sep = ":"), writable = FALSE)
}

# Return FALSE for a proven non-function, NULL for an uncertain value, or
# the callable source. A delayed named RHS is resolved in its earlier frame.
glue_callable_value <- function(workspace, document, fragment, target, seen = character()) {
    if (identical(target$kind, "package")) return(target)
    if (length(seen) >= 12L || !length(target$key) || target$key %in% seen) return(NULL)
    owner <- if (identical(target$uri, document$uri)) document else workspace$documents$get(target$uri, NULL)
    s <- owner$parse_data$syntax_data
    row <- target$value_row
    if (is.null(s) || !isTRUE(row > 0L)) return(NULL)
    repeat {
        parts <- syntax_children(s, row)
        parts <- parts[s$token[parts] != "COMMENT"]
        if (length(parts) != 3L || s$token[[parts[[1L]]]] != "'('" || s$token[[parts[[3L]]]] != "')'") break
        row <- parts[[2L]]
    }
    if (s$function_close[[row]]) {
        target$value_row <- row
        return(target)
    }
    if (class_literal_row(s, row, c("NUM_CONST", "STR_CONST", "NULL_CONST"))) return(FALSE)
    if (length(parts) == 3L && identical(s$token[parts], c("SYMBOL_PACKAGE", "NS_GET", "SYMBOL"))) {
        return(glue_package_function(workspace, owner, class_token_text(s$text[[parts[[3L]]]]),
                class_token_text(s$text[[parts[[1L]]]])))
    }
    alias <- class_literal_row(s, row, "SYMBOL")
    if (!alias) return(NULL)
    name <- class_token_text(s$text[[alias]])
    if (identical(target$kind, "named")) {
        call <- syntax_call_arguments(s, fragment$call_row)
        fragment$before <- match(target$row, call$tags)
        source <- glue_context_at(workspace, owner, fragment, name, function_position = FALSE)$target
    } else {
        source <- glue_lexical_target(owner, name, row)
        if (is.null(source)) source <- glue_workspace_target(workspace, owner, name)
    }
    if (is.null(source)) source <- glue_package_function(workspace, owner, name, value_lookup = TRUE)
    if (is.null(source)) return(NULL)
    glue_callable_value(workspace, owner, fragment, source, c(seen, target$key))
}

glue_call_target <- function(workspace, document, fragment, name, bindings, lexical_only = FALSE) {
    unknown <- list(kind = "unknown", name = name, writable = FALSE)
    candidates <- bindings$targets[names(bindings$targets) == name]
    skipped <- character()
    searched_workspace <- FALSE
    repeat {
        if (length(candidates)) {
            target <- candidates[[1L]]
            candidates <- candidates[-1L]
        } else {
            if (isTRUE(bindings$open)) return(unknown)
            target <- glue_lexical_target(document, name, fragment$call_row, skipped)
            if (is.null(target) && lexical_only) return(NULL)
            if (is.null(target) && !searched_workspace) {
                target <- glue_workspace_target(workspace, document, name)
                searched_workspace <- TRUE
            }
            if (is.null(target) || target$key %in% skipped) return(glue_package_function(workspace, document, name))
            skipped <- c(skipped, target$key)
        }
        value <- glue_callable_value(workspace, document, fragment, target)
        if (is.null(value)) return(unknown)
        if (!identical(value, FALSE)) {
            if (identical(value$key, target$key)) return(value)
            target$callable <- value
            return(target)
        }
    }
}

glue_context_at <- function(workspace, document, fragment, name = NULL, function_position = isTRUE(fragment$function_position)) {
    context <- glue_call_bindings(workspace, document, fragment)
    candidates <- unique(names(context$targets))
    target <- if (length(name) && nzchar(name)) context$targets[[name]]
    if (length(name) && nzchar(name) && function_position) {
        target <- glue_call_target(workspace, document, fragment, name, context)
    } else if (is.null(target) && length(name) && nzchar(name)) {
        if (isTRUE(context$open)) {
            target <- list(kind = "unknown", name = name, writable = FALSE)
        } else {
            target <- glue_lexical_target(document, name, fragment$call_row)
            if (is.null(target)) target <- glue_workspace_target(workspace, document, name)
        }
    }
    list(names = as.character(candidates), target = target, blocked = isTRUE(context$open) || identical(target$kind, "unknown"),
        shadowed = isTRUE(context$shadowed))
}
