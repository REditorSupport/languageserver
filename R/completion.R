# TODO: group the completions into different catagories according to
# https://github.com/wch/r-source/blob/trunk/src/library/utils/R/completion.R

CompletionItemKind <- list(
    Text = 1,
    Method = 2,
    Function = 3,
    Constructor = 4,
    Field = 5,
    Variable = 6,
    Class = 7,
    Interface = 8,
    Module = 9,
    Property = 10,
    Unit = 11,
    Value = 12,
    Enum = 13,
    Keyword = 14,
    Snippet = 15,
    Color = 16,
    File = 17,
    Reference = 18,
    Folder = 19,
    EnumMember = 20,
    Constant = 21,
    Struct = 22,
    Event = 23,
    Operator = 24,
    TypeParameter = 25
)

InsertTextFormat <- list(
    PlainText = 1,
    Snippet = 2
)

quote_completion_name <- function(name) {
    if (identical(make.names(name), name)) name else encodeString(name, quote = "`")
}

escape_completion_snippet <- function(text) {
    text <- gsub("\\", "\\\\", text, fixed = TRUE)
    text <- gsub("$", "\\$", text, fixed = TRUE)
    gsub("}", "\\}", text, fixed = TRUE)
}

sort_prefixes <- list(
    arg = "0-",
    scope = "1-",
    workspace = "2-",
    imported = "3-",
    global = "4-",
    token = "5-"
)

constants <- c("TRUE", "FALSE", "NULL",
    "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_",
    "Inf", "NaN")

#' Build the symbol indexes used by completion providers
#'
#' This runs with the document parser so completion requests do not have to
#' repeatedly walk the complete XML parse tree. All values stored here are
#' ordinary R vectors and can therefore be returned by the parse subprocess.
#' @noRd
completion_parse_data <- function(data) {
    empty_scope <- function() {
        list(
            name = character(),
            line = integer(),
            definition_line1 = integer(),
            definition_col1 = integer(),
            definition_line2 = integer(),
            definition_col2 = integer(),
            line1 = integer(),
            col1 = integer(),
            line2 = integer(),
            col2 = integer()
        )
    }

    if (is.null(data) || !nrow(data)) {
        return(list(
            tokens = character(),
            empty_tokens = character(),
            symbols = empty_scope(),
            functions = empty_scope(),
            formals = empty_scope()
        ))
    }

    index <- .Call(
        "completion_parse_index_c",
        PACKAGE = "languageserver",
        data$id,
        data$parent,
        data$token,
        data$line1,
        data$col1,
        data$line2,
        data$col2
    )

    make_records <- function(name_rows, range_rows) {
        if (!length(name_rows)) {
            return(empty_scope())
        }
        list(
            name = data$text[name_rows],
            line = data$line1[name_rows],
            definition_line1 = data$line1[name_rows],
            definition_col1 = data$col1[name_rows],
            definition_line2 = data$line2[name_rows],
            definition_col2 = data$col2[name_rows],
            line1 = data$line1[range_rows],
            col1 = data$col1[range_rows],
            line2 = data$line2[range_rows],
            col2 = data$col2[range_rows]
        )
    }

    list(
        tokens = unique(data$text[index$token]),
        empty_tokens = unique(data$text[index$empty_token]),
        symbols = make_records(index$symbol_name, index$symbol_range),
        functions = make_records(index$function_name, index$function_range),
        formals = make_records(index$formal_name, index$formal_range)
    )
}

#' Complete language constants
#' @noRd
constant_completion <- function(token) {
    consts <- constants[match_with(constants, token)]
    completions <- lapply(consts, function(const) {
        list(label = const,
            kind = CompletionItemKind$Constant,
            sortText = paste0(sort_prefixes$global, const),
            data = list(type = "constant")
        )
    })
}

#' Complete a package name
#' @noRd
installed_package_names <- local({
    previous_paths <- NULL
    previous_stamp <- NULL
    packages <- character()
    function(lib_paths = .libPaths()) {
        # Installing/removing packages changes the library directory. Avoid
        # checking metadata inside every package directory on each request.
        stamp <- file.info(lib_paths, extra_cols = FALSE)[c("mtime", "ctime")]
        if (!identical(lib_paths, previous_paths) ||
                !identical(stamp, previous_stamp)) {
            packages <<- .packages(all.available = TRUE, lib.loc = lib_paths)
            previous_paths <<- lib_paths
            previous_stamp <<- stamp
        }
        packages
    }
})

#' @noRd
package_completion <- function(token) {
    installed_packages <- installed_package_names()
    token_packages <- installed_packages[match_with(installed_packages, token)]
    completions <- lapply(token_packages, function(package) {
        list(label = package,
            kind = CompletionItemKind$Module,
            sortText = paste0(sort_prefixes$global, package),
            data = list(type = "package")
        )
    })
    completions
}

#' Extract string values from a default argument expression
#' @param default_expr the default value expression from formals()
#' @return character vector of values, or NULL if not applicable
#' @noRd
extract_default_values <- function(default_expr) {
    # If missing, no default value
    if (missing(default_expr) || is.name(default_expr) && as.character(default_expr) == "") {
        return(NULL)
    }
    
    # If it's a call to c(), extract the arguments
    if (is.call(default_expr) && length(default_expr) > 1) {
        func_name <- as.character(default_expr[[1]])
        
        if (identical(func_name, "c")) {
            # Extract all arguments to c()
            values <- character(0)
            for (i in seq(2, length(default_expr))) {
                arg <- default_expr[[i]]
                # Only handle character literals
                if (is.character(arg)) {
                    values <- c(values, arg)
                } else if (is.call(arg) && as.character(arg[[1]]) %in% c("I")) {
                    # Handle I("value")
                    if (length(arg) > 1 && is.character(arg[[2]])) {
                        values <- c(values, arg[[2]])
                    }
                }
            }
            if (length(values) > 0) {
                return(values)
            }
        }
    }
    
    # If it's a simple string, return it
    if (is.character(default_expr) && length(default_expr) == 1) {
        return(default_expr)
    }
    
    NULL
}

#' Complete argument values based on default parameter values
#' @noRd
argument_value_completion <- function(workspace, funct, package, arg_name, token,
    exported_only = TRUE, formals_list = NULL, uri = NULL) {
    # Reuse formals already resolved by the caller when completing multiple
    # arguments from the same function.
    if (is.null(formals_list)) {
        formals_list <- if (is.null(uri)) {
            workspace$get_formals(
                funct, package, exported_only = exported_only)
        } else {
            call_with_optional_uri(
                workspace$get_formals,
                funct, package, exported_only = exported_only, uri = uri)
        }
    }
    
    if (is.null(formals_list) || !is.list(formals_list)) {
        return(list())
    }
    
    # Get the default value for the specific argument
    if (!arg_name %in% names(formals_list)) {
        return(list())
    }
    
    default_value <- formals_list[[arg_name]]
    
    # Extract possible values from the default
    values <- extract_default_values(default_value)
    
    if (is.null(values) || length(values) == 0) {
        return(list())
    }
    
    # Filter values that match the token
    matching_values <- values[match_with(values, token)]
    
    # Create completion items
    completions <- lapply(matching_values, function(value) {
        list(
            label = value,
            kind = CompletionItemKind$Value,
            detail = paste0("value for ", arg_name),
            sortText = paste0(sort_prefixes$arg, value),
            insertText = sprintf('"%s"', value),
            insertTextFormat = InsertTextFormat$PlainText,
            data = list(
                type = "argument_value",
                funct = funct,
                package = package,
                argument = arg_name,
                context_uri = uri
            )
        )
    })
    
    completions
}

#' Complete argument values based on function call context
#' @noRd
arg_value_completion <- function(uri, workspace, document, point, token, funct, package = NULL, exported_only = TRUE) {
    # Get the package context
    package_for_call <- package
    if (is.null(package_for_call)) {
        package_for_call <- call_with_optional_uri(
            workspace$guess_namespace, funct, isf = TRUE, uri = uri)
    }
    
    # Try to get the formals - works with NULL package for user-defined functions
    formals_list <- call_with_optional_uri(
        workspace$get_formals,
        funct, package_for_call, exported_only = exported_only, uri = uri)
    
    if (is.null(formals_list) || !is.list(formals_list) || length(formals_list) == 0) {
        return(list())
    }
    
    # Get all parameters with character vector defaults
    param_names <- names(formals_list)
    all_completions <- list()
    
    for (param_name in param_names) {
        values <- extract_default_values(formals_list[[param_name]])
        if (!is.null(values) && length(values) > 0) {
            # Filter values that match the current token
            matching_values <- values[match_with(values, token)]
            if (length(matching_values) > 0) {
                # Generate completions for this parameter
                param_completions <- argument_value_completion(
                    workspace, funct, package_for_call, param_name, token,
                    exported_only, formals_list, uri = uri)
                all_completions <- c(all_completions, param_completions)
            }
        }
    }
    
    all_completions
}

#' Complete a function argument
#' @noRd
arg_completion <- function(uri, workspace, point, token, funct, package = NULL, exported_only = TRUE) {
    token_args <- NULL
    token_data <- NULL

    if (is.null(package)) {
        xdoc <- workspace$get_parse_data(uri)$xml_doc
        if (!is.null(xdoc)) {
            row <- point$row + 1
            col <- point$col + 1
            xpath <- glue(signature_xpath, row = row,
                token_quote = xml_single_quote(funct))
            all_defs <- xdoc_find_definitions(xdoc, row, col, funct, xpath)
            if (length(all_defs)) {
                last_def <- all_defs[[length(all_defs)]]
                func_line1 <- as.integer(xml_attr(last_def, "line1"))
                args <- xml_text(xml_find_all(last_def, "SYMBOL_FORMALS"))
                token_args <- args[match_with(args, token)]
                token_data <- list(
                    type = "parameter",
                    funct = funct,
                    uri = uri,
                    line = func_line1
                )
            }
        }

        if (is.null(token_args)) {
            package <- call_with_optional_uri(
                workspace$guess_namespace, funct, isf = TRUE, uri = uri)
        }
    }

    if (!is.null(package)) {
        args <- names(call_with_optional_uri(
            workspace$get_formals,
            funct, package, exported_only = exported_only, uri = uri))

        if (package == "base" && funct == "options") {
            args <- c(args, names(.Options))
        }

        if (is.character(args)) {
            token_args <- args[match_with(args, token)]
            token_data <- list(
                type = "parameter",
                funct = funct,
                package = package,
                context_uri = uri
            )
        }
    }

    completions <- .mapply(function(arg, sort_text) {
        # XML formals already include source backticks; namespace formals do not.
        inserted <- if (is.null(token_data$uri)) quote_completion_name(arg) else arg
        list(label = arg,
            kind = CompletionItemKind$Variable,
            detail = "parameter",
            sortText = sort_text,
            insertText = paste0(inserted, " = "),
            insertTextFormat = InsertTextFormat$PlainText,
            data = token_data
        )
    }, list(token_args, sprintf("%s%03d", sort_prefixes$arg, seq_along(token_args))), NULL)

    completions
}


ns_function_completion <- function(ns, token, exported_only, snippet_support) {
    nsname <- ns$package_name
    functs <- ns$get_symbols(want_functs = TRUE, exported_only = exported_only)
    functs <- functs[match_with(functs, token)]
    if (nsname == WORKSPACE) {
        tag <- "[workspace]"
        sort_prefix <- sort_prefixes$workspace
    } else {
        tag <- paste0("{", nsname, "}")
        sort_prefix <- sort_prefixes$global
    }
    if (isTRUE(snippet_support)) {
        completions <- lapply(functs, function(object) {
            list(label = object,
                kind = CompletionItemKind$Function,
                detail = tag,
                sortText = paste0(sort_prefix, object),
                insertText = paste0(escape_completion_snippet(quote_completion_name(object)), "($0)"),
                insertTextFormat = InsertTextFormat$Snippet,
                data = list(
                    type = "function",
                    package = nsname
            ))
        })
    } else {
        completions <- lapply(functs, function(object) {
            inserted <- quote_completion_name(object)
            item <- list(label = object,
                kind = CompletionItemKind$Function,
                detail = tag,
                sortText = paste0(sort_prefix, object),
                data = list(
                    type = "function",
                    package = nsname
            ))
            if (!identical(inserted, object)) item$insertText <- inserted
            item
        })
    }
    completions
}

imported_object_completion <- function(workspace, token, snippet_support) {
    keys <- workspace$imported_objects$keys()
    completions <- vector("list", length(keys))
    idx <- 0L
    for (object in keys) {
        if (!match_with(object, token)) {
            next
        }
        nsname <- workspace$imported_objects$get(object)
        ns <- workspace$get_namespace(nsname)
        if (is.null(ns)) {
            next
        }
        if (ns$exists_funct(object)) {
            if (isTRUE(snippet_support)) {
                item <- list(label = object,
                    kind = CompletionItemKind$Function,
                    detail = paste0("{", nsname, "}"),
                    sortText = paste0(sort_prefixes$imported, object),
                    insertText = paste0(escape_completion_snippet(quote_completion_name(object)), "($0)"),
                    insertTextFormat = InsertTextFormat$Snippet,
                    data = list(
                        type = "function",
                        package = nsname
                ))
            } else {
                inserted <- quote_completion_name(object)
                item <- list(label = object,
                    kind = CompletionItemKind$Function,
                    detail = paste0("{", nsname, "}"),
                    sortText = paste0(sort_prefixes$imported, object),
                    data = list(
                        type = "function",
                        package = nsname
                ))
                if (!identical(inserted, object)) item$insertText <- inserted
            }
            idx <- idx + 1L
            completions[[idx]] <- item
        }
    }
    if (idx == 0L) {
        return(NULL)
    }
    if (idx < length(completions)) {
        completions <- completions[seq_len(idx)]
    }
    completions
}

#' Select the best completion candidates without constructing completion items
#' @noRd
completion_select_indices <- function(labels, sort_text, token, limit) {
    if (length(labels) <= limit) {
        return(seq_along(labels))
    }
    selected <- .Call(
        "completion_select_c",
        PACKAGE = "languageserver",
        labels,
        sort_text,
        token,
        as.integer(limit)
    )
    if (is.null(selected)) {
        candidate_order <- order(
            !startsWith(labels, token), sort_text, method = "radix")
        selected <- candidate_order[seq_len(limit)]
    }
    selected
}


#' Complete any object in the workspace
#' @noRd
workspace_completion <- function(workspace, token,
    package = NULL, exported_only = TRUE, snippet_support = NULL, limit = Inf,
    uri = NULL) {
    candidates <- list()
    truncated <- FALSE
    get_namespace <- function(name) {
        if (is.null(uri)) workspace$get_namespace(name)
        else call_with_optional_uri(workspace$get_namespace, name, uri = uri)
    }

    append_candidates <- function(objects, kind, detail, sort_prefix,
        type, package, is_function = FALSE) {
        if (!length(objects)) {
            return(NULL)
        }
        selected <- NULL
        if (length(objects) > limit) {
            # Every group shares a sort prefix, so candidates outside its
            # best `limit` cannot appear in the combined best `limit` either.
            # Trim before expanding metadata or allocating sort strings.
            selected <- completion_select_indices(objects, objects, token, limit)
            objects <- objects[selected]
            truncated <<- TRUE
        }
        size <- length(objects)
        recycle <- function(value) {
            if (length(value) == 1L) {
                rep.int(value, size)
            } else if (!is.null(selected)) {
                value[selected]
            } else {
                value
            }
        }
        candidates[[length(candidates) + 1L]] <<- list(
            label = objects,
            kind = recycle(kind),
            detail = recycle(detail),
            sort_text = paste0(sort_prefix, objects),
            type = recycle(type),
            package = recycle(package),
            is_function = recycle(is_function)
        )
        NULL
    }

    if (is.null(package)) {
        loaded_packages <- if (is.null(uri) ||
                !is.function(workspace$loaded_packages_for_context)) {
            workspace$loaded_packages
        } else {
            workspace$loaded_packages_for_context(uri)
        }
        packages <- c(WORKSPACE, loaded_packages)
    } else {
        packages <- c(package)
    }

    if (is.null(package) || exported_only) {
        for (nsname in packages) {
            ns <- get_namespace(nsname)
            if (is.null(ns)) {
                next
            }
            if (nsname == WORKSPACE) {
                tag <- "[workspace]"
                sort_prefix <- sort_prefixes$workspace
            } else {
                tag <- paste0("{", nsname, "}")
                sort_prefix <- sort_prefixes$global
            }

            functs <- ns$get_symbols(want_functs = TRUE, exported_only = TRUE)
            functs <- functs[match_with(functs, token)]
            append_candidates(
                functs, CompletionItemKind$Function, tag, sort_prefix,
                "function", nsname, is_function = TRUE)

            nonfuncts <- ns$get_symbols(want_functs = FALSE, exported_only = TRUE)
            nonfuncts <- nonfuncts[match_with(nonfuncts, token)]
            append_candidates(
                nonfuncts, CompletionItemKind$Field, tag, sort_prefix,
                "nonfunction", nsname)

            lazydata <- ns$get_lazydata()
            lazydata <- lazydata[match_with(lazydata, token)]
            append_candidates(
                lazydata, CompletionItemKind$Field, tag, sort_prefix,
                "lazydata", nsname)
        }
    } else {
        ns <- get_namespace(package)
        if (!is.null(ns)) {
            tag <- paste0("{", package, "}")
            functs <- ns$get_symbols(want_functs = TRUE, exported_only = FALSE)
            functs <- functs[match_with(functs, token)]
            append_candidates(
                functs, CompletionItemKind$Function, tag, sort_prefixes$global,
                "function", package, is_function = TRUE)

            nonfuncts <- ns$get_symbols(want_functs = FALSE, exported_only = FALSE)
            nonfuncts <- nonfuncts[match_with(nonfuncts, token)]
            append_candidates(
                nonfuncts, CompletionItemKind$Field, tag, sort_prefixes$global,
                "nonfunction", package)
        }
    }

    if (is.null(package)) {
        keys <- as.character(workspace$imported_objects$keys())
        keys <- keys[match_with(keys, token)]
        imported_labels <- character(length(keys))
        imported_packages <- character(length(keys))
        imported_count <- 0L
        for (object in keys) {
            nsname <- workspace$imported_objects$get(object)
            ns <- workspace$get_namespace(nsname)
            if (!is.null(ns) && ns$exists_funct(object)) {
                imported_count <- imported_count + 1L
                imported_labels[[imported_count]] <- object
                imported_packages[[imported_count]] <- nsname
            }
        }
        if (imported_count) {
            selected <- seq_len(imported_count)
            imported_labels <- imported_labels[selected]
            imported_packages <- imported_packages[selected]
            append_candidates(
                imported_labels,
                CompletionItemKind$Function,
                paste0("{", imported_packages, "}"),
                sort_prefixes$imported,
                "function",
                imported_packages,
                is_function = TRUE
            )
        }
    }

    if (!length(candidates)) {
        return(list())
    }

    combine <- function(name) {
        unlist(lapply(candidates, "[[", name), use.names = FALSE)
    }
    labels <- combine("label")
    sort_text <- combine("sort_text")
    truncated <- truncated || length(labels) > limit
    selected <- completion_select_indices(labels, sort_text, token, limit)

    labels <- labels[selected]
    kinds <- combine("kind")[selected]
    details <- combine("detail")[selected]
    sort_text <- sort_text[selected]
    types <- combine("type")[selected]
    packages <- combine("package")[selected]
    functions <- combine("is_function")[selected]

    completions <- unname(Map(function(label, kind, detail, sort_text,
        type, package, is_function) {
        inserted <- quote_completion_name(label)
        data <- list(type = type, package = package)
        if (!is.null(uri)) data$context_uri <- uri
        if (isTRUE(snippet_support) && is_function) {
            list(
                label = label,
                kind = kind,
                detail = detail,
                sortText = sort_text,
                insertText = paste0(escape_completion_snippet(inserted), "($0)"),
                insertTextFormat = InsertTextFormat$Snippet,
                data = data
            )
        } else {
            item <- list(
                label = label,
                kind = kind,
                detail = detail,
                sortText = sort_text,
                data = data
            )
            if (!identical(inserted, label)) item$insertText <- inserted
            item
        }
    }, labels, kinds, details, sort_text, types, packages, functions))

    if (truncated) {
        attr(completions, "truncated") <- TRUE
    }

    completions
}

scope_completion_symbols_xpath <- paste(
    "(* | descendant-or-self::expr | descendant-or-self::expr_or_assign_or_help)[self::FUNCTION or self::OP-LAMBDA]/following-sibling::SYMBOL_FORMALS",
    "(* | descendant-or-self::expr | descendant-or-self::expr_or_assign_or_help)/LEFT_ASSIGN[not(following-sibling::expr/*[self::FUNCTION or self::OP-LAMBDA])]/preceding-sibling::expr[count(*)=1]/SYMBOL",
    "(* | descendant-or-self::expr | descendant-or-self::expr_or_assign_or_help)/RIGHT_ASSIGN[not(preceding-sibling::expr/*[self::FUNCTION or self::OP-LAMBDA])]/following-sibling::expr[count(*)=1]/SYMBOL",
    "(* | descendant-or-self::expr | descendant-or-self::expr_or_assign_or_help)/EQ_ASSIGN[not(following-sibling::expr/*[self::FUNCTION or self::OP-LAMBDA])]/preceding-sibling::expr[count(*)=1]/SYMBOL",
    "forcond/SYMBOL",
    sep = "|")

scope_completion_functs_xpath <- paste(
    "(* | descendant-or-self::expr | descendant-or-self::expr_or_assign_or_help)/LEFT_ASSIGN[following-sibling::expr/*[self::FUNCTION or self::OP-LAMBDA]]/preceding-sibling::expr[count(*)=1]/SYMBOL",
    "(* | descendant-or-self::expr | descendant-or-self::expr_or_assign_or_help)/RIGHT_ASSIGN[preceding-sibling::expr/*[self::FUNCTION or self::OP-LAMBDA]]/following-sibling::expr[count(*)=1]/SYMBOL",
    "(* | descendant-or-self::expr | descendant-or-self::expr_or_assign_or_help)/EQ_ASSIGN[following-sibling::expr/*[self::FUNCTION or self::OP-LAMBDA]]/preceding-sibling::expr[count(*)=1]/SYMBOL",
    sep = "|")

#' Complete bindings supplied by the enclosing execution context
#' @noRd
context_scope_completion <- function(uri, workspace, token, point,
    snippet_support, limit, exclude) {
    document <- if (!is.null(workspace$documents)) workspace$documents$get(uri, NULL)
    if (is.null(document) || !check_scope(uri, document, point)) return(list())
    if (!is.null(member_cursor(document, point))) return(list())
    semantic <- document$parse_data$semantic_data
    col <- document$to_lsp_position(point$row, point$col)$character
    excluded <- semantic$lines == point$row & semantic$cols <= col &
        ((semantic$types == SemanticTokenTypes$comment & semantic$cols + semantic$lengths >= col) |
                (semantic$types == SemanticTokenTypes$string & semantic$cols + semantic$lengths > col))
    if (any(excluded)) return(list())
    token_data <- completion_token(document, point)
    if (!is.null(token_data$package) || token_data$accessor %in% c("$", "@")) return(list())
    start <- token_data$range$start$col
    rest <- substring(document$line0(point$row), point$col + 1L)
    suffix <- regexpr("^[[:alnum:]_.]*", rest, perl = TRUE)
    end <- if (isTRUE(token_data$quoted)) {
        token_data$range$end$col
    } else {
        point$col + attr(suffix, "match.length")[[1L]]
    }
    cursor <- list(
        operator = start + 1L, operator_row = point$row,
        start = start, end = end,
        accessor = NULL, before = substr(document$line0(point$row), 1L, start)
    )
    resolved <- tryCatch(member_resolve_cursor(uri, workspace, document, point, cursor),
        error = function(e) {
            logger$info("context scope completion:", e)
            NULL
        })
    if (is.null(resolved$value)) return(list())
    fields <- resolved$value$fields
    labels <- names(fields)
    # Indexed definitions include source backticks; inferred bindings do not.
    exclude <- vapply(exclude, function(name) {
        if (!startsWith(name, "`")) return(name)
        tryCatch(as.character(parse(text = name)[[1L]]), error = function(e) name)
    }, character(1L))
    # Source-indexed candidates still need inferred class information. Resolve
    # a bounded set in the same lexical environment, so aliases and shadowing
    # agree with hover rather than falling back to a global binding.
    candidates <- exclude[match_with(exclude, token)]
    selected <- completion_select_indices(candidates, candidates, token, limit)
    class_info <- list()
    for (label in candidates[selected]) {
        if (isTRUE(resolved$budget$exhausted)) break
        value <- if (label %in% names(fields)) {
            fields[[label]]
        } else {
            member_infer(as.name(label), resolved$index, resolved$bindings, budget = resolved$budget)
        }
        description <- member_r6_description(value)
        if (!is.null(description)) class_info[[label]] <- description
    }
    # R6 execution-context bindings hide enclosing declarations. Retain them
    # here even if the source index found an outer symbol with the same name.
    context_names <- resolved$value$context_bindings
    labels <- setdiff(labels[match_with(labels, token)], setdiff(exclude, context_names))
    selected <- completion_select_indices(labels, labels, token, limit)
    items <- lapply(labels[selected], function(label) {
        value <- fields[[label]]
        is_function <- !is.null(value$function_expr) || !is.null(value$function_key) ||
            "function" %in% value$type
        inserted <- quote_completion_name(label)
        description <- member_r6_description(value)
        item <- list(
            label = label,
            kind = if (is_function) CompletionItemKind$Function else CompletionItemKind$Field,
            detail = paste(c("[scope]", if (!is.null(description)) gsub("`", "", description, fixed = TRUE)), collapse = " "),
            documentation = if (!is.null(description)) list(kind = "markdown", value = description),
            sortText = paste0(sort_prefixes$scope, label),
            data = list(type = "scope", context_uri = uri)
        )
        if (is_function && isTRUE(snippet_support)) {
            item$insertText <- paste0(escape_completion_snippet(inserted), "($0)")
            item$insertTextFormat <- InsertTextFormat$Snippet
        } else if (!identical(inserted, label)) {
            item$insertText <- inserted
        }
        item
    })
    attr(items, "class_info") <- class_info
    attr(items, "member_context") <- resolved
    if (length(labels) > limit || resolved$budget$exhausted) attr(items, "truncated") <- TRUE
    items
}

scope_completion <- function(uri, workspace, token, point,
    snippet_support = NULL, limit = Inf) {
    parse_data <- workspace$get_parse_data(uri)
    completion_data <- parse_data$completion_data
    row <- point$row + 1L
    col <- point$col + 1L

    contains_point <- function(records) {
        (records$line1 < row |
                records$line1 == row & records$col1 <= col) &
            (records$line2 > row |
                    records$line2 == row & records$col2 >= col - 1L)
    }

    if (!is.null(completion_data)) {
        symbol_selector <- contains_point(completion_data$symbols)
        formal_selector <- contains_point(completion_data$formals)
        scope_symbol_names <- c(
            completion_data$symbols$name[symbol_selector],
            completion_data$formals$name[formal_selector]
        )
        scope_symbol_lines <- c(
            completion_data$symbols$line[symbol_selector],
            completion_data$formals$line[formal_selector]
        )

        function_selector <- contains_point(completion_data$functions)
        scope_funct_names <- completion_data$functions$name[function_selector]
        scope_funct_lines <- completion_data$functions$line[function_selector]
    } else {
        xdoc <- parse_data$xml_doc
        if (is.null(xdoc)) {
            return(context_scope_completion(
                uri, workspace, token, point, snippet_support, limit, character()))
        }
        enclosing_scopes <- xdoc_find_enclosing_scopes(xdoc, row, col)

        in_lexical_scope <- function(nodes) {
            vapply(nodes, function(node) {
                scope <- xml_find_first(node, "ancestor::expr[FUNCTION or OP-LAMBDA][1]")
                if (inherits(scope, "xml_missing")) return(TRUE)
                contains_point(list(
                    line1 = as.integer(xml_attr(scope, "line1")),
                    col1 = as.integer(xml_attr(scope, "col1")),
                    line2 = as.integer(xml_attr(scope, "line2")),
                    col2 = as.integer(xml_attr(scope, "col2"))
                ))
            }, logical(1L))
        }
        scope_symbol_nodes <- xml_find_all(enclosing_scopes,
            scope_completion_symbols_xpath)
        scope_symbol_nodes <- scope_symbol_nodes[in_lexical_scope(scope_symbol_nodes)]
        scope_symbol_names <- xml_text(scope_symbol_nodes)
        scope_symbol_lines <- as.integer(xml_attr(scope_symbol_nodes, "line1"))

        scope_funct_nodes <- xml_find_all(enclosing_scopes,
            scope_completion_functs_xpath)
        scope_funct_nodes <- scope_funct_nodes[in_lexical_scope(scope_funct_nodes)]
        scope_funct_names <- xml_text(scope_funct_nodes)
        scope_funct_lines <- as.integer(xml_attr(scope_funct_nodes, "line1"))
    }

    scope_symbol_selector <- match_with(scope_symbol_names, token)
    scope_symbol_names <- rev(scope_symbol_names[scope_symbol_selector])
    scope_symbol_lines <- rev(scope_symbol_lines[scope_symbol_selector])
    scope_symbol_selector <- !duplicated(scope_symbol_names)

    scope_symbol_names <- scope_symbol_names[scope_symbol_selector]
    scope_symbol_lines <- scope_symbol_lines[scope_symbol_selector]

    scope_funct_selector <- match_with(scope_funct_names, token)
    scope_funct_names <- rev(scope_funct_names[scope_funct_selector])
    scope_funct_lines <- rev(scope_funct_lines[scope_funct_selector])
    scope_funct_selector <- !duplicated(scope_funct_names)

    scope_funct_names <- scope_funct_names[scope_funct_selector]
    scope_funct_lines <- scope_funct_lines[scope_funct_selector]

    candidate_names <- c(scope_symbol_names, scope_funct_names)
    context_completions <- context_scope_completion(
        uri, workspace, token, point, snippet_support, limit, candidate_names)
    context <- attr(context_completions, "member_context")
    normalize_names <- function(names) {
        vapply(names, function(name) {
            if (!startsWith(name, "`")) return(name)
            tryCatch(as.character(parse(text = name)[[1L]]), error = function(e) name)
        }, character(1L))
    }
    # Drop enclosing candidates hidden by the method context. For source
    # locals, use the final lexical value to choose the function/field item;
    # this also removes duplicates when a local changes an outer binding kind.
    keep_source <- function(names, functions) {
        vapply(normalize_names(names), function(name) {
            if (name %in% context$value$context_bindings) return(FALSE)
            value <- context$value$fields[[name]]
            if (is.null(value)) return(TRUE)
            if (!length(value$type) && is.null(value$function_expr) && is.null(value$function_key)) {
                # A method-local binding hides an outer function even when
                # its value is opaque. Do not resurrect the outer call snippet.
                if (name %in% context$value$context_locals) return(!functions)
                if (!identical(value$reason, "formal")) return(TRUE)
            }
            is_function <- !is.null(value$function_expr) || !is.null(value$function_key) ||
                "function" %in% value$type
            identical(is_function, functions)
        }, logical(1L))
    }
    keep <- keep_source(scope_symbol_names, FALSE)
    scope_symbol_names <- scope_symbol_names[keep]
    scope_symbol_lines <- scope_symbol_lines[keep]
    keep <- keep_source(scope_funct_names, TRUE)
    scope_funct_names <- scope_funct_names[keep]
    scope_funct_lines <- scope_funct_lines[keep]
    candidate_names <- c(scope_symbol_names, scope_funct_names)
    truncated <- length(candidate_names) > limit
    if (truncated) {
        keep <- completion_select_indices(
            candidate_names, candidate_names, token, limit)
        symbol_keep <- keep[keep <= length(scope_symbol_names)]
        function_keep <- keep[keep > length(scope_symbol_names)] -
            length(scope_symbol_names)
        scope_symbol_names <- scope_symbol_names[symbol_keep]
        scope_symbol_lines <- scope_symbol_lines[symbol_keep]
        scope_funct_names <- scope_funct_names[function_keep]
        scope_funct_lines <- scope_funct_lines[function_keep]
    }

    scope_symbol_completions <- .mapply(function(symbol, line) {
        list(
            label = symbol,
            kind = CompletionItemKind$Field,
            sortText = paste0(sort_prefixes$scope, symbol),
            detail = "[scope]",
            data = list(
                type = "nonfunction",
                uri = uri,
                line = line
            )
        )
    }, list(scope_symbol_names, scope_symbol_lines), NULL)

    if (isTRUE(snippet_support)) {
        scope_funct_completions <- .mapply(function(symbol, line) {
            list(
                label = symbol,
                kind = CompletionItemKind$Function,
                detail = "[scope]",
                sortText = paste0(sort_prefixes$scope, symbol),
                insertText = paste0(escape_completion_snippet(symbol), "($0)"),
                insertTextFormat = InsertTextFormat$Snippet,
                data = list(
                    type = "function",
                    uri = uri,
                    line = line
                )
            )
        }, list(scope_funct_names, scope_funct_lines), NULL)
    } else {
        scope_funct_completions <- .mapply(function(symbol, line) {
            list(
                label = symbol,
                kind = CompletionItemKind$Function,
                sortText = paste0(sort_prefixes$scope, symbol),
                detail = "[scope]",
                data = list(
                    type = "function",
                    uri = uri,
                    line = line
                )
            )
        }, list(scope_funct_names, scope_funct_lines), NULL)
    }

    completions <- c(scope_symbol_completions, scope_funct_completions, context_completions)
    class_info <- attr(context_completions, "class_info")
    for (i in seq_along(completions)) {
        label <- completions[[i]]$label
        if (startsWith(label, "`")) label <- tryCatch(as.character(parse(text = label)[[1L]]), error = function(e) label)
        description <- class_info[[label]]
        if (!is.null(description)) {
            completions[[i]]$detail <- paste("[scope]", gsub("`", "", description, fixed = TRUE))
            completions[[i]]$documentation <- list(kind = "markdown", value = description)
        }
    }
    truncated <- truncated || isTRUE(attr(context_completions, "truncated")) ||
        length(completions) > limit
    if (length(completions) > limit) {
        labels <- vapply(completions, `[[`, character(1L), "label")
        sort_text <- vapply(completions, `[[`, character(1L), "sortText")
        completions <- completions[completion_select_indices(labels, sort_text, token, limit)]
    }
    if (truncated) {
        attr(completions, "truncated") <- TRUE
    }
    attr(completions, "member_context") <- context
    completions
}

token_completion <- function(uri, workspace, token, exclude = NULL, limit = Inf) {
    parse_data <- workspace$get_parse_data(uri)
    completion_data <- parse_data$completion_data
    if (!is.null(completion_data)) {
        symbols <- if (nzchar(token)) {
            completion_data$tokens
        } else {
            completion_data$empty_tokens
        }
        symbols <- symbols[startsWith(symbols, token)]
    } else {
        xdoc <- parse_data$xml_doc
        if (is.null(xdoc)) {
            return(list())
        }

        token_quote <- xml_single_quote(token)

        symbols <- xml_text(xml_find_all(
            xdoc,
            glue(
                "//*[
                    (self::SYMBOL[preceding-sibling::OP-DOLLAR] or self::SYMBOL_SUB) and
                    starts-with(text(),'{token_quote}')]",
                token_quote = token_quote
            )
        ))

        if (nzchar(token)) {
            symbols <- c(symbols, xml_text(xml_find_all(
                xdoc,
                glue(
                    "//*[(self::SYMBOL or self::SYMBOL_SUB or self::SYMBOL_FORMALS or self::SYMBOL_FUNCTION_CALL) and
                        starts-with(text(),'{token_quote}')]",
                    token_quote = token_quote
                )
            )))
        }
    }

    symbols <- setdiff(symbols, exclude)
    truncated <- length(symbols) > limit
    if (truncated) {
        selected <- completion_select_indices(symbols, symbols, token, limit)
        symbols <- symbols[selected]
    }
    completions <- lapply(symbols, function(symbol) {
        list(
            label = symbol,
            kind = CompletionItemKind$Text,
            sortText = paste0(sort_prefixes$token, symbol)
        )
    })
    if (truncated) {
        attr(completions, "truncated") <- TRUE
    }
    completions
}

completion_token <- function(document, point) {
    result <- document$detect_token(point, forward = FALSE)
    line <- document$line0(point$row)
    quoted <- .Call("scan_backtick_completion_c", line, as.integer(point$col),
        PACKAGE = "languageserver")
    if (is.null(quoted)) return(result)

    before <- substr(line, 1L, quoted$start)
    # Member providers handle their own quoted names and replacement ranges.
    if (grepl("[$@][ \\t]*$", before)) return(result)
    namespace <- document$detect_token(
        list(row = point$row, col = quoted$start), forward = FALSE)
    result$package <- namespace$package
    result$accessor <- namespace$accessor
    result$full_token <- paste0("`", quoted$token)
    result$token <- tryCatch(
        as.character(parse(text = paste0("`", quoted$token, "`"))[[1L]]),
        error = function(e) quoted$token
    )
    result$range <- list(
        start = list(row = point$row, col = quoted$start),
        end = list(row = point$row, col = quoted$end)
    )
    result$quoted <- TRUE
    result
}

completion_has_call_parens <- function(document, token_result) {
    line_text <- document$line0(token_result$range$end$row)
    rest_of_line <- substr(
        line_text,
        token_result$range$end$col + 1L,
        nchar(line_text)
    )
    startsWith(trimws(rest_of_line), "(")
}

#' The response to a textDocument/completion request
#' @noRd
completion_reply <- function(id, uri, workspace, document, point, capabilities) {
    if (!check_scope(uri, document, point) && is.null(member_cursor(document, point))) {
        return(Response$new(
            id,
            result = list(
                isIncomplete = FALSE,
                items = list()
            )))
    }

    t0 <- Sys.time()
    snippet_support <- isTRUE(capabilities$completionItem$snippetSupport) &&
        lsp_settings$get("snippet_support")
    nmax <- lsp_settings$get("max_completions")
    if (isTRUE(lsp_settings$get("member_completion"))) {
        members <- tryCatch(member_completion(uri, workspace, document, point,
            snippet_support, nmax), error = function(e) {
                logger$info("static member completion:", e)
                NULL
            })
        if (!is.null(members)) return(Response$new(id, result = list(
            isIncomplete = isTRUE(attr(members, "truncated")), items = unname(members))))
    }

    token_result <- completion_token(document, point)

    full_token <- token_result$full_token
    token <- token_result$token
    package <- token_result$package
    function_snippet_support <- snippet_support &&
        !completion_has_call_parens(document, token_result)

    completions <- list()
    providers_incomplete <- FALSE
    member_context <- FALSE

    if (is.null(package)) {
        scope_completions <- scope_completion(uri, workspace, token, point,
            function_snippet_support, nmax)
        member_context <- attr(scope_completions, "member_context")
        if (is.null(member_context)) member_context <- FALSE
        providers_incomplete <- providers_incomplete ||
            isTRUE(attr(scope_completions, "truncated"))
        completions <- c(completions, scope_completions)
    }
    if (nzchar(full_token)) {
        if (is.null(package)) {
            completions <- c(completions, constant_completion(token), package_completion(token))
        }
        workspace_completions <- workspace_completion(
            workspace, token, package, token_result$accessor == "::",
            function_snippet_support, nmax, uri = uri)
        providers_incomplete <- providers_incomplete ||
            isTRUE(attr(workspace_completions, "truncated"))
        completions <- c(completions, workspace_completions)
    }

    if (token_result$accessor == "") {
        call_result <- document$detect_call(point)
        if (!is.null(call_result$opening)) {
            constructor_args <- if (isTRUE(lsp_settings$get("member_completion"))) {
                member_constructor_arguments(uri, workspace, document, point, token)
            }
            completions <- c(
                completions,
                if (!is.null(constructor_args)) constructor_args else if (nzchar(call_result$token)) arg_completion(uri, workspace, point, token,
                    call_result$token, call_result$package,
                    exported_only = call_result$accessor != ":::"),
                if (is.null(constructor_args) && nzchar(call_result$token)) arg_value_completion(uri, workspace, document, point, token,
                    call_result$token, call_result$package,
                    exported_only = call_result$accessor != ":::"))
        }
    }

    if (is.null(token_result$package)) {
        existing_symbols <- vapply(completions, "[[", character(1), "label")
        token_completions <- token_completion(
            uri, workspace, token, existing_symbols, nmax)
        providers_incomplete <- providers_incomplete ||
            isTRUE(attr(token_completions, "truncated"))
        completions <- c(
            completions,
            token_completions
        )
    }

    init_count <- length(completions)

    if (providers_incomplete || init_count > nmax) {
        isIncomplete <- TRUE
        label_text <- vapply(completions, "[[", character(1), "label")
        sort_text <- vapply(completions, "[[", character(1), "sortText")
        selected <- completion_select_indices(
            label_text, sort_text, token, nmax)
        completions <- completions[selected]
    } else {
        isIncomplete <- FALSE
    }

    if (isTRUE(lsp_settings$get("member_completion"))) {
        completions <- member_class_completion_info(uri, workspace, document, point, token_result, completions,
            resolved = member_context)
    }

    if (isTRUE(token_result$quoted)) {
        edit_range <- range(
            document$to_lsp_position(point$row, token_result$range$start$col),
            document$to_lsp_position(point$row, token_result$range$end$col)
        )
        completions <- lapply(completions, function(item) {
            inserted <- if (is.null(item$insertText)) item$label else item$insertText
            item$textEdit <- text_edit(edit_range, inserted)
            item$filterText <- if (is.null(item$data$package) && startsWith(item$label, "`")) {
                item$label
            } else {
                encodeString(item$label, quote = "`")
            }
            item
        })
    }

    t1 <- Sys.time()

    logger$info("completions: ", list(
        init_count = init_count,
        final_count = length(completions),
        time = as.numeric(t1 - t0),
        isIncomplete = isIncomplete
    ))

    Response$new(
        id,
        result = list(
            isIncomplete = isIncomplete,
            items = completions
        )
    )
}

#' The response to a completionItem/resolve request
#' @noRd
completion_item_resolve_reply <- function(id, workspace, params, capabilities) {
    resolved <- FALSE
    if (is.null(params$data) || is.null(params$data$type)) {
    } else {
        if (params$data$type == "member") {
            if (isTRUE(capabilities$completionItem$labelDetailsSupport) &&
                    !is.null(params$data$signature) && is.null(params$data$parameter)) params$labelDetails <- list(
                detail = substring(params$data$signature, nchar(params$label) + 1L))
            if (!is.null(params$data$package) && is.character(params$data$function_id) &&
                    length(params$data$function_id) == 1L && !is.na(params$data$function_id)) {
                doc <- workspace$get_documentation(params$data$function_id,
                    params$data$package, isf = TRUE, uri = params$data$context_uri)
                if (is.list(doc) && !is.null(doc$description)) params$documentation <- list(
                    kind = "markdown", value = paste(c(params$documentation$value, doc$description), collapse = "\n\n"))
            }
            resolved <- TRUE
        } else if (params$data$type == "package") {
            if (length(find.package(params$label, quiet = TRUE))) {
                desc <- utils::packageDescription(params$label, fields = c("Title", "Description"))
                description <- gsub("\\s*\n\\s*", " ", desc$Description)
                params$documentation <- list(
                    kind = "markdown",
                    value = sprintf("**%s**\n\n%s", desc$Title, description)
                )
                resolved <- TRUE
            }
        } else if (params$data$type == "parameter") {
            doc <- NULL
            doc_string <- NULL
            if (is.null(params$data$uri)) {
                doc <- call_with_optional_uri(
                    workspace$get_documentation,
                    params$data$funct, params$data$package, isf = TRUE,
                    uri = params$data$context_uri)
            } else {
                document <- workspace$documents$get(params$data$uri)
                func_line1 <- params$data$line
                doc_line1 <- detect_comments(document$content, func_line1 - 1) + 1
                if (doc_line1 < func_line1) {
                    comment <- document$content[doc_line1:(func_line1 - 1)]
                    doc <- convert_comment_to_documentation(comment)
                }
            }
            if (is.list(doc)) {
                doc_string <- doc$arguments[[params$label]]
                if (!is.null(doc_string)) {
                    params$documentation <- list(kind = "markdown", value = doc_string)
                    resolved <- TRUE
                }
            }
        } else if (params$data$type %in% c("constant", "function", "nonfunction", "lazydata")) {
            if (isTRUE(capabilities$completionItem$labelDetailsSupport)) {
                if (params$data$type == "function") {
                    sig <- call_with_optional_uri(
                        workspace$get_signature,
                        params$label, params$data$package,
                        uri = params$data$context_uri)
                    if (!is.null(sig)) {
                        params$labelDetails <- list(
                            detail = substr(sig, nchar(params$label) + 1, nchar(sig))
                        )
                    }
                }
            }

            doc <- NULL
            doc_string <- NULL
            if (is.null(params$data$uri)) {
                doc <- call_with_optional_uri(
                    workspace$get_documentation,
                    params$label, params$data$package,
                    isf = params$data$type == "function",
                    uri = params$data$context_uri)
            } else {
                document <- workspace$documents$get(params$data$uri)
                token_line1 <- params$data$line
                doc_line1 <- detect_comments(document$content, token_line1 - 1) + 1
                if (doc_line1 < token_line1) {
                    comment <- document$content[doc_line1:(token_line1 - 1)]
                    doc <- convert_comment_to_documentation(comment)
                }
            }

            if (is.character(doc)) {
                doc_string <- doc
            } else if (is.list(doc)) {
                doc_string <- doc$description
            }

            if (!is.null(doc_string)) {
                params$documentation <- list(kind = "markdown",
                    value = paste(c(params$documentation$value, doc_string), collapse = "\n\n"))
                resolved <- TRUE
            }
        }
    }

    params$data <- NULL
    Response$new(
        id,
        result = params
    )
}
