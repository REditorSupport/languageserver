# Keys select the exact call spelling. A bare registration may name a user's
# own wrapper; a package-qualified registration applies only to public :: calls.
glue_highlight_config <- local({
    cache <- new.env(parent = emptyenv())
    function(settings = lsp_settings) {
        enabled <- !identical(settings$get("glue_highlighting"), FALSE)
        rules <- settings$get("glue_highlight_functions")
        if ((!is.list(rules) && !is.atomic(rules)) || is.object(rules) || !is.null(dim(rules))) rules <- NULL
        if (identical(enabled, cache$config$enabled) && identical(rules, cache$previous)) return(cache$config)
        functions <- list()
        for (i in seq_along(names(rules))) {
            name <- names(rules)[[i]]
            if (is.na(name) || !grepl("^([[:alpha:].][[:alnum:]_.]*::)?[[:alpha:].][[:alnum:]_.]*$", name)) next
            selector <- rules[[i]]
            if (is.object(selector) || !is.null(dim(selector))) next
            if (identical(selector, FALSE) || (is.character(selector) && length(selector) == 1L &&
                        !is.na(selector) && nzchar(selector))) {
                functions[[name]] <- selector
            } else if (is.numeric(selector) && length(selector) == 1L && is.finite(selector) &&
                    selector >= 1 && selector <= .Machine$integer.max && selector == floor(selector)) {
                functions[[name]] <- as.integer(selector)
            }
        }
        if (length(functions)) functions <- functions[order(names(functions))]
        native <- names(glue_template_packages)
        disabled <- vapply(functions, identical, logical(1L), FALSE)
        cache$previous <- rules
        cache$config <- list(enabled = enabled, functions = functions,
            callees = unique(sub("^.*::", "", names(functions)[!disabled])),
            overridden = intersect(names(functions), c(native, paste(glue_template_packages, native, sep = "::"))))
        cache$config
    }
})

# Fragments are physically ordered and disjoint. Only the last one starting
# on a preceding line can reach the viewport, whereas all fragments starting
# on either boundary line can change that line's string token divisions.
glue_highlight_lines <- function(fragments, lines) {
    if (is.null(lines) || !length(fragments)) return(fragments)
    rows <- attr(fragments, "rows", exact = TRUE)
    first <- max(1L, findInterval(lines[[1L]], rows, left.open = TRUE))
    last <- findInterval(lines[[2L]], rows)
    if (first <= last && fragments[[first]]$end$row < lines[[1L]]) first <- first + 1L
    if (first > last) return(NULL)
    if (first == 1L && last == length(fragments)) return(fragments)
    i <- seq.int(first, last)
    structure(fragments[i], rows = rows[i])
}

# Map each native fragment to its exact call spelling. Whole-document calls
# already passed the parser's grammar checks; the first/last children of the
# callee give its package/name without reconstructing its argument list.
glue_highlight_native <- function(workspace, document, parsed, config, lines) {
    fragments <- glue_highlight_lines(parsed$glue_templates, lines)
    if (!length(fragments)) return(NULL)
    sites <- parsed$range_data$calls
    family <- names(glue_template_packages)
    if (is.null(lines) && !length(config$overridden) && !is.null(sites) && !isTRUE(parsed$parse_error) &&
            !length(parsed$partial_incomplete_lines) && !any(is.na(sites$package) & sites$name %in% family)) return(fragments)

    syntax <- parsed$syntax_data
    rows <- vapply(fragments, "[[", 0L, "call_row")
    name <- rep(NA_character_, length(rows))
    qualified <- logical(length(rows))
    valid <- which(rows > 0L)
    if (length(valid)) {
        head <- syntax$children[syntax$child_start[rows[valid]]]
        last <- syntax$children[syntax$child_end[head]]
        name[valid] <- syntax$text[last]
        qualified[valid] <- syntax$child_start[head] != syntax$child_end[head]
    }
    for (i in which(rows == 0L)) {
        fragment <- fragments[[i]]
        if (length(fragment$call_name)) name[[i]] <- fragment$call_name
        qualified[[i]] <- !is.null(fragment$package)
    }
    key <- name
    key[qualified] <- paste(glue_template_packages[name[qualified]], name[qualified], sep = "::")
    keep <- name %in% family & !key %in% config$overridden
    bare <- which(keep & !qualified)
    for (indices in split(bare, name[bare])) {
        nm <- name[[indices[[1L]]]]
        owner <- if (is.function(workspace$guess_namespace)) workspace$guess_namespace(nm, isf = TRUE, uri = document$uri)
        index <- parsed$reference_index
        defined <- any(index$is_definition[reference_indices(index, nm)])
        if (defined) {
            call_rows <- unique(rows[indices])
            shadowed <- vapply(call_rows, function(row) {
                # A recovery tree's row is unavailable in the accepted host.
                !row || glue_bare_call_shadowed(workspace, document, row, nm, owner)
            }, logical(1L))
            keep[indices] <- !shadowed[match(rows[indices], call_rows)]
        } else if (length(owner) && !identical(owner, glue_template_packages[[nm]])) {
            keep[indices] <- FALSE
        }
    }
    if (all(keep)) fragments else glue_order_fragments(fragments[keep], NULL)
}

glue_registered_template_calls <- function(syntax, config, lines) {
    # A backticked syntactic callee has the same R name as its unquoted form.
    spellings <- c(config$callees, paste0("`", config$callees, "`"))
    symbols <- which(syntax$token == "SYMBOL_FUNCTION_CALL" & syntax$text %in% spellings)
    heads <- syntax$parent_row[symbols]
    heads <- heads[heads > 0L]
    rows <- syntax$parent_row[heads]
    valid <- which(rows > 0L)
    if (!is.null(lines)) valid <- valid[syntax$line1[rows[valid]] <= lines[[2L]] + 1L & syntax$line2[rows[valid]] >= lines[[1L]] + 1L]
    calls <- lapply(valid, function(i) {
        head <- heads[[i]]
        row <- rows[[i]]
        call <- syntax_call_arguments(syntax, row)
        if (is.null(call)) return(NULL)
        key <- call$name
        if (!is.null(call$package)) {
            if (!"NS_GET" %in% syntax$token[syntax_children(syntax, head)]) return(NULL)
            key <- paste(call$package, key, sep = "::")
        }
        selector <- config$functions[[key]]
        if (is.null(selector) || identical(selector, FALSE)) return(NULL)
        values <- syntax_pipe_arguments(syntax, row, call)$values
        keys <- names(values)
        if (any(keys %in% c(".open", ".close", ".comment", ".literal", ".transformer"))) return(NULL)
        at <- if (is.character(selector)) which(keys == selector) else which(!nzchar(keys))[selector]
        if (length(at) != 1L || is.na(at)) return(NULL)
        value <- values[[at]]
        # Only the call's own literal may be recolored. A pipe can fill a
        # parameter but its lhs is an ordinary expression outside the call.
        if (!value %in% call$values) return(NULL)
        literal <- class_literal_row(syntax, value, "STR_CONST")
        if (!literal) return(NULL)
        if (!is.null(lines) && (syntax$line2[[literal]] < lines[[1L]] + 1L || syntax$line1[[literal]] > lines[[2L]] + 1L)) return(NULL)
        list(row = row, literals = literal, all_direct = TRUE, separator = FALSE)
    })
    Filter(Negate(is.null), calls)
}

glue_highlight_fragments <- function(workspace, document, parsed, config = glue_highlight_config(), lines = NULL) {
    if (!config$enabled) return(NULL)
    native <- glue_highlight_native(workspace, document, parsed, config, lines)
    if (!length(config$callees)) return(native)
    calls <- parsed$range_data$calls
    if (!is.null(calls)) {
        candidates <- calls$name
        if (!is.null(lines)) candidates <- candidates[calls$line <= lines[[2L]] & calls$end_line >= lines[[1L]]]
        if (!any(candidates %in% c(config$callees, paste0("`", config$callees, "`")))) return(native)
    }
    syntax <- parsed$syntax_data
    registered <- glue_registered_template_calls(syntax, config, lines)
    if (!length(registered)) return(native)
    custom <- glue_template_index(syntax, document$content, calls = registered)$fragments
    glue_highlight_lines(glue_order_fragments(c(native, custom), NULL), lines)
}
