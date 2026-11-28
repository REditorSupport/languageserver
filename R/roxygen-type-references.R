# Class hints use the scope enclosing the documented function. Resolve only
# matching spellings when references or rename is requested; nominal S3 names
# without a generator binding do not constitute lexical references.
roxygen_type_binding <- function(workspace, document, site, name, qualified, local_names) {
    parsed <- document$parse_data
    data <- parsed$member_data
    if (!qualified && name %in% local_names) {
        token <- navigation_token(parsed, site$row + 1L, site$start + 1L)
        if (!isTRUE(token > 0L)) return(NULL)
        target <- glue_lexical_target(document, name, token, preceding = TRUE)
        if (!is.null(target$key) && startsWith(target$key, "local:")) return(target)
    }

    uri <- document$uri
    view <- if (!qualified) member_workspace_view(workspace, document)
    history <- if (is.null(view$bindings)) data$bindings[[name]] else view$bindings[[name]]
    at <- if (qualified) c(Inf, Inf) else c(site$function_row, site$function_col)
    if (member_history_position(history, at)) {
        uri <- member_history_owner(history, at, uri)
        if (is.null(uri)) return(NULL)
    } else {
        if (!isTRUE(workspace$index$enabled) || is.null(data$document_package)) return(NULL)
        root <- workspace$index$package_root_for_uri(uri)
        if (is.null(root) || !workspace$index$has_definition(name, uri)) return(NULL)
        uri <- member_package_binding_owner(name, workspace, root, uri)
        if (is.null(uri) || identical(uri, document$uri)) return(NULL)
    }
    list(key = paste0("global:", name), uri = uri)
}

# A script can receive a binding from an earlier sibling source() call. Each
# verified execution leading into the script must agree on the class binding.
roxygen_inherited_generator <- function(workspace, cases, name) {
    values <- vapply(cases, function(case) {
        doc <- workspace$documents$get(case$uri)
        budget <- member_request_budget()
        index <- member_context_index(workspace, case$uri, doc, case$at, referenced = name)
        value <- member_infer(as.name(name), index, list(.__member_position__ = case$at), budget = budget)
        if (isTRUE(budget$exhausted) || isTRUE(budget$transient)) return(NA)
        member_is_class_generator(value)
    }, logical(1L))
    if (length(values) && !anyNA(values) && length(unique(values)) == 1L) values[[1L]] else NA
}

roxygen_type_references <- function(workspace, document, sites, name, key, owner_uri, for_rename, new_name, owner_scope,
    source_contexts = NULL) {
    package <- document$parse_data$member_data$document_package
    spellings <- c(name, new_name)
    if (!is.null(package)) spellings <- c(spellings, paste(package, spellings, sep = "::"))
    if (for_rename && roxygen_unresolved_type_hint(document$parse_data$member_data, spellings)) {
        return(list(problem = "Incomplete R code prevents resolving a documented type"))
    }
    if (is.null(sites)) return(NULL)
    sites <- sites[sites$type %in% spellings, , drop = FALSE]
    if (!nrow(sites)) return(NULL)
    groups <- split(seq_len(nrow(sites)), paste(sites$function_row, sites$function_col, sites$type))
    indexed <- document$parse_data$reference_index
    local_names <- Filter(function(nm) {
        rows <- reference_indices(indexed, nm)
        any(indexed$is_definition[rows] & startsWith(indexed$definition_key[rows], "local:"))
    }, c(name, new_name))
    declarations <- new.env(parent = emptyenv())
    locations <- list()
    for (group in groups) {
        site <- sites[group[[1L]], ]
        qualified <- grepl("::", site$type, fixed = TRUE)
        target <- roxygen_type_binding(workspace, document, site, name, qualified, local_names)
        inherited <- if (is.null(target) && !qualified && length(source_contexts) && startsWith(key, "global:")) {
            reference_source_hint_owner(source_contexts, document, site, name)
        }
        if (is.null(inherited)) {
            if (!identical(target$key, key) || !identical(target$uri, owner_uri)) next
            value <- roxygen_type_generator_at(workspace, document, site, name, declarations)
            generator <- if (isTRUE(value$reason %in% c("incomplete_type_hint", "type_hint_budget"))) NA else member_is_class_generator(value)
        } else {
            if (!is.null(inherited$uri) && !identical(inherited$uri, owner_uri)) next
            generator <- if (is.null(inherited$uri)) NA else roxygen_inherited_generator(workspace, inherited$cases, name)
        }
        if (for_rename && is.na(generator)) {
            return(list(problem = "Rename could not verify the binding of a documented type"))
        }
        if (!isTRUE(generator)) next
        old <- site$type == name || identical(site$type, paste(package, name, sep = "::"))
        if (!is.null(new_name)) {
            # A nearer lexical binding captures a renamed use. Conversely an
            # existing annotation for the new name stays valid in that scope.
            closer <- FALSE
            if (!qualified) {
                other <- roxygen_type_binding(workspace, document, site, new_name, FALSE, local_names)
                if (!is.null(other$key) && startsWith(other$key, "local:")) {
                    scope <- rename_binding_scopes(document$parse_data$syntax_data, other$row)
                    s <- document$parse_data$syntax_data
                    closer <- scope > 0L && (is.null(owner_scope) || !owner_scope ||
                            s$order_rank[[scope]] >= s$order_rank[[owner_scope]])
                }
            }
            if ((old && closer) || (!old && !closer)) {
                return(list(problem = "The new name would change the binding of a documented type"))
            }
            if (old && !grepl(paste0("(*UCP)^", roxygen_type_identifier, "$"), new_name, perl = TRUE)) {
                return(list(problem = "The new name cannot be written as a documented type"))
            }
        }
        if (!old) next
        for (i in group) {
            loc <- location(document$uri, range(document$to_lsp_position(sites$row[[i]], sites$start[[i]]),
                    document$to_lsp_position(sites$row[[i]], sites$end[[i]])))
            if (for_rename) attr(loc, "roxygen_type") <- TRUE
            locations[[length(locations) + 1L]] <- loc
        }
    }
    list(locations = locations)
}
