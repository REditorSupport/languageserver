member_builtin_hint_types <- c("NULL", "logical", "integer", "numeric", "double", "complex", "character",
    "raw", "list", "pairlist", "environment", "function", "expression")

# Only class names that can come from an imported package need its model.
member_type_hint_packages <- function(parsed) {
    data <- parsed$member_data
    types <- roxygen_declared_types(data$type_hints)
    declarations <- parsed$class_data$declarations
    local <- vapply(declarations, `[[`, "", "name")
    lapply(stats::setNames(types, types), function(type) {
        parts <- strsplit(type, "::", fixed = TRUE)[[1L]]
        if (length(parts) == 2L && !identical(parts[[1L]], data$document_package)) {
            return(setdiff(parts[[1L]], "base"))
        }
        name <- utils::tail(parts, 1L)
        if (name %in% local) {
            return(if (identical(declarations[[match(name, local)]]$type, "S7")) "S7" else character())
        }
        if (name %in% member_builtin_hint_types) return(character())
        imports <- Filter(function(item) name %in% item$names, data$namespace_bindings)
        owners <- if (length(imports)) vapply(imports, `[[`, "", "package") else data$hint_import_packages
        unique(c(setdiff(parsed$packages, startup_packages), owners))
    })
}

# A current-package qualifier bypasses local variables and attached packages.
member_project_type_generator <- function(name, index, budget) {
    if (length(index$document_bindings[[name]])) {
        member_resolve_document(name, index, list(), budget, 0L, character())
    } else {
        member_package_declaration(name, index, budget, 0L, character())
    }
}

member_is_class_generator <- function(value) {
    identical(value$r6_role, "generator") || !is.null(value$refclass_instance) ||
        !is.null(value$s4_generator) || !is.null(value$s7_generator)
}

member_type_generator <- function(name, package, index, bindings, budget) {
    if (is.null(package)) return(member_infer(as.name(name), index, bindings, budget = budget))
    if (identical(package, index$document_package)) return(member_project_type_generator(name, index, budget))
    catalog <- member_lookup(index$namespace_indices, package)
    if (is.null(catalog) && identical(package, index$package)) catalog <- index
    member_lookup(catalog$roots, name)
}

# A type name describes an instance. Read generator and class metadata without
# calling constructors or inspecting the user's session.
member_hinted_value <- function(types, index, bindings, budget, class_links = attr(types, "roxygen_class_links", exact = TRUE)) {
    if (!length(types) || length(types) > max_type_hint_alternatives || !roxygen_type_hints_enabled() ||
            (length(class_links) && isTRUE(budget$explicit_hints_only))) return(NULL)
    resolve <- function(type) {
        previous <- budget$resolving_hints
        if (type %in% previous || length(previous) >= 32L || isTRUE(budget$exhausted)) {
            return(member_value(reason = "recursive_type_hint"))
        }
        budget$resolving_hints <- c(previous, type)
        on.exit(budget$resolving_hints <- previous)
        parts <- strsplit(type, "::", fixed = TRUE)[[1L]]
        name <- utils::tail(parts, 1L)
        package <- if (length(parts) == 2L) parts[[1L]]
        linked <- length(class_links) && type %in% class_links
        project <- !is.null(package) && identical(package, index$document_package)
        if (project) bindings <- list()
        generator <- member_type_generator(name, package, index, bindings, budget)
        if (identical(generator$r6_role, "generator")) return(generator$fields$new$result_shape)
        if (!is.null(generator$refclass_instance)) return(generator$refclass_instance)
        if (!is.null(generator$s7_generator)) return(member_s7_shape(generator$s7_generator, generator$closure))
        class <- generator$s4_generator
        owner <- package
        if (linked && is.null(class) && is.null(package)) {
            history <- member_lookup(index$document_bindings, name)
            at <- bindings$.__member_position__
            if (name %in% names(bindings) || (length(history) && (is.null(at) || member_history_position(history, at) > 0L)) ||
                    member_package_shadows(index, name)) return(NULL)
            attached <- member_attached_root(index, name, at)
            if (!is.null(attached)) {
                # A topic may link to an ordinary imported object or alias.
                # Only its own namespace can prove a same-named class.
                if (!identical(attached$function_key, name) || is.null(attached$metadata)) return(NULL)
                owner <- attached$metadata
            }
        }
        s4_package <- if (!project) owner
        if (is.null(class) && (!project || length(index$document_s4[[name]])) &&
                !is.null(member_s4_descriptor(name, index, bindings, s4_package, budget))) {
            class <- list(name = name, package = s4_package)
        }
        if (!is.null(class)) {
            if (!is.null(class$recipe)) bindings$.__s4_classes__[[class$name]] <- class$recipe
            value <- member_s4_shape(class$name, index, bindings, class$package, budget = budget)
            if (is.null(value$type) && !isTRUE(budget$exhausted) &&
                    value$reason %in% c("unknown_s4_class", "unknown_s4_parent")) {
                value$s4_class <- list(name = class$name, package = class$package)
            }
            return(value)
        }
        # An S3 class need not be an exported symbol. Catalogs carry the
        # inheritance vector, so finding the owner does not decode namespaces.
        owners <- if (!is.null(owner)) owner else unique(index$hint_packages)
        matches <- Filter(function(pkg) {
            candidate <- if (identical(pkg, index$package)) index else member_lookup(index$namespace_indices, pkg)
            !is.null(member_lookup(candidate$classes, name))
        }, owners)
        if (length(matches) == 1L) return(member_value(type = name, member_package = matches[[1L]]))
        if (length(matches) > 1L) return(if (linked) NULL else member_value(reason = "ambiguous_type_hint"))
        if ((is.null(owner) || identical(owner, "base")) && name %in% member_builtin_hint_types) {
            return(member_value(type = if (name == "numeric") "double" else name))
        }
        # User-defined S3 classes need no declaration. Keep the nominal class
        # for dispatch even when no member table or inheritance is available.
        if (linked) {
            # A Markdown topic may describe any function or concept. An S3
            # method with a coincidentally matching suffix proves no class.
            owners <- if (!is.null(owner)) owner else unique(c("base", index$document_package, index$hint_packages))
            path <- s3_hint_ancestry(name, owner, owners)
            if (is.null(path)) return(NULL)
        } else {
            path <- s3_hint_ancestry(name, package)
        }
        if (is.null(path)) path <- structure(name, incomplete_s3_path = TRUE)
        member_value(type = name, classes = path, member_package = owner)
    }
    if (length(types) == 1L) {
        result <- resolve(types[[1L]])
    } else {
        values <- lapply(types, resolve)
        if (length(class_links) && any(vapply(values, is.null, logical(1L)))) return(NULL)
        if (length(class_links)) {
            for (i in seq_along(values)) values[[i]]$advisory_type <- TRUE
        }
        result <- member_value(reason = "type_hint_union", possible_returns = values)
    }
    if (length(class_links) && !is.null(result)) {
        result$advisory_type <- TRUE
        budget$advisory_reads <- (budget$advisory_reads %s3_or% 0L) + 1L
    }
    result
}

# Annotations fill gaps in inference. In particular a concrete argument or
# return value can have a narrower class and additional fields.
member_needs_call_hint <- function(value) {
    if (identical(value$reason, "recursive_default")) return(FALSE)
    if (!is.null(value$function_expr) || !is.null(value$function_key) || isTRUE(value$known_literal)) return(FALSE)
    if (!length(value$type)) return(!length(value$possible_returns) || identical(value$reason, "open_dispatch"))
    length(value$type) == 1L && !value$type %in% c(".missing", ".never", "NULL", "logical", "integer", "double",
        "complex", "character", "raw", "list", "pairlist", "function") && !length(value$classes) && is.null(value$r6_role) &&
        is.null(value$s4_class) && !isTRUE(value$s7) && is.null(value$fields) && is.null(value$slots) &&
        is.null(value$elements) && is.null(value$element_shape) && is.null(value$member_package)
}

# A concrete-looking child (even a literal returned by a selected method)
# can depend on an advisory receiver. Keep that dependency on materialized
# values so dispatch sees it through aliases, projections and cache hits.
member_advisory_result <- function(value, before, budget) {
    if (!identical(before, budget$advisory_reads)) {
        if (!is.null(value)) value$advisory_type <- TRUE
    } else if (isTRUE(value$advisory_type)) {
        budget$advisory_reads <- (before %s3_or% 0L) + 1L
    }
    value
}

member_refine_call_hint <- function(value, types, index, lexical, budget) {
    if (!length(types) || !member_needs_call_hint(value) || isTRUE(budget$exhausted)) return(value)
    hint <- member_parameter_hint_value(types, index, lexical, budget)
    if (is.null(hint) || (length(value$type) && !identical(value$type, hint$type))) return(value)
    if (!is.null(value$return_evidence)) return(member_join(value$return_evidence, hint))
    hint
}

# Keep only lexical inputs to the documented type. Member values can outlive
# a request, so look up the defining document on demand for cross-file reads.
member_class_hint <- function(types, index, bindings) {
    if (!length(types)) return(NULL)
    keys <- c(member_type_dependencies(types), ".__member_position__", ".__s4_classes__")
    list(types = types, bindings = bindings[intersect(names(bindings), keys)], uri = index$dispatch_document$uri)
}

member_refine_class_hint <- function(value, hint, index, budget = NULL) {
    if (is.null(hint) || (isTRUE(budget$explicit_hints_only) &&
                length(attr(hint$types, "roxygen_class_links", exact = TRUE)))) return(value)
    if (!member_needs_call_hint(value)) {
        if (isTRUE(budget$class_hint_probe)) budget$used_member_hint <- TRUE
        return(value)
    }
    if (is.null(budget)) budget <- member_request_budget()
    if (!is.null(budget$hint_wait) && !is.null(hint$uri)) {
        doc <- roxygen_hint_document(index$dispatch_workspace, hint$uri, index$dispatch_document)
        s3_record_hint_wait(budget$hint_wait, index$dispatch_workspace, doc, hint$types, uri = hint$uri)
    }
    if (!is.null(hint$uri) && !identical(hint$uri, index$dispatch_document$uri)) {
        workspace <- index$dispatch_workspace
        doc <- roxygen_hint_document(workspace, hint$uri)
        if (is.null(doc)) return(value)
        at <- hint$bindings$.__member_position__ %s3_or% c(Inf, Inf)
        index <- member_context_index(workspace, doc$uri, doc, at, referenced = member_type_dependencies(hint$types))
        if (!is.null(budget$origin_name)) {
            previous <- budget$document
            budget$document <- doc
            on.exit(budget$document <- previous)
        }
    }
    result <- member_refine_call_hint(value, hint$types, index, hint$bindings, budget)
    if (isTRUE(budget$class_hint_probe) && (length(result$type) || length(result$possible_returns))) budget$used_member_hint <- TRUE
    result
}
