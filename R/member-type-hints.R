member_builtin_hint_types <- c("NULL", "logical", "integer", "numeric", "double", "complex", "character",
    "raw", "list", "pairlist", "environment", "function", "expression")

# Only class names that can come from an imported package need its model.
member_type_hint_packages <- function(parsed) {
    data <- parsed$member_data
    types <- unique(unlist(data$type_hints, use.names = FALSE))
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

# A type name describes an instance. Read generator and class metadata without
# calling constructors or inspecting the user's session.
member_hinted_value <- function(types, index, bindings, budget) {
    if (!roxygen_type_hints_enabled()) return(NULL)
    if (!length(types) || length(types) > max_type_hint_alternatives) return(NULL)
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
        project <- !is.null(package) && identical(package, index$document_package)
        if (project) {
            # A qualification names a package binding, even inside a function
            # which shadows it. Read the package scope, including peer files.
            bindings <- list()
            generator <- member_project_type_generator(name, index, budget)
        } else if (is.null(package)) {
            generator <- member_infer(as.name(name), index, bindings, budget = budget)
        } else {
            catalog <- member_lookup(index$namespace_indices, package)
            if (is.null(catalog) && identical(package, index$package)) catalog <- index
            generator <- member_lookup(catalog$roots, name)
        }
        if (identical(generator$r6_role, "generator")) return(generator$fields$new$result_shape)
        if (!is.null(generator$refclass_instance)) return(generator$refclass_instance)
        if (!is.null(generator$s7_generator)) return(member_s7_shape(generator$s7_generator, generator$closure))
        class <- generator$s4_generator
        owner <- if (!project) package
        if (is.null(class) && (!project || length(index$document_s4[[name]])) &&
                !is.null(member_s4_descriptor(name, index, bindings, owner, budget))) {
            class <- list(name = name, package = owner)
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
        owners <- if (!is.null(package)) package else unique(index$hint_packages)
        matches <- Filter(function(pkg) {
            candidate <- if (identical(pkg, index$package)) index else member_lookup(index$namespace_indices, pkg)
            !is.null(member_lookup(candidate$classes, name))
        }, owners)
        if (length(matches) == 1L) return(member_value(type = name, member_package = matches[[1L]]))
        if (length(matches) > 1L) return(member_value(reason = "ambiguous_type_hint"))
        if ((is.null(package) || identical(package, "base")) && name %in% member_builtin_hint_types) {
            return(member_value(type = if (name == "numeric") "double" else name))
        }
        # User-defined S3 classes need no declaration. Keep the nominal class
        # for dispatch even when no member table or inheritance is available.
        path <- s3_hint_ancestry(name, package)
        if (is.null(path)) path <- structure(name, incomplete_s3_path = TRUE)
        member_value(type = name, classes = path, member_package = package)
    }
    if (length(types) == 1L) return(resolve(types[[1L]]))
    values <- lapply(types, resolve)
    member_value(reason = "type_hint_union", possible_returns = values)
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

member_refine_call_hint <- function(value, types, index, lexical, budget) {
    if (!length(types) || !member_needs_call_hint(value) || isTRUE(budget$exhausted)) return(value)
    hint <- member_hinted_value(types, index, lexical, budget)
    if (is.null(hint) || (length(value$type) && !identical(value$type, hint$type))) return(value)
    if (!is.null(value$return_evidence)) return(member_join(value$return_evidence, hint))
    hint
}
