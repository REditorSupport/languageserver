s4_registration_helpers <- c("setGeneric", "setGroupGeneric", "setMethod", "setReplaceMethod")

# Snapshot the standard S4 groups supplied by this version of R. Retrieve the
# group objects from methods so project registrations cannot change the table.
s4_definition_group_index <- local({
    index <- new.env(hash = TRUE, parent = emptyenv())
    namespace <- asNamespace("methods")
    for (name in c("Ops", "Arith", "Compare", "Logic", "Math", "Math2", "Summary", "Complex", "matrixOps")) {
        group <- get0(name, namespace, inherits = FALSE)
        if (is.null(group)) next
        parents <- methods::getGroup(group, recursive = TRUE, where = namespace)
        groups <- c(name, unlist(parents, use.names = FALSE))
        for (member in methods::getGroupMembers(group)) index[[member]] <- groups
    }
    index
})

s4_definition_groups <- function(name) {
    get0(name, s4_definition_group_index, inherits = FALSE, ifnotfound = character())
}

s4_registration_name <- function(helper, name) {
    if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) return(NULL)
    if (helper == "setReplaceMethod") paste0(name, "<-") else name
}

# Keep source declarations separate from ordinary variable bindings. Several
# methods for one generic can be declared in the same file or package.
s4_declarations <- function(expressions, content, incomplete_lines = integer()) {
    # Escaped names can also spell a helper. Most files contain no registrations.
    if (!length(expressions) ||
            !any(grepl("set(Generic|GroupGeneric|Method|ReplaceMethod)|\\\\", content, perl = TRUE))) {
        return(list())
    }
    groups <- new.env(hash = TRUE, parent = emptyenv())
    shadowed <- character()
    visit <- function(expr, ref) {
        if (missing(expr) || !is.call(expr) || is.null(ref)) return()
        head <- expr[[1L]]
        if (is.symbol(head)) {
            helper <- as.character(head)
            if (helper == "{") {
                refs <- attr(expr, "srcref")
                for (i in seq_along(refs)[-1L]) visit(expr[[i]], refs[[i]])
                return()
            }
            if (helper %in% c("<-", "=", "<<-") && length(expr) == 3L && is.symbol(expr[[2L]])) {
                target <- as.character(expr[[2L]])
                if (target %in% s4_registration_helpers) shadowed <<- union(shadowed, target)
                return()
            }
            if (helper %in% shadowed) return()
        } else if (is.call(head) && is_ns_call(head) && identical(member_name(head[[2L]]), "methods")) {
            helper <- member_name(head[[3L]])
        } else {
            return()
        }
        if (is.null(helper) || !helper %in% s4_registration_helpers) return()

        # setReplaceMethod forwards the arguments following f to setMethod.
        # Match against known formals without evaluating any project expression.
        is_method <- helper %in% c("setMethod", "setReplaceMethod")
        fn <- getExportedValue("methods", if (is_method) "setMethod" else helper)
        matched <- tryCatch(match.call(fn, expr), error = function(e) NULL)
        if ("where" %in% names(matched)) return()
        definition <- if (is_method) matched$definition else matched$def
        if ((is_method || !is.null(definition)) && !is.symbol(definition) && !is.call(definition)) return()
        name <- s4_registration_name(helper, if (is_method) matched$f else matched$name)
        if (is.null(name)) return()

        if (is.null(groups[[name]])) groups[[name]] <- collections::queue()
        groups[[name]]$push(list(kind = if (is_method) "method" else "generic", range = expr_range(ref)))
    }
    refs <- attr(expressions, "srcref")
    for (i in seq_along(refs)) {
        ref <- refs[[i]]
        if (length(incomplete_lines) &&
                findInterval(ref[[1L]] - 2L, incomplete_lines) != findInterval(ref[[3L]] - 1L, incomplete_lines)) next
        visit(expressions[[i]], ref)
    }
    lapply(as.list(groups, all.names = TRUE), function(group) group$as_list())
}
