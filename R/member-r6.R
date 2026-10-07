# Recognize declarative R6 definitions without creating an instance or running
# initialize/active functions. Only explicit package calls or verified imports
# qualify; a document function called R6Class has ordinary function semantics.
member_is_r6_call <- function(expr, index, bindings) {
    if (!is.call(expr)) {
        return(FALSE)
    }
    head <- expr[[1L]]
    if (member_head(head, "::")) {
        return(identical(member_name(head[[2L]]), "R6") &&
                identical(member_name(head[[3L]]), "R6Class"))
    }
    identical(member_name(head), "R6Class") && !"R6Class" %in% names(bindings) &&
        !"R6Class" %in% names(index$document_bindings) &&
        isTRUE(index$r6_attached)
}

member_r6_shape <- function(expr, index, bindings, budget) {
    args <- as.list(expr)[-1L]
    active <- args$active
    inherited <- member_value()
    if (!is.null(args$inherit)) {
        generator <- member_infer(args$inherit, index, bindings, budget = budget)
        inherited <- generator$fields$new$result_shape
    }
    fields <- if (is.null(inherited$fields)) list() else inherited$fields
    # Private fields are available to method summaries but never candidates.
    private <- list()
    for (section in c("private", "public")) {
        node <- args[[section]]
        if (!member_head(node, "list")) next
        values <- as.list(node)[-1L]
        for (name in names(values)) {
            value <- member_infer(values[[name]], index, bindings, budget = budget)
            if (!is.null(value$function_expr)) {
                value$receiver_name <- "self"
                value$closure$self <- NULL
                value$closure$super <- inherited
                value$closure$private <- member_value(type = "environment", fields = private)
            }
            if (section == "public") {
                fields[name] <- list(value)
            } else {
                private[name] <- list(value)
            }
        }
    }
    if (member_head(active, "list")) {
        for (name in names(as.list(active)[-1L])) {
            fields[name] <- list(member_value(reason = "active_property"))
        }
    }
    instance <- member_value(type = "environment", fields = fields, open = TRUE)
    member_value(type = "environment", fields = list(new = member_value(
        type = "function", result_shape = instance
    )))
}

member_r6_runtime_shape <- function(generator, index, trail = list()) {
    if (length(trail) > 16L || any(vapply(trail, identical, logical(1L), generator))) {
        return(member_value(reason = "inheritance_cycle"))
    }
    get_plain <- function(name) member_binding(generator, name)[[2L]]
    public <- get_plain("public_methods")
    public_fields <- get_plain("public_fields")
    active <- get_plain("active")
    inherited <- get_plain("inherit")
    if (!is.list(public) || is.object(public)) {
        return(member_value(reason = "unsupported_r6"))
    }
    convert <- function(values) {
        as.call(c(list(as.name("list")), lapply(values, function(x) {
            if (typeof(x) == "closure") {
                member_function_syntax(x)
            } else if (!is.object(x) && is.atomic(x) && length(x) <= 100L) {
                x
            } else {
                as.name(".__unknown_field__")
            }
        })))
    }
    bindings <- list()
    # Inheritance may be an expression resolved in the generator's parent scope.
    # Read a direct binding only; never evaluate get_inherit() or an expression.
    if (is.symbol(inherited)) {
        scope <- get_plain("parent_env")
        if (is.environment(scope)) {
            record <- member_binding(scope, as.character(inherited))
            parent <- record[[2L]]
            if (identical(record[[1L]], "value") && is.environment(parent)) {
                bindings$.__r6_parent__ <- member_r6_runtime_shape(parent, index, c(trail, list(generator)))
            }
        }
    }
    expr <- as.call(list(as.call(list(as.name("::"), as.name("R6"), as.name("R6Class"))),
            classname = "inspected", public = convert(c(public_fields, public)),
            active = convert(active), inherit = as.name(".__r6_parent__")
        ))
    member_r6_shape(expr, index, bindings, NULL)
}
