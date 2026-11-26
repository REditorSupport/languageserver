# Only the documented implementation argument owns the declaration's tags.
# Match setReplaceMethod's forwarded arguments against setMethod, as methods
# itself does. The optional replacement callback of setAs is a different method.
roxygen_registration_argument <- c(setGeneric = "def", setGroupGeneric = "def", setMethod = "definition",
    setReplaceMethod = "definition", setAs = "def", new_generic = "fun")

roxygen_registered_function <- function(syntax, row, declaration, head, memo) {
    parts <- syntax_children(syntax, head)
    if (!length(parts)) return(0L)
    if (length(parts) > 1L && any(syntax$token[parts] %in% c("'$'", "LBB"))) {
        return(roxygen_r6_set_function_row(syntax, row, memo))
    }
    last <- parts[[length(parts)]]
    if (syntax$token[[last]] != "SYMBOL_FUNCTION_CALL") return(0L)
    name <- class_token_text(syntax$text[[last]])
    if (!name %in% names(roxygen_registration_argument) || !class_top_level_statement(syntax, declaration)) return(0L)
    package <- if (name == "new_generic") "S7" else "methods"
    if (length(parts) == 1L) {
        if (roxygen_helper_shadowed(syntax, name, last, memo)) return(0L)
    } else if (length(parts) != 3L || syntax$token[parts[[2L]]] != "NS_GET" ||
            !identical(class_token_text(syntax$text[[parts[[1L]]]]), package)) {
        return(0L)
    }
    call <- syntax_call_arguments(syntax, row)
    if (is.null(call)) return(0L)
    formals <- if (name == "new_generic") {
        c("name", "dispatch_args", "fun")
    } else {
        helper <- if (name == "setReplaceMethod") "setMethod" else name
        names(formals(getExportedValue("methods", helper)))
    }
    args <- class_matched_arguments(call, formals)
    value <- args[[roxygen_registration_argument[[name]]]]
    if (is.null(value) || !value) return(0L)
    repeat {
        bits <- syntax_children(syntax, value)
        bits <- bits[syntax$token[bits] != "COMMENT"]
        if (!length(bits)) return(0L)
        tokens <- syntax$token[bits]
        if (tokens[[1L]] %in% c("FUNCTION", r"('\\')")) return(bits[[1L]])
        if (length(bits) != 3L || !identical(tokens[c(1L, 3L)], c("'('", "')'"))) return(0L)
        value <- bits[[2L]]
    }
}
