# Source declarations are shared by the outline, symbol search, navigation and
# semantic tokens. All row numbers refer to the same physical parser tree.
class_call_helpers <- c(R6Class = "R6", setClass = "S4", setRefClass = "RefClass", new_class = "S7",
    setGeneric = "S4Generic", setGroupGeneric = "S4Generic", setMethod = "S4Method", setReplaceMethod = "S4Method")
class_helper_packages <- c(R6Class = "R6", setClass = "methods", setRefClass = "methods", new_class = "S7",
    setGeneric = "methods", setGroupGeneric = "methods", setMethod = "methods", setReplaceMethod = "methods")

syntax_call_arguments <- function(syntax, row) {
    children <- syntax_children(syntax, row)
    children <- children[syntax$token[children] != "COMMENT"]
    if (length(children) < 3L || syntax$token[children[[2L]]] != "'('") return(NULL)
    head <- syntax_children(syntax, children[[1L]])
    if (!length(head) || !utils::tail(syntax$token[head], 1L) %in% "SYMBOL_FUNCTION_CALL" ||
            !(length(head) == 1L || (length(head) == 3L && syntax$token[head[[2L]]] %in% c("NS_GET", "NS_GET_INT")))) {
        return(NULL)
    }
    part <- children[-seq_len(2L)]
    part <- part[syntax$token[part] != "')'"]
    comma <- syntax$token[part] == "','"
    if (length(part)) {
        group_index <- cumsum(c(TRUE, comma))[seq_along(part)]
        groups <- split(part[!comma], factor(group_index[!comma], levels = seq_len(sum(comma) + 1L)))
        value <- tag <- integer(length(groups))
        for (i in seq_along(groups)) {
            group <- groups[[i]]
            eq <- match("EQ_SUB", syntax$token[group], nomatch = 0L)
            if (eq > 1L) tag[[i]] <- group[[eq - 1L]]
            rows <- group[syntax$token[group] == "expr"]
            if (length(rows) == 1L) value[[i]] <- rows
        }
        keys <- rep("", length(groups))
        keys[tag > 0L] <- class_token_text(syntax$text[tag[tag > 0L]])
        names(value) <- keys
    } else {
        value <- tag <- integer()
    }
    list(name = class_token_text(utils::tail(syntax$text[head], 1L)),
        package = if (length(head) == 3L) class_token_text(syntax$text[head[[1L]]]),
        values = value, tags = tag)
}

class_matched_arguments <- function(call, formals) {
    fn <- function() NULL
    formals(fn) <- as.pairlist(stats::setNames(rep(list(quote(expr = )), length(formals)), formals))
    values <- as.list(call$values)
    matched <- tryCatch(match.call(fn, as.call(c(list(as.name("decl")), values)), expand.dots = FALSE),
        error = function(e) NULL)
    as.list(matched)[-1L]
}

class_literal_row <- function(syntax, row, kinds) {
    if (!length(row) || !row) return(0L)
    children <- syntax_children(syntax, row)
    if (length(children) == 1L && syntax$token[children] %in% kinds) children else 0L
}

class_source_declarations <- function(syntax, content, uri, incomplete_lines = integer()) {
    which_call <- which(syntax$token == "SYMBOL_FUNCTION_CALL" & syntax$text %in% names(class_call_helpers))
    if (!length(which_call)) return(list())
    assignments <- syntax$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")
    helper_names <- which(syntax$token == "SYMBOL" & syntax$text %in% names(class_call_helpers))
    shadowed <- integer()
    for (token in helper_names) {
        i <- syntax$parent_row[syntax$parent_row[token]]
        if (!length(i) || !i) next
        parts <- syntax_children(syntax, i)
        parts <- parts[syntax$token[parts] != "COMMENT"]
        if (length(parts) != 3L || !assignments[parts[[2L]]]) next
        lhs <- if (syntax$token[parts[[2L]]] == "RIGHT_ASSIGN") 3L else 1L
        if (identical(class_literal_row(syntax, parts[[lhs]], "SYMBOL"), token) &&
                (!length(syntax$function_scope) || !syntax$function_scope[[token]])) shadowed <- c(shadowed, token)
    }
    document <- Document$new(uri, content = content)
    at <- function(row) navigation_range(document, syntax, row)
    formals <- list(R6 = names(formals(R6::R6Class)), RefClass = names(formals(methods::setRefClass)),
        S4 = names(formals(methods::setClass)),
        S7 = c("name", "parent", "package", "properties", "abstract", "constructor", "validator"))
    declarations <- masked <- list()
    for (symbol in which_call) {
        row <- syntax$parent_row[syntax$parent_row[symbol]]
        if (!length(row) || !row || (length(incomplete_lines) &&
                    findInterval(syntax$line1[row] - 2L, incomplete_lines) !=
                        findInterval(syntax$line2[row] - 1L, incomplete_lines))) next
        call <- syntax_call_arguments(syntax, row)
        helper <- syntax$text[symbol]
        if (is.null(call) || !identical(call$name, helper) ||
                (!is.null(call$package) && call$package != class_helper_packages[[helper]])) next
        type <- class_call_helpers[[helper]]
        formal_names <- if (type %in% c("S4Generic", "S4Method")) {
            names(formals(getExportedValue("methods", if (type == "S4Method") "setMethod" else helper)))
        } else {
            formals[[type]]
        }
        args <- class_matched_arguments(call, formal_names)
        if (!is.null(args$where)) next
        parent <- syntax$parent_row[[row]]
        expr <- row
        binding <- 0L
        if (parent) {
            siblings <- syntax_children(syntax, parent)
            siblings <- siblings[syntax$token[siblings] != "COMMENT"]
            if (length(siblings) == 3L && syntax$token[siblings[[2L]]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")) {
                lhs <- if (syntax$token[siblings[[2L]]] == "RIGHT_ASSIGN") 3L else 1L
                if (siblings[[4L - lhs]] != row) next
                binding <- class_literal_row(syntax, siblings[[lhs]], "SYMBOL")
                if (!binding) next
                expr <- parent
            } else if (!any(syntax$token[siblings] == "'{'")) {
                next
            }
        }
        if (is.null(call$package) && any(syntax$text[shadowed] == helper &
                    syntax$order_rank[shadowed] < syntax$order_rank[symbol])) {
            if (binding) masked[[class_token_text(syntax$text[[binding]])]] <- at(expr)
            next
        }
        # A class declared inside a function is not a workspace binding.
        if (length(syntax$function_scope) && syntax$function_scope[[row]] > 0L) next
        enclosing <- syntax$parent_row[[expr]]
        if (enclosing && !any(syntax$token[syntax_children(syntax, enclosing)] == "'{'")) next
        literal <- class_literal_row(syntax, switch(type, R6 = args$classname, S7 = args$name, S4Generic = args$name, S4Method = args$f, args$Class), "STR_CONST")
        if (!literal && !binding) next
        name_row <- if (binding) binding else literal
        name <- class_token_text(syntax$text[[name_row]])
        class_name <- if (literal) class_token_text(syntax$text[[literal]]) else name
        if (identical(helper, "setReplaceMethod")) name <- class_name <- paste0(class_name, "<-")
        kind <- switch(type, S4Generic = SymbolKind$Function, S4Method = SymbolKind$Method, SymbolKind$Class)
        member_rows <- member_values <- integer()
        members <- list()
        for (section in switch(type, R6 = c("public", "private", "active"), S4 = c("slots", "representation"),
                RefClass = c("fields", "methods"), S7 = "properties")) {
            group <- args[[section]]
            if (is.null(group) || !group) next
            entries <- syntax_call_arguments(syntax, group)
            accepted <- if (section %in% c("slots", "representation", "fields")) c("c", "list", "representation") else "list"
            if (is.null(entries) || !entries$name %in% accepted) next
            owner <- if (entries$name == "representation") "methods" else "base"
            if (!is.null(entries$package) && entries$package != owner) next
            for (i in which(entries$tags > 0L & entries$values > 0L)) {
                tag <- entries$tags[[i]]
                value <- entries$values[[i]]
                parts <- syntax_children(syntax, value)
                is_function <- any(syntax$token[parts] == "FUNCTION" | syntax$text[parts] == "\\")
                member_kind <- if (section %in% c("active", "properties")) SymbolKind$Property else if (
                    section == "methods" || (section %in% c("public", "private") && is_function)) SymbolKind$Method else SymbolKind$Field
                detail <- if (type == "S4") "slot" else if (type == "S7") "property" else section
                selection <- at(tag)
                members[[length(members) + 1L]] <- document_symbol(names(entries$values)[[i]], member_kind,
                    range(selection$start, at(value)$end), selection, detail = detail)
                member_rows <- c(member_rows, tag)
                member_values <- c(member_values, value)
            }
        }
        ancestry <- switch(type, R6 = args$inherit, S7 = args$parent, args$contains)
        parents <- character()
        if (length(ancestry) && ancestry) {
            simple <- class_literal_row(syntax, ancestry, c("STR_CONST", "SYMBOL"))
            if (simple) parents <- class_token_text(syntax$text[[simple]]) else if (type %in% c("S4", "RefClass")) {
                values <- syntax_call_arguments(syntax, ancestry)
                if (identical(values$name, "c")) {
                    rows <- vapply(values$values, function(i) class_literal_row(syntax, i, "STR_CONST"), integer(1L))
                    parents <- class_token_text(syntax$text[rows[rows > 0L]])
                }
            }
        }
        declarations[[length(declarations) + 1L]] <- list(name = name, class_name = class_name, type = type, kind = kind,
            uri = uri, range = at(expr), selectionRange = at(name_row), name_row = name_row, literal_row = literal,
            binding = binding > 0L, children = members, member_rows = member_rows, member_values = member_values,
            parents = parents)
    }
    if (length(masked)) attr(declarations, "masked") <- masked
    declarations
}

class_binding_definitions <- function(definitions, classes) {
    masked <- attr(classes, "masked", exact = TRUE)
    for (name in intersect(names(definitions), names(masked))) {
        if (equal_range(definitions[[name]]$range, masked[[name]])) definitions[[name]]$type <- "variable"
    }
    for (cls in classes) {
        if (cls$binding && cls$kind == SymbolKind$Class && !is.null(definitions[[cls$name]]) &&
                equal_range(definitions[[cls$name]]$range, cls$range)) definitions[[cls$name]]$type <- cls$type
    }
    definitions
}

class_workspace_declarations <- function(workspace, uris = NULL) {
    indexed <- !is.null(workspace$index) && isTRUE(workspace$index$enabled)
    if (is.null(uris)) uris <- unique(c(if (indexed) workspace$index$summaries$keys(), workspace$documents$keys()))
    unlist(lapply(uris, function(uri) {
        document <- workspace$documents$get(uri, NULL)
        if (!is.null(document)) {
            current_parse_data(uri, workspace, document)$class_data$declarations
        } else if (indexed) {
            workspace$index$summaries$get(uri, NULL)$class_declarations
        }
    }), recursive = FALSE)
}

class_hierarchy_item <- function(cls) {
    list(name = cls$class_name, uri = cls$uri, range = cls$selectionRange, classType = cls$type)
}
