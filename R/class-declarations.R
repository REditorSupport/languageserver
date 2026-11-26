# Source declarations are shared by the outline, symbol search, navigation and
# semantic tokens. All row numbers refer to the same physical parser tree.
class_call_helpers <- c(R6Class = "R6", setClass = "S4", setRefClass = "RefClass", new_class = "S7",
    deprecated_class = "S7", new_generic = "S7Generic", deprecated_generic = "S7Generic", method = "S7Method",
    setGeneric = "S4Generic", setGroupGeneric = "S4Generic", setMethod = "S4Method", setReplaceMethod = "S4Method")
class_helper_packages <- c(R6Class = "R6", setClass = "methods", setRefClass = "methods", new_class = "S7",
    deprecated_class = "S7", new_generic = "S7", deprecated_generic = "S7", method = "S7",
    setGeneric = "methods", setGroupGeneric = "methods", setMethod = "methods", setReplaceMethod = "methods")

syntax_call_arguments <- function(syntax, row, computed_name = NULL) {
    children <- syntax_children(syntax, row)
    children <- children[syntax$token[children] != "COMMENT"]
    if (length(children) < 3L || syntax$token[children[[2L]]] != "'('") return(NULL)
    head <- syntax_children(syntax, children[[1L]])
    if (is.null(computed_name) && (!length(head) || !utils::tail(syntax$token[head], 1L) %in% "SYMBOL_FUNCTION_CALL" ||
            !(length(head) == 1L || (length(head) == 3L && syntax$token[head[[2L]]] %in% c("NS_GET", "NS_GET_INT"))))) {
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
    list(name = if (is.null(computed_name)) class_token_text(utils::tail(syntax$text[head], 1L)) else computed_name,
        package = if (is.null(computed_name) && length(head) == 3L) class_token_text(syntax$text[head[[1L]]]),
        values = value, tags = tag)
}

# Match the arguments as received by the callee, including pipe insertion
# and direct placeholders. The original argument rows remain available.
syntax_pipe_arguments <- function(syntax, row, call) {
    parent <- syntax$parent_row[[row]]
    if (parent) {
        parts <- syntax_children(syntax, parent)
        parts <- parts[syntax$token[parts] != "COMMENT"]
        if (length(parts) == 3L && parts[[3L]] == row && syntax$text[[parts[[2L]]]] %in% c("|>", "%>%", "%T>%", "%<>%")) {
            native <- syntax$text[[parts[[2L]]]] == "|>"
            placeholder <- vapply(call$values, function(arg) {
                if (!arg) return(FALSE)
                only <- syntax_children(syntax, arg)
                length(only) == 1L && if (native) syntax$token[[only]] == "PLACEHOLDER" else syntax$text[[only]] == "."
            }, logical(1L))
            if (any(placeholder)) {
                call$values[placeholder] <- parts[[1L]]
            } else {
                call$values <- c(stats::setNames(parts[[1L]], ""), call$values)
            }
        }
    }
    call
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

class_s7_property_formals <- list(new_property = c("class", "getter", "setter", "validator", "default", "name"),
    deprecated_property = c("old", "new", "when", "method", "class", "default", "validator"))

# An unnamed property takes its name from the descriptor. A deprecated
# property can also refer to a replacement by its literal property name.
class_s7_property_literals <- function(syntax, call) {
    keys <- class_s7_property_formals[[call$name]]
    if (is.null(keys) || (!is.null(call$package) && call$package != "S7")) return(NULL)
    args <- class_matched_arguments(call, keys)
    if (call$name == "deprecated_property" && is.null(args$when)) return(NULL)
    list(name = class_literal_row(syntax, if (call$name == "new_property") args$name else args$old, "STR_CONST"),
        replacement = class_literal_row(syntax, args$new, "STR_CONST"))
}

# Hierarchy names refer to the characters inside a registry name literal.
# Keep escape sequences in their written source positions.
class_literal_range <- function(selection, text) {
    prefix <- if (startsWith(text, '"') || startsWith(text, "'")) 1L else 0L
    suffix <- prefix
    if (!prefix && grepl("^[rR]['\"]", text)) {
        delimiter <- regexpr("^[rR]['\"]-*", text)
        prefix <- attr(delimiter, "match.length") + 1L
        suffix <- prefix - 1L
    }
    range(position(selection$start$line, selection$start$character + prefix),
        position(selection$end$line, selection$end$character - suffix))
}

# The syntax indexes run before the metadata worker. Installed exports can
# disambiguate wildcard imports without loading a namespace or running hooks.
class_s7_exports <- local({
    cache <- NULL
    function(pkg) {
        factories <- names(class_helper_packages)[class_helper_packages == "S7"]
        keys <- c(":=", setdiff(factories, "method"), "method<-", "S4_contains", "prop", names(class_s7_property_formals))
        possible <- stats::setNames(rep(NA, length(keys)), keys)
        if (!grepl("^[[:alpha:]][[:alnum:].]*$", pkg)) return(possible)
        dir <- find.package(pkg, lib.loc = .libPaths(), quiet = TRUE)
        if (length(dir) != 1L) return(possible)
        path <- file.path(dir, "Meta", "nsInfo.rds")
        stat <- file.info(path, extra_cols = FALSE)
        if (is.na(stat$size) || stat$isdir) return(possible)
        if (is.null(cache)) cache <<- ByteLruCache$new(256 * 1024L, max_entries = 64L)
        stamp <- stat[c("size", "ctime", "mtime")]
        previous <- cache$get(path)
        if (identical(stamp, previous$stamp)) return(previous$exports)
        info <- tryCatch(readRDS(path), error = function(e) NULL)
        if (is.list(info)) {
            found <- keys %in% as.character(info$exports)
            possible[] <- found
            for (pattern in info$exportPatterns) possible[!found & grepl(pattern, keys)] <- NA
        }
        cache$set(path, list(stamp = stamp, exports = possible))
        possible
    }
})

class_s7_exports_current <- function(declarations) {
    exports <- attr(declarations, "source_exports", exact = TRUE)
    if (is.null(exports)) return(TRUE)
    all(vapply(names(exports), function(pkg) identical(exports[[pkg]], class_s7_exports(pkg)), logical(1L)))
}

# R parses every infix := as LEFT_ASSIGN. Verify which binding is visible at
# the declaration before treating the right-hand call as a named S7 factory.
class_s7_scope <- function(syntax, calls, call_names, imports, masked, attachment_exclusions = NULL) {
    snapshots <- new.env(hash = TRUE, parent = emptyenv())
    exports <- function(pkg) {
        if (!exists(pkg, snapshots, inherits = FALSE)) snapshots[[pkg]] <- class_s7_exports(pkg)
        snapshots[[pkg]]
    }
    api <- exports("S7")
    owners <- stats::setNames(rep(NA, length(api)), names(api))
    for (item in member_import_bindings(imports, names(api))) {
        flags <- exports(item$package)[item$targets]
        keys <- is.na(flags) | flags
        owners[item$names[keys]] <- identical(item$package, "S7") & item$names[keys] == item$targets[keys]
    }
    events <- calls[call_names %in% c("library", "require", "attach", "detach")]
    event_rows <- syntax$parent_row[syntax$parent_row[events]]
    top_level <- vapply(event_rows, function(row) row > 0L && class_top_level_statement(syntax, row), logical(1L))
    events <- events[top_level]
    event_rows <- event_rows[top_level]
    order <- order(syntax$order_rank[event_rows])
    ranks <- syntax$order_rank[event_rows[order]]
    attached <- stats::setNames(rep(FALSE, length(api)), names(api))
    s7_attached <- used_exclusions <- FALSE
    changes <- lapply(order, function(i) {
        call <- syntax_call_arguments(syntax, event_rows[[i]])
        package <- NULL
        if (!is.null(call) && call$name %in% c("library", "require") && (identical(call$package, "base") ||
                    (is.null(call$package) && !masked(call$name, events[[i]])))) {
            fn <- if (call$name == "library") base::library else base::require
            args <- class_matched_arguments(call, names(formals(fn)))
            if (all(names(args) %in% member_attachment_arguments)) {
                token <- class_literal_row(syntax, args$package, c("SYMBOL", "STR_CONST"))
                if (token && (is.null(args$character.only) || syntax$token[[token]] == "STR_CONST")) {
                    package <- class_token_text(syntax$text[[token]])
                }
            }
        }
        if (is.null(package) || !nzchar(package)) {
            attached[] <<- FALSE
            s7_attached <<- FALSE
        } else {
            flags <- exports(package)
            if (s7_attached) {
                excluded <- intersect(names(flags)[is.na(flags) | flags], attachment_exclusions[[package]])
                if (length(excluded)) {
                    flags[excluded] <- FALSE
                    used_exclusions <<- TRUE
                }
            }
            attached[is.na(flags) | flags] <<- identical(package, "S7")
            if (identical(package, "S7")) s7_attached <<- TRUE
        }
        attached
    })
    owns <- function(name, row) {
        if (masked(name, row) || identical(api[[name]], FALSE)) return(FALSE)
        if (!is.na(owners[[name]])) return(owners[[name]])
        at <- findInterval(syntax$order_rank[[row]], ranks)
        at > 0L && changes[[at]][[name]]
    }
    list(owns = owns, exports = as.list(snapshots, all.names = TRUE), used_exclusions = used_exclusions)
}

# S7 ownership needs unconditional statements. For other class systems,
# declarations in control-flow bodies can also describe workspace types.
class_top_level_statement <- function(syntax, row, allow_control = FALSE) {
    parent <- syntax$parent_row[[row]]
    while (parent) {
        head <- syntax$token[[syntax$children[[syntax$child_start[[parent]]]]]]
        if (head != "'{'") {
            if (!allow_control || !head %in% c("IF", "FOR", "WHILE", "REPEAT")) return(FALSE)
            parts <- syntax_children(syntax, parent)
            parts <- parts[syntax$token[parts] != "COMMENT"]
            previous <- match(row, parts, nomatch = 0L) - 1L
            # An if/loop body follows its header, or the else/repeat keyword.
            if (previous < 1L || !syntax$token[[parts[[previous]]]] %in%
                    c("')'", "ELSE", "forcond", "REPEAT")) return(FALSE)
        }
        row <- parent
        parent <- syntax$parent_row[[row]]
    }
    TRUE
}

# Recover the RHS of an unconditional assignment before this use. The
# declaration index also uses this for a named method implementation.
class_binding_value <- function(syntax, navigation, token) {
    found <- navigation_definitions(list(syntax_data = syntax, navigation_data = navigation),
        syntax$line1[[token]], syntax$col1[[token]], class_token_text(syntax$text[[token]]), token = token, last = TRUE)
    if (!length(found) || syntax$order_rank[[found]] >= syntax$order_rank[[token]] ||
            !class_top_level_statement(syntax, found)) return(0L)
    parts <- syntax_children(syntax, found)
    parts <- parts[syntax$token[parts] != "COMMENT"]
    if (length(parts) != 3L || !syntax$text[[parts[[2L]]]] %in% c("<-", "=", "->")) return(0L)
    parts[[if (syntax$token[[parts[[2L]]]] == "RIGHT_ASSIGN") 1L else 3L]]
}

class_source_declarations <- function(syntax, content, uri, incomplete_lines = integer(),
    namespace_imports = member_namespace_imports(uri), attachment_exclusions = NULL, navigation = NULL) {
    calls <- which(syntax$token == "SYMBOL_FUNCTION_CALL")
    call_names <- reference_symbol_names(syntax$text[calls])
    has_bridge <- "S4_contains" %in% call_names
    helpers <- which(call_names %in% names(class_call_helpers))
    binds <- if (":=" %in% syntax$text) which(syntax$token == "LEFT_ASSIGN" & syntax$text == ":=")
    if (length(binds)) {
        rows <- syntax$parent_row[binds]
        binds <- binds[vapply(rows, function(row) class_top_level_statement(syntax, row), logical(1L))]
    }
    if (!length(helpers) && !length(binds)) return(list())
    assignments <- syntax$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")
    variables <- which(syntax$token == "SYMBOL")
    variable_names <- reference_symbol_names(syntax$text[variables])
    helper_names <- variables[variable_names %in% c(names(class_call_helpers), names(class_s7_property_formals),
            ":=", "method<-", "S4_contains", "prop", "library", "require")]
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
    shadows <- if (length(shadowed)) split(syntax$order_rank[shadowed], reference_symbol_names(syntax$text[shadowed]))
    is_masked <- function(name, row) any(shadows[[name]] < syntax$order_rank[[row]])
    s7_scope <- NULL
    s7_owns <- function(name, row) {
        if (is.null(s7_scope)) s7_scope <<- class_s7_scope(syntax, calls, call_names, namespace_imports, is_masked, attachment_exclusions)
        s7_scope$owns(name, row)
    }
    document <- Document$new(uri, content = content)
    at <- function(row) navigation_range(document, syntax, row)
    binding_value <- function(token) {
        if (is.null(navigation)) navigation <<- navigation_parse_data(syntax)
        class_binding_value(syntax, navigation, token)
    }
    formals <- list(R6 = names(formals(R6::R6Class)), RefClass = names(formals(methods::setRefClass)),
        S4 = names(formals(methods::setClass)),
        S7 = c("name", "parent", "package", "properties", "abstract", "constructor", "validator"),
        S7Generic = c("name", "dispatch_args", "fun"), S7Method = c("generic", "signature"))
    declarations <- masked <- list()
    for (i in helpers) {
        symbol <- calls[[i]]
        helper <- call_names[[i]]
        row <- syntax$parent_row[syntax$parent_row[symbol]]
        if (!length(row) || !row || (length(incomplete_lines) &&
                    findInterval(syntax$line1[row] - 2L, incomplete_lines) !=
                        findInterval(syntax$line2[row] - 1L, incomplete_lines))) next
        call <- syntax_call_arguments(syntax, row)
        if (is.null(call) || !identical(call$name, helper) ||
                (!is.null(call$package) && call$package != class_helper_packages[[helper]])) next
        type <- class_call_helpers[[helper]]
        formal_names <- if (type %in% c("S4Generic", "S4Method")) {
            names(formals(getExportedValue("methods", if (type == "S4Method") "setMethod" else helper)))
        } else {
            formals[[type]]
        }
        parent <- syntax$parent_row[[row]]
        expr <- row
        binding <- 0L
        named_bind <- FALSE
        if (parent) {
            siblings <- syntax_children(syntax, parent)
            siblings <- siblings[syntax$token[siblings] != "COMMENT"]
            if (length(siblings) == 3L && syntax$token[siblings[[2L]]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")) {
                lhs <- if (syntax$token[siblings[[2L]]] == "RIGHT_ASSIGN") 3L else 1L
                named_bind <- identical(syntax$text[[siblings[[2L]]]], ":=")
                if (type == "S7Method") {
                    if (named_bind || siblings[[lhs]] != row) next
                    impl <- siblings[[4L - lhs]]
                    if (!syntax$function_close[[impl]]) {
                        # The compatibility vignette assigns the existing prop
                        # function as the $ method. A NULL RHS removes a method.
                        parts <- syntax_children(syntax, impl)
                        parts <- parts[syntax$token[parts] != "COMMENT"]
                        bare <- class_literal_row(syntax, impl, "SYMBOL")
                        known <- identical(class_token_text(syntax$text[parts]), c("S7", "::", "prop")) ||
                            (bare && class_token_text(syntax$text[[bare]]) == "prop" && s7_owns("prop", bare))
                        if (!known && bare) {
                            bound <- binding_value(bare)
                            known <- bound && syntax$function_close[[bound]] > 0L
                        }
                        if (!known) next
                    }
                } else {
                    if (siblings[[4L - lhs]] != row) next
                    binding <- class_literal_row(syntax, siblings[[lhs]], "SYMBOL")
                    if (!binding) next
                    if (named_bind && (!type %in% c("S7", "S7Generic") || !s7_owns(":=", symbol) ||
                                "name" %in% names(call$values))) next
                }
                expr <- parent
            }
        }
        if (type == "S7Method" && expr == row) next
        # Only statements in the outer lexical scope declare workspace classes.
        if (length(syntax$function_scope) && syntax$function_scope[[row]] > 0L) next
        binding_name <- if (type == "S7Method") "method<-" else helper
        check_owner <- class_helper_packages[[helper]] == "S7"
        if (!class_top_level_statement(syntax, expr, allow_control = !check_owner)) next
        if (is.null(call$package) && (is_masked(binding_name, symbol) || (check_owner && !s7_owns(binding_name, symbol)))) {
            if (binding) masked[[class_token_text(syntax$text[[binding]])]] <- at(expr)
            next
        }
        alias <- FALSE
        if (helper %in% c("deprecated_class", "deprecated_generic")) {
            # The wrappers forward the class or generic arguments. 'when' and
            # the deprecation controls describe the wrapper, not its constructor.
            tags <- names(call$values)
            if (!"when" %in% tags) next
            if (type == "S7" && "alias" %in% tags) {
                flag <- class_literal_row(syntax, call$values[["alias"]], "NUM_CONST")
                alias <- !flag || syntax$text[[flag]] != "FALSE"
            }
            forward <- !tags %in% c("when", "new", "method", if (type == "S7") "alias" else "new_label")
            replacing <- "new" %in% tags
            call$values <- call$values[forward]
        } else {
            replacing <- FALSE
        }
        args <- class_matched_arguments(call, if (named_bind) setdiff(formal_names, "name") else formal_names)
        if ((!length(args) && length(call$values)) || !is.null(args$where)) next
        literal <- class_literal_row(syntax, switch(type, R6 = args$classname, S7 = args$name,
                S7Generic = args$name, S7Method = args$generic, S4Generic = args$name, S4Method = args$f, args$Class),
            if (type == "S7Method") "SYMBOL" else "STR_CONST")
        if ((type == "S7Method" && (!literal || is.null(args$signature))) ||
                (type == "S7Generic" && (!binding || (!named_bind && is.null(args$name)) ||
                            (!replacing && is.null(args$dispatch_args))))) next
        if (type %in% c("S4Generic", "S4Method")) {
            # The methods helpers return the generic's name. A variable taking
            # that return value is a separate binding from the registered API.
            if (!literal) next
            binding <- 0L
            expr <- row
        }
        if (!literal && !binding) next
        name_row <- if (binding) binding else literal
        name <- class_token_text(syntax$text[[name_row]])
        class_name <- if (literal) class_token_text(syntax$text[[literal]]) else name
        if (identical(helper, "setReplaceMethod")) name <- class_name <- paste0(class_name, "<-")
        kind <- switch(type, S4Generic = , S7Generic = SymbolKind$Function, S4Method = , S7Method = SymbolKind$Method, SymbolKind$Class)
        # A deprecation alias returns the replacement's class identity. It
        # binds a callable constructor but does not declare another class.
        if (alias) {
            type <- "S7Alias"
            kind <- SymbolKind$Function
        }
        member_rows <- member_values <- integer()
        replacements <- integer()
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
            for (i in which(entries$values > 0L & (entries$tags > 0L | type == "S7"))) {
                tag <- entries$tags[[i]]
                value <- entries$values[[i]]
                if (type == "S7") {
                    descriptor <- syntax_call_arguments(syntax, value)
                    if (!is.null(descriptor) && descriptor$name %in% names(class_s7_property_formals) &&
                            (!is.null(descriptor$package) || s7_owns(descriptor$name, value))) {
                        literal_names <- class_s7_property_literals(syntax, descriptor)
                        if (!tag && length(literal_names$name)) tag <- literal_names$name
                        if (isTRUE(literal_names$replacement > 0L)) replacements <- c(replacements, literal_names$replacement)
                    }
                    if (!tag) next
                }
                parts <- syntax_children(syntax, value)
                is_function <- any(syntax$token[parts] == "FUNCTION" | syntax$text[parts] == "\\")
                member_kind <- if (section %in% c("active", "properties")) SymbolKind$Property else if (
                    section == "methods" || (section %in% c("public", "private") && is_function)) SymbolKind$Method else SymbolKind$Field
                detail <- if (type == "S4") "slot" else if (type == "S7") "property" else section
                selection <- at(tag)
                member_name <- class_token_text(syntax$text[[tag]])
                start <- if (entries$tags[[i]]) selection$start else at(value)$start
                members[[length(members) + 1L]] <- document_symbol(member_name, member_kind,
                    range(start, at(value)$end), selection, detail = detail)
                member_rows <- c(member_rows, tag)
                member_values <- c(member_values, value)
            }
        }
        ancestry <- switch(type, R6 = args$inherit, S7 = args$parent, args$contains)
        parents <- character()
        parent_lookup <- NULL
        if (length(ancestry) && ancestry) {
            # S4 contains= names a registry entry; an S7 parent= takes a
            # generator. A same-spelled binding and registered class can
            # be different declarations in the same document.
            kinds <- switch(type, S7 = "SYMBOL", S4 = "STR_CONST", c("STR_CONST", "SYMBOL"))
            bridge <- if (has_bridge && type == "S4") class_literal_row(syntax, ancestry, "SYMBOL") else 0L
            if (bridge) {
                target <- binding_value(bridge)
                if (target && identical(syntax_call_arguments(syntax, target)$name, "S4_contains")) ancestry <- target
            }
            simple <- class_literal_row(syntax, ancestry, kinds)
            if (simple) {
                parents <- class_token_text(syntax$text[[simple]])
            } else {
                values <- syntax_call_arguments(syntax, ancestry)
                if (type %in% c("S4", "RefClass") && identical(values$name, "c")) {
                    rows <- vapply(values$values, function(i) class_literal_row(syntax, i, "STR_CONST"), integer(1L))
                    parents <- class_token_text(syntax$text[rows[rows > 0L]])
                } else if (length(values$values) == 1L) {
                    get_class <- type == "S7" && identical(values$name, "getClass") && identical(values$package, "methods")
                    contains <- type == "S4" && identical(values$name, "S4_contains") &&
                        (identical(values$package, "S7") || (is.null(values$package) && s7_owns("S4_contains", ancestry)))
                    if (get_class || contains) {
                        formal <- if (get_class) "Class" else "class"
                        arg <- class_matched_arguments(values, formal)[[formal]]
                        target <- class_literal_row(syntax, arg, if (get_class) "STR_CONST" else "SYMBOL")
                        if (target) {
                            parents <- class_token_text(syntax$text[[target]])
                            parent_lookup <- list(types = if (get_class) "S4" else "S7", registry = get_class)
                        }
                    }
                }
            }
        }
        hierarchy_range <- if (literal && (type %in% c("S4", "RefClass") || !binding) && kind == SymbolKind$Class) {
            class_literal_range(at(literal), syntax$text[[literal]])
        }
        declarations[[length(declarations) + 1L]] <- list(name = name, class_name = class_name, type = type, kind = kind,
            uri = uri, range = at(expr), selectionRange = at(name_row), hierarchyRange = hierarchy_range,
            name_row = name_row, literal_row = literal,
            binding = binding > 0L, named_bind = named_bind, children = members, member_rows = member_rows, member_values = member_values,
            parents = parents, parent_lookup = parent_lookup)
        if (length(replacements)) {
            declarations[[length(declarations)]]$property_replacements <- replacements
            declarations[[length(declarations)]]$property_parent <- class_literal_row(syntax, args$parent, "SYMBOL")
        }
    }
    # S7's bind operator supplies name= to any function call. Its result
    # need not be a class or generic, and indexing it never evaluates the RHS.
    if (length(binds)) {
        known <- vapply(Filter(function(x) isTRUE(x$named_bind), declarations), `[[`, integer(1L), "name_row")
        if (length(known)) {
            class_rows <- syntax$parent_row[syntax$parent_row[known]]
            binds <- binds[!syntax$parent_row[binds] %in% class_rows]
        }
        bindings <- list()
        for (op in binds) {
            row <- syntax$parent_row[[op]]
            parts <- syntax_children(syntax, row)
            parts <- parts[syntax$token[parts] != "COMMENT"]
            if (length(parts) != 3L) next
            lhs <- class_literal_row(syntax, parts[[1L]], "SYMBOL")
            if (!lhs) next
            if (length(incomplete_lines) && findInterval(syntax$line1[[row]] - 2L, incomplete_lines) !=
                    findInterval(syntax$line2[[row]] - 1L, incomplete_lines)) next
            rhs <- syntax_children(syntax, parts[[3L]])
            rhs <- rhs[syntax$token[rhs] != "COMMENT"]
            if (length(rhs) < 3L || syntax$token[[rhs[[2L]]]] != "'('") next
            equals <- which(syntax$token[rhs] == "EQ_SUB")
            if ("name" %in% class_token_text(syntax$text[rhs[equals - 1L]])) next
            if (!s7_owns(":=", op)) next
            bindings[[length(bindings) + 1L]] <- list(name = class_token_text(syntax$text[[lhs]]),
                type = "variable", kind = SymbolKind$Field, binding = TRUE, named_bind = TRUE, range = at(row))
        }
        if (length(bindings)) attr(declarations, "s7_bindings") <- bindings
    }
    if (length(masked)) attr(declarations, "masked") <- masked
    if (!is.null(s7_scope)) attr(declarations, "source_exports") <- s7_scope$exports
    if (isTRUE(s7_scope$used_exclusions)) attr(declarations, "attachment_rules") <- TRUE
    if (!is.null(s7_scope) && length(uri) && nzchar(uri) && basename(dirname(path_from_uri(uri))) == "R") {
        attr(declarations, "namespace_imports") <- as.list(namespace_imports)
    }
    declarations
}

class_binding_definitions <- function(definitions, classes, document = NULL) {
    masked <- attr(classes, "masked", exact = TRUE)
    for (name in intersect(names(definitions), names(masked))) {
        if (equal_range(definitions[[name]]$range, masked[[name]])) definitions[[name]]$type <- "variable"
    }
    ordinary <- attr(classes, "s7_bindings", exact = TRUE)
    if (length(ordinary)) classes <- c(classes, ordinary)
    for (cls in classes) {
        if (!cls$binding) next
        previous <- definitions[[cls$name]]
        type <- if (cls$kind == SymbolKind$Function) "function" else cls$type
        if (isTRUE(cls$named_bind) && (is.null(previous) || compare_position(previous$range$start, cls$range$start) <= 0L)) {
            definitions[[cls$name]] <- list(name = cls$name, type = type, range = cls$range)
        } else if (!is.null(previous) && equal_range(previous$range, cls$range)) {
            definitions[[cls$name]]$type <- type
        }
    }
    if (!is.null(document) && length(classes)) {
        # Cross-file definitions and completion also consult the workspace's
        # object/function inventories. Replace only the final named binding.
        active <- Filter(function(x) isTRUE(x$named_bind) && equal_range(x$range, definitions[[x$name]]$range), classes)
        keys <- vapply(active, `[[`, "", "name")
        if (length(keys)) {
            callable <- vapply(definitions[keys], function(x) identical(x$type, "function"), logical(1L))
            document$objects <- union(document$objects, keys)
            document$functs <- union(setdiff(document$functs, keys), keys[callable])
            document$nonfuncts <- union(setdiff(document$nonfuncts, keys), keys[!callable])
            document$functions[keys] <- NULL
            document$signatures[keys] <- NULL
        }
    }
    definitions
}

class_workspace_sources <- function(workspace, uris = NULL) {
    documents <- workspace$documents
    indexed <- isTRUE(workspace$index$enabled)
    if (is.null(uris)) uris <- unique(c(if (indexed) workspace$index$summaries$keys(), documents$keys()))
    if (!length(uris)) return(list())
    if (indexed) {
        # A live buffer can use a different URI spelling from the shallow
        # index (or a retained disk document) for the same physical file.
        open <- workspace_s4_open_uris(workspace, NULL, documents$keys())
        if (length(open)) {
            aliases <- unname(open[vapply(uris, index_canonical_uri, character(1L))])
            uris[!is.na(aliases)] <- aliases[!is.na(aliases)]
        }
    }
    lapply(unique(uris), function(uri) {
        document <- documents$get(uri, NULL)
        if (isTRUE(document$is_open) || !indexed) {
            if (!is.null(document)) current_parse_data(uri, workspace, document)
        } else {
            summary <- workspace$index$summaries$get(index_canonical_uri(uri), NULL)
            if (is.null(summary) && !is.null(document)) {
                current_parse_data(uri, workspace, document)
            } else if (!isFALSE(summary$cacheable)) {
                summary
            }
        }
    })
}

class_source_records <- function(data) {
    if (is.null(data$class_data)) data$class_declarations else data$class_data$declarations
}

class_workspace_declarations <- function(workspace, uris = NULL) {
    unlist(lapply(class_workspace_sources(workspace, uris), class_source_records), recursive = FALSE)
}

class_hierarchy_item <- function(cls) {
    selection <- if (is.null(cls$hierarchyRange)) cls$selectionRange else cls$hierarchyRange
    list(name = cls$class_name, uri = cls$uri, range = selection, classType = cls$type, binding = cls$name)
}
