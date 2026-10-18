class_index_key <- function(type, name) paste(type, name, sep = "\r")

# Parser XML has no namespaces. xml2's default namespace discovery walks the
# whole document for every local query, turning class extraction quadratic.
class_xml_first <- function(node, xpath) xml_find_first(node, xpath, ns = character())
class_xml_all <- function(node, xpath) xml_find_all(node, xpath, ns = character())

# Match inert argument indices, retaining each value's original XML location.
# This gives class providers R's named/positional matching without evaluating
# declarations, and keeps piped names and placeholders at their source ranges.
class_xml_arguments <- function(node) {
    callee <- class_xml_first(node, paste0(
        ".//SYMBOL_FUNCTION_CALL[text() = 'R6Class' or text() = 'setClass' or ",
        "text() = 'setRefClass' or text() = 'setMethod'][1]"))
    if (!length(callee)) return(list())
    call <- xml_parent(xml_parent(callee))
    fn <- switch(xml_text(callee), R6Class = R6::R6Class, setClass = methods::setClass,
        setRefClass = methods::setRefClass, setMethod = methods::setMethod)
    groups <- call_argument_groups(call)
    arguments <- lapply(groups, function(group) {
        if (!length(group$nodes)) return(class_xml_first(call, "__missing__"))
        expr <- group$nodes[xml_name(group$nodes) == "expr"]
        if (length(expr)) expr[[1L]] else class_xml_first(call, "__missing__")
    })
    names(arguments) <- vapply(groups, function(group) {
        if (is.null(group$name)) "" else group$name
    }, character(1L))
    operator <- class_xml_first(call, "preceding-sibling::*[not(self::COMMENT)][1]")
    native <- xml_name(operator) %in% c("PIPE", "OP-PIPE")
    magrittr <- xml_name(operator) == "SPECIAL" && xml_text(operator) %in% c("%>%", "%<>%", "%T>%")
    if (native || isTRUE(magrittr)) {
        lhs <- class_xml_first(operator, "preceding-sibling::expr[1]")
        placeholder <- vapply(arguments, function(argument) {
            leaf <- class_xml_first(argument, if (native) "self::expr[count(*) = 1]/PLACEHOLDER" else
                    "self::expr[count(*) = 1]/SYMBOL[text() = '.']")
            length(leaf) > 0L
        }, logical(1L))
        if (any(placeholder)) arguments[placeholder] <- list(lhs)
        else arguments <- c(list(lhs), arguments)
    }
    values <- as.list(seq_along(arguments))
    names(values) <- names(arguments)
    matched <- tryCatch(match.call(fn, as.call(c(list(as.name("class_call")), values)),
            expand.dots = FALSE), error = function(e) NULL)
    values <- as.list(matched)[-1L]
    lapply(values, function(index) {
        if (is.integer(index) && length(index) == 1L) arguments[[index]] else
            class_xml_first(call, "__missing__")
    })
}

class_xml_argument <- function(call, name) {
    value <- class_xml_arguments(call)[[name]]
    if (is.null(value)) class_xml_first(call, "__missing__") else value
}

class_xml_name <- function(call) {
    arguments <- class_xml_arguments(call)
    value <- arguments$classname
    if (is.null(value)) value <- arguments$Class
    if (is.null(value)) return(class_xml_first(call, "__missing__"))
    class_xml_first(value, "./STR_CONST")
}

# The legacy class extractors keep their precise XPath semantics, but all
# document-wide call discovery and name matching happens once in the worker.
class_xml_index <- function(xdoc) {
    functions <- c("R6Class", "setClass", "setRefClass", "setMethod")
    result <- stats::setNames(vector("list", length(functions)), functions)
    unquote <- function(node) {
        if (length(node)) gsub('["\047`]', "", xml_text(node)) else character()
    }
    for (fun in functions) {
        calls <- class_xml_all(xdoc, paste0("//SYMBOL_FUNCTION_CALL[text() = '", fun,
                "']/ancestor::expr[.//OP-LEFT-PAREN][1]"))
        names <- parents <- vector("list", length(calls))
        for (i in seq_along(calls)) {
            call <- calls[[i]]
            if (fun == "R6Class") {
                string <- unquote(class_xml_name(call))
                lhs <- unquote(class_xml_first(call,
                        "ancestor::expr[LEFT_ASSIGN or EQ_ASSIGN][1]/expr[1]/SYMBOL[1]"))
                names[[i]] <- unique(c(string, lhs))
                param <- class_xml_argument(call, "inherit")
                symbol <- class_xml_first(param, "./SYMBOL | ./expr//SYMBOL")
                if (length(symbol)) {
                    parents[[i]] <- xml_text(symbol)
                } else {
                    string <- class_xml_first(param, "./STR_CONST | ./expr//STR_CONST")
                    parents[[i]] <- unquote(if (length(string)) string else param)
                }
            } else {
                names[[i]] <- if (fun == "setMethod") {
                    strings <- class_xml_all(class_xml_argument(call, "signature"), ".//STR_CONST")
                    gsub('["\047`]', "", xml_text(strings))
                } else {
                    unquote(class_xml_name(call))
                }
                param <- class_xml_argument(call, "contains")
                parents[[i]] <- gsub('["\047`]', "", xml_text(class_xml_all(param, ".//STR_CONST")))
            }
        }
        groups <- function(values) {
            names <- unlist(values, use.names = FALSE)
            rows <- rep(seq_along(values), lengths(values))
            keep <- !is.na(names) & nzchar(names)
            list2env(split(rows[keep], names[keep]),
                hash = TRUE, parent = emptyenv())
        }
        result[[fun]] <- list(calls = calls, names = groups(names), parents = groups(parents))
    }
    result
}

#' Select class calls by indexed name or inheritance, with a legacy fallback
#' @noRd
class_xml_calls <- function(xdoc, fun, name = NULL, parent = FALSE) {
    index <- attr(xdoc, "class_index", exact = TRUE)[[fun]]
    if (is.null(index)) {
        return(class_xml_all(xdoc, paste0("//SYMBOL_FUNCTION_CALL[text() = '", fun,
                    "']/ancestor::expr[.//OP-LEFT-PAREN][1]")))
    }
    if (is.null(name)) return(index$calls)
    rows <- get0(name, envir = if (parent) index$parents else index$names,
        inherits = FALSE, ifnotfound = integer())
    index$calls[unique(rows)]
}

#' Serialized class outlines and inheritance facts, without retained XML
#' @noRd
class_parse_data <- function(uri, content, xml_data, syntax, definitions) {
    empty_map <- function() new.env(hash = TRUE, parent = emptyenv())
    result <- list(members = empty_map(), supertypes = empty_map(), subtypes = empty_map(), bytes = 0)
    if (!any(syntax$token == "SYMBOL_FUNCTION_CALL" &
                syntax$text %in% c("R6Class", "setClass", "setRefClass", "setMethod"))) return(result)
    xdoc <- xml2::read_xml(xml_data)
    index <- class_xml_index(xdoc)
    attr(xdoc, "class_index") <- index
    document <- Document$new(uri, content = content)
    for (def in definitions) {
        if (!is.null(def$type) && def$type %in% c("R6", "S4", "RefClass") && !is.null(def$name)) {
            result$members[[class_index_key(def$type, def$name)]] <- tryCatch(
                extract_class_members(document, xdoc, def), error = function(e) NULL)
        }
    }
    functions <- c(R6 = "R6Class", S4 = "setClass", RefClass = "setRefClass")
    supers <- list(R6 = find_r6_supertypes, S4 = find_s4_supertypes, RefClass = find_refclass_supertypes)
    subs <- list(R6 = find_r6_subtypes, S4 = find_s4_subtypes, RefClass = find_refclass_subtypes)
    for (type in names(functions)) {
        calls <- index[[functions[[type]]]]
        for (name in ls(calls$names, all.names = TRUE)) {
            result$supertypes[[class_index_key(type, name)]] <- supers[[type]](document, xdoc, name, uri)
        }
        for (name in ls(calls$parents, all.names = TRUE)) {
            result$subtypes[[class_index_key(type, name)]] <- subs[[type]](document, xdoc, name, uri)
        }
    }
    result$bytes <- sum(vapply(result[c("members", "supertypes", "subtypes")], function(map) {
        as.numeric(utils::object.size(as.list(map)))
    }, numeric(1L)))
    result
}

class_member_symbols <- function(document, parse_data, def) {
    index <- parse_data$class_data
    if (!is.null(index) && !is.null(def$name)) {
        key <- class_index_key(def$type, def$name)
        if (exists(key, envir = index$members, inherits = FALSE)) return(index$members[[key]])
    }
    extract_class_members(document, parse_data_xml(parse_data), def)
}
