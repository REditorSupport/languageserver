class_index_key <- function(type, name) paste(type, name, sep = "\r")

# Parser XML has no namespaces. xml2's default namespace discovery walks the
# whole document for every local query, turning class extraction quadratic.
class_xml_first <- function(node, xpath) xml_find_first(node, xpath, ns = character())
class_xml_all <- function(node, xpath) xml_find_all(node, xpath, ns = character())

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
                string <- unquote(class_xml_first(call, ".//STR_CONST[1]"))
                lhs <- unquote(class_xml_first(call,
                        "ancestor::expr[LEFT_ASSIGN or EQ_ASSIGN][1]/expr[1]/SYMBOL[1]"))
                names[[i]] <- unique(c(string, lhs))
                param <- class_xml_first(call, ".//SYMBOL_SUB[text() = 'inherit']/following-sibling::expr[1]")
                symbol <- class_xml_first(param, "./SYMBOL | ./expr//SYMBOL")
                if (length(symbol)) {
                    parents[[i]] <- xml_text(symbol)
                } else {
                    string <- class_xml_first(param, "./STR_CONST | ./expr//STR_CONST")
                    parents[[i]] <- unquote(if (length(string)) string else param)
                }
            } else {
                strings <- class_xml_all(call, paste0(".//SYMBOL_FUNCTION_CALL[text() = '", fun,
                        "']/parent::expr/following-sibling::expr", if (fun != "setMethod") "[1]", "//STR_CONST"))
                names[[i]] <- if (fun == "setMethod") {
                    if (length(strings) > 1L) gsub('["\047`]', "", xml_text(strings[-1L])) else character()
                } else if (length(strings)) {
                    unquote(strings[[1L]])
                } else {
                    character()
                }
                param <- class_xml_first(call,
                    "./SYMBOL_SUB[text() = 'contains']/following-sibling::*[not(self::COMMENT)][1][self::EQ_SUB]/following-sibling::expr[1]")
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
