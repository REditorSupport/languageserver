#' Prepare type hierarchy information
#'
#' Detects type definitions at the cursor position and returns information about them.
#' Supports S3, S4, RefClass, and R6Class definitions.
#'
#' @noRd
prepare_type_hierarchy_reply <- function(id, uri, workspace, document, point) {
    if (!check_r_region(document, point)) {
        return(Response$new(id, result = NULL))
    }

    token <- document$detect_token(point)

    logger$info("prepare_type_hierarchy_reply: ", list(
        uri = uri,
        token = token
    ))

    result <- NULL

    # Check if token is a type definition
    type_info <- detect_type_definition(uri, workspace, document, point, token$token)

    if (!is.null(type_info)) {
        result <- list(
            list(
                name = type_info$name,
                kind = SymbolKind$Class,
                uri = type_info$uri,
                range = type_info$range,
                selectionRange = type_info$range,
                data = list(
                    definition = type_info,
                    classType = type_info$classType,
                    contextUri = uri
                )
            )
        )
    }

    logger$info("prepare_type_hierarchy_reply result: ", result)

    Response$new(
        id,
        result = result
    )
}

#' Get type hierarchy supertypes
#'
#' Returns the parent types/classes that a given type inherits from.
#'
#' @noRd
type_hierarchy_supertypes_reply <- function(id, workspace, item) {
    logger$info("type_hierarchy_supertypes_reply: ", item$name)

    result <- list()

    if (!is.null(item$data$definition)) {
        supertypes <- find_type_supertypes(workspace, item$data$definition)

        if (length(supertypes) > 0) {
            result <- lapply(supertypes, function(supertype) {
                list(
                    name = supertype$name,
                    kind = SymbolKind$Class,
                    uri = supertype$uri,
                    range = supertype$range,
                    selectionRange = supertype$range,
                    data = list(
                        definition = supertype,
                        classType = supertype$classType,
                        contextUri = item$data$contextUri
                    )
                )
            })
        }
    }

    logger$info("type_hierarchy_supertypes result: ", result)

    Response$new(id, result = result)
}

#' Get type hierarchy subtypes
#'
#' Returns the child types/classes that inherit from a given type.
#'
#' @noRd
type_hierarchy_subtypes_reply <- function(id, workspace, item) {
    logger$info("type_hierarchy_subtypes_reply: ", item$name)

    result <- list()

    if (!is.null(item$data$definition)) {
        context_uri <- item$data$contextUri
        if (is.null(context_uri)) context_uri <- item$uri
        subtypes <- find_type_subtypes(
            workspace, item$data$definition, context_uri = context_uri)

        if (length(subtypes) > 0) {
            result <- lapply(subtypes, function(subtype) {
                list(
                    name = subtype$name,
                    kind = SymbolKind$Class,
                    uri = subtype$uri,
                    range = subtype$range,
                    selectionRange = subtype$range,
                    data = list(
                        definition = subtype,
                        classType = subtype$classType,
                        contextUri = context_uri
                    )
                )
            })
        }
    }

    logger$info("type_hierarchy_subtypes result: ", result)

    Response$new(id, result = result)
}

#' Detect if a symbol is a type/class definition
#'
#' @noRd
detect_type_definition <- function(uri, workspace, document, point, token_text) {
    parsed <- current_parse_data(uri, workspace, document)
    if (is.null(parsed)) return(NULL)
    row <- point$row + 1L
    col <- point$col + 1L
    syntax <- parsed$syntax_data
    if (!is.null(syntax)) {
        focused <- member_navigation_token(parsed, point)
        if (!focused) return(NULL)
        token_name <- syntax$token[[focused]]
        if (!token_name %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "STR_CONST")) return(NULL)
        token_value <- class_token_text(syntax$text[[focused]])
        declarations <- parsed$class_data$declarations
        if (token_value %in% names(attr(declarations, "masked", exact = TRUE)) &&
                identical(parsed$definitions[[token_value]]$type, "variable")) return(NULL)
        for (cls in declarations) {
            if (cls$kind == SymbolKind$Class && focused %in% c(cls$name_row, cls$literal_row)) {
                return(class_hierarchy_item(cls))
            }
        }
        if (token_name %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL") && !is.null(parsed$member_data)) {
            cls <- class_navigation_declaration(uri, workspace, document, point)
            if (!is.null(cls)) return(class_hierarchy_item(cls))
            # A plain source binding can share a name with a class. Only
            # dotted S3 method names still need legacy method detection here.
            bound <- navigation_definitions(parsed, row, col, token_value, token = focused, last = TRUE)
            if (length(bound) && !grepl(".", token_value, fixed = TRUE)) return(NULL)
        }
    }

    xdoc <- parse_data_xml(parsed)
    if (is.null(xdoc)) return(NULL)
    token <- xdoc_find_token(xdoc, row, col)
    if (!length(token) || !xml_name(token) %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "STR_CONST")) return(NULL)
    token_value <- class_token_text(xml_text(token))
    enclosing_scopes <- xdoc_find_enclosing_scopes(xdoc, row, col, top = TRUE)

    # Fallback scan for R6Class definition
    r6_type <- detect_r6class(enclosing_scopes, token_value, document, uri)
    if (!is.null(r6_type)) {
        return(r6_type)
    }

    # Check for setClass (S4)
    s4_type <- detect_s4class(enclosing_scopes, token_value, document, uri)
    if (!is.null(s4_type)) {
        return(s4_type)
    }

    # Check for setRefClass
    refclass_type <- detect_refclass(enclosing_scopes, token_value, document, uri)
    if (!is.null(refclass_type)) {
        return(refclass_type)
    }

    # Check for S3 class method definitions
    s3_type <- detect_s3class(enclosing_scopes, token_value, document, uri)
    if (!is.null(s3_type)) {
        return(s3_type)
    }

    NULL
}

#' Detect R6Class definitions
#'
#' Matches patterns like: ClassName <- R6::R6Class(...)
#'
#' @noRd
detect_r6class <- function(scopes, token_text, document, uri) {
    # Look for R6Class pattern - simpler approach
    token_quote <- xml_single_quote(token_text)

    # Pattern: name <- R6::R6Class(...)
    xpath <- glue(
        "(//LEFT_ASSIGN | //EQ_ASSIGN)[
            preceding-sibling::expr[count(*) = 1]/SYMBOL[text() = '{token_quote}']
            and following-sibling::expr[1]//SYMBOL_FUNCTION_CALL[text() = 'R6Class']
        ]/parent::*",
        token_quote = token_quote
    )

    defs <- class_xml_all(scopes, xpath)
    if (length(defs) > 0) {
        defn <- defs[[1]]
        range_info <- get_element_range(document, defn)
        if (!is.null(range_info)) {
            return(list(
                name = token_text,
                uri = uri,
                range = range_info,
                classType = "R6"
            ))
        }
    }

    detect_class_literal(scopes, token_text, document, uri, "R6Class", "R6")
}

# Resolve the matched classname/Class argument, including its original pipe-LHS
# location. String literals in other arguments must not become class names.
detect_class_literal <- function(scopes, token_text, document, uri, fun, type) {
    for (call in class_xml_calls(scopes, fun, token_text)) {
        node <- class_xml_name(call)
        if (!length(node) || class_token_text(xml_text(node)) != token_text) next
        range_info <- get_element_range(document, node)
        if (!is.null(range_info)) return(list(
            name = token_text, uri = uri, range = range_info, classType = type))
    }
    NULL
}

#' Detect S4 class definitions (setClass)
#'
#' Matches patterns like: setClass("ClassName", ...)
#'
#' @noRd
detect_s4class <- function(scopes, token_text, document, uri) {
    detect_class_literal(scopes, token_text, document, uri, "setClass", "S4")
}

#' Detect RefClass definitions (setRefClass)
#'
#' Matches patterns like: setRefClass("ClassName", ...)
#'
#' @noRd
detect_refclass <- function(scopes, token_text, document, uri) {
    detect_class_literal(scopes, token_text, document, uri, "setRefClass", "RefClass")
}

#' Detect S3 class method definitions
#'
#' Matches patterns like: method.ClassName <- function(...) or
#' setMethod("generic", "ClassName", ...) for S4 methods
#'
#' @noRd
detect_s3class <- function(scopes, token_text, document, uri) {
    # Pattern: method.ClassName <- function(...)
    # Extract ClassName from method.ClassName
    parts <- strsplit(token_text, "\\.")[[1L]]
    if (length(parts) >= 2L) {
        token_quote <- xml_single_quote(token_text)
        defs <- class_xml_all(scopes, paste0("(//LEFT_ASSIGN | //EQ_ASSIGN)[",
                "preceding-sibling::expr[count(*)=1]/SYMBOL[text()='", token_quote,
                "'] and following-sibling::expr[1]/FUNCTION]/parent::*"))
        if (length(defs)) {
            # A generic or a class name can itself contain dots. Use the longest
            # recognized generic prefix rather than splitting off the last word.
            for (n in rev(seq_len(length(parts) - 1L))) {
                generic <- paste(parts[seq_len(n)], collapse = ".")
                standard <- any(vapply(intersect(startup_packages, loadedNamespaces()), function(pkg) {
                    fn <- get0(generic, asNamespace(pkg), inherits = FALSE)
                    is.function(fn) && (generic %in% .S3PrimitiveGenerics ||
                            (!is.primitive(fn) && "UseMethod" %in% all.names(body(fn))))
                }, logical(1L)))
                declared <- any(class_token_text(xml_text(class_xml_all(scopes,
                                "//SYMBOL_FUNCTION_CALL[text()='UseMethod']/parent::expr/following-sibling::expr[1]/STR_CONST"))) == generic)
                if (standard || declared) return(list(name = paste(parts[-seq_len(n)], collapse = "."),
                        uri = uri, range = get_element_range(document, defs[[1L]]), classType = "S3"))
            }
        }
    }
    for (call in class_xml_calls(scopes, "setMethod", token_text)) {
        signature <- class_xml_argument(call, "signature")
        for (literal in class_xml_all(signature, ".//STR_CONST")) {
            if (identical(class_token_text(xml_text(literal)), token_text)) return(list(
                name = token_text, uri = uri, range = get_element_range(document, literal), classType = "S4"))
        }
    }

    NULL
}

class_parent_lookup <- function(child) {
    if (!is.null(child$parent_lookup)) return(child$parent_lookup)
    list(types = if (child$type == "S7") c("S7", "S4") else child$type,
        registry = child$type %in% c("S4", "RefClass"))
}

# An ordinary assignment to the generator name can shadow its declaration.
# Check the lexical definition in each file visible to the child, including
# shallow source summaries for files that have not been opened.
class_hierarchy_parent_bindings <- function(workspace, uri, lookup, name) {
    sources <- class_workspace_sources(workspace, reference_source_scope(workspace, uri))
    candidates <- list()
    for (source in sources) {
        declarations <- class_source_records(source)
        if (lookup$registry) {
            # contains= names a registered class, independent of any ordinary
            # variable (including the generator binding) with that name.
            declarations <- Filter(function(x) x$type %in% lookup$types && x$class_name == name, declarations)
        } else {
            declarations <- Filter(function(x) x$type %in% lookup$types && x$binding && x$name == name, declarations)
            bound <- source$definitions[[name]]
            if (!is.null(bound) && !any(vapply(declarations, function(x) equal_range(x$range, bound$range), logical(1L)))) {
                return(list(known = TRUE, candidates = list()))
            }
        }
        candidates <- c(candidates, declarations)
    }
    list(known = length(candidates) > 0L, candidates = candidates)
}

# A generator's binding and its declared class name can differ. The location
# in the hierarchy item disambiguates different generators named e.g. "Node".
class_hierarchy_relatives <- function(workspace, item, parents) {
    if (!item$classType %in% c("R6", "S7", "S4", "RefClass") || is.null(workspace$documents)) return(NULL)
    # Range-less S4/RefClass items use the existing name-based class index.
    if (is.null(item$range) && item$classType %in% c("S4", "RefClass")) return(NULL)
    own <- Filter(function(x) x$type == item$classType && item$name %in% c(x$name, x$class_name),
        class_workspace_declarations(workspace, item$uri))
    if (!is.null(item$binding)) {
        own <- Filter(function(x) x$name == item$binding && equal_range(class_hierarchy_item(x)$range, item$range), own)
        if (!length(own)) return(list()) # the declaration was edited or removed
    }
    if (!length(own)) return(NULL) # legacy XML-only snapshots
    if (length(own) > 1L) {
        own <- Filter(function(x) !is.null(item$range) && equal_range(class_hierarchy_item(x)$range, item$range), own)
        if (length(own) != 1L) return(list())
    }
    cls <- own[[1L]]
    if (parents) {
        if (!length(cls$parents)) return(list())
        legacy <- workspace$get_parse_data(cls$uri)$class_data$supertypes
        found <- lapply(unique(cls$parents), function(name) {
            found <- class_hierarchy_parent_bindings(workspace, cls$uri, class_parent_lookup(cls), name)
            if (length(found$candidates) == 1L) return(class_hierarchy_item(found$candidates[[1L]]))
            if (found$known || !is.environment(legacy)) return(NULL)
            # Preserve the source reference in inherit= for external parents.
            key <- class_index_key(cls$type, cls$class_name)
            fallback <- Filter(function(x) x$name == name, get0(key, legacy, inherits = FALSE))
            if (length(fallback)) fallback[[1L]]
        })
        return(Filter(Negate(is.null), found))
    }

    uris <- reference_source_scope(workspace, cls$uri, callers = TRUE)
    children <- Filter(function(child) {
        if (!length(child$parents)) return(FALSE)
        lookup <- class_parent_lookup(child)
        name <- if (lookup$registry) cls$class_name else cls$name
        cls$type %in% lookup$types && name %in% child$parents
    }, class_workspace_declarations(workspace, uris))
    scopes <- new.env(hash = TRUE, parent = emptyenv())
    related <- Filter(function(child) {
        # Package members share one scope. Scripts have their own source graph,
        # which can include a second, unrelated parent with the same binding.
        index <- workspace$index
        scope <- if (isTRUE(index$enabled)) index$package_root_for_uri(child$uri)
        if (is.null(scope)) scope <- if (isTRUE(index$enabled)) child$uri else "workspace"
        lookup <- class_parent_lookup(child)
        parent_name <- if (lookup$registry) cls$class_name else cls$name
        key <- paste(c(lookup$registry, lookup$types, scope), collapse = ":")
        if (!exists(key, scopes, inherits = FALSE)) {
            candidates <- class_hierarchy_parent_bindings(workspace, child$uri, lookup, parent_name)$candidates
            scopes[[key]] <- length(candidates) == 1L && candidates[[1L]]$uri == cls$uri &&
                equal_range(candidates[[1L]]$selectionRange, cls$selectionRange)
        }
        scopes[[key]]
    }, children)
    lapply(related, class_hierarchy_item)
}

#' Find supertypes (parent types) of a given type
#'
#' @noRd
find_type_supertypes <- function(workspace, type_def) {
    catalog <- class_hierarchy_relatives(workspace, type_def, parents = TRUE)
    if (!is.null(catalog)) return(catalog)
    cache_key <- paste(
        "super", type_def$uri, type_def$classType, type_def$name,
        sep = "\r")
    if (!is.null(workspace$type_hierarchy_cache) &&
            workspace$type_hierarchy_cache$has(cache_key)) {
        return(workspace$type_hierarchy_cache$get(cache_key))
    }
    supertypes <- list()

    # Get the document where the type is defined
    doc <- workspace$documents$get(type_def$uri, NULL)
    if (is.null(doc) && !type_def$classType %in% c("R6", "S7")) {
        return(supertypes)
    }

    class_type <- type_def$classType
    parse_data <- workspace$get_parse_data(type_def$uri)
    if (is.null(doc) || class_type == "S7") {
        # S7 and shallow-only declarations use the source catalog below.
    } else if (!is.null(parse_data$class_data) && class_type %in% c("R6", "S4", "RefClass", "S3")) {
        supertypes <- get0(class_index_key(class_type, type_def$name),
            envir = parse_data$class_data$supertypes, inherits = FALSE, ifnotfound = list())
    } else {
        xdoc <- parse_data_xml(parse_data)
        if (is.null(xdoc)) return(supertypes)

        if (class_type == "R6") {
            supertypes <- find_r6_supertypes(doc, xdoc, type_def$name, type_def$uri)
        } else if (class_type == "S4") {
            supertypes <- find_s4_supertypes(doc, xdoc, type_def$name, type_def$uri)
        } else if (class_type == "RefClass") {
            supertypes <- find_refclass_supertypes(doc, xdoc, type_def$name, type_def$uri)
        } else if (class_type == "S3") {
            supertypes <- find_s3_supertypes(doc, xdoc, type_def$name, type_def$uri)
        }
    }

    # Final deduplication by class name
    if (length(supertypes) > 0) {
        seen_names <- character()
        unique_supertypes <- list()
        for (supertype in supertypes) {
            if (!supertype$name %in% seen_names) {
                seen_names <- c(seen_names, supertype$name)
                unique_supertypes <- c(unique_supertypes, list(supertype))
            }
        }
        supertypes <- unique_supertypes
    }

    if (!is.null(workspace$type_hierarchy_cache)) {
        workspace$type_hierarchy_cache$set(cache_key, supertypes)
    }
    supertypes
}

#' Find R6 supertypes (inherit parameter)
#'
#' @noRd
find_r6_supertypes <- function(doc, xdoc, class_name, uri) {
    supertypes <- list()

    # Find full R6Class call expressions (handle namespaced calls like R6::R6Class)
    all_class_defs <- class_xml_calls(xdoc, "R6Class", class_name)

    for (class_def in all_class_defs) {
        class_str <- class_xml_name(class_def)
        class_symbol <- class_xml_first(class_def,
            "ancestor::expr[LEFT_ASSIGN or EQ_ASSIGN][1]/expr[1]/SYMBOL[1]")
        class_name_value <- NULL
        if (length(class_str)) {
            class_name_value <- class_token_text(xml_text(class_str))
        } else if (length(class_symbol)) {
            class_name_value <- class_token_text(xml_text(class_symbol))
        }
        if (is.null(class_name_value) || class_name_value != class_name) next

        inherit_param <- class_xml_argument(class_def, "inherit")

        if (length(inherit_param) > 0) {
            # Extract class name from SYMBOL or STR_CONST within the expr
            inherit_symbol <- class_xml_first(inherit_param, "./SYMBOL | ./expr//SYMBOL")
            if (length(inherit_symbol)) {
                inherit_name <- class_token_text(xml_text(inherit_symbol))
            } else {
                inherit_str <- class_xml_first(inherit_param, "./STR_CONST | ./expr//STR_CONST")
                if (length(inherit_str)) {
                    inherit_name <- class_token_text(xml_text(inherit_str))
                } else {
                    inherit_name <- class_token_text(xml_text(inherit_param))
                }
            }

            range_info <- get_element_range(doc, inherit_param)
            if (!is.null(range_info)) {
                supertypes <- c(supertypes, list(list(
                    name = inherit_name,
                    uri = uri,
                    range = range_info,
                    classType = "R6"
                )))
            }
        }
    }

    # Deduplicate by class name
    if (length(supertypes) > 0) {
        seen_names <- character()
        unique_supertypes <- list()
        for (supertype in supertypes) {
            if (!supertype$name %in% seen_names) {
                seen_names <- c(seen_names, supertype$name)
                unique_supertypes <- c(unique_supertypes, list(supertype))
            }
        }
        supertypes <- unique_supertypes
    }

    supertypes
}

#' Find S4 supertypes (contains parameter in setClass)
#'
#' @noRd
find_s4_supertypes <- function(doc, xdoc, class_name, uri) {
    supertypes <- list()

    # Look for setClass calls with this class name
    all_setclass_calls <- class_xml_calls(xdoc, "setClass", class_name)

    for (setclass_call in all_setclass_calls) {
        # Get the first string constant (the class name)
        first_str <- class_xml_name(setclass_call)

        if (!length(first_str)) next
        call_class_name <- class_token_text(xml_text(first_str))

        if (call_class_name != class_name) next

        # Now find the contains parameter
        contains_param <- class_xml_argument(setclass_call, "contains")

        if (length(contains_param) > 0) {
            # Could contain one or more class names as strings
            parent_strs <- class_xml_all(contains_param, ".//STR_CONST")
            for (parent_str in parent_strs) {
                parent_name <- class_token_text(xml_text(parent_str))
                range_info <- get_element_range(doc, parent_str)
                if (!is.null(range_info)) {
                    supertypes <- c(supertypes, list(list(
                        name = parent_name,
                        uri = uri,
                        range = range_info,
                        classType = "S4"
                    )))
                }
            }
        }
    }

    supertypes
}

#' Find RefClass supertypes (contains parameter in setRefClass)
#'
#' @noRd
find_refclass_supertypes <- function(doc, xdoc, class_name, uri) {
    supertypes <- list()

    # Look for setRefClass calls with this class name
    all_setrefclass_calls <- class_xml_calls(xdoc, "setRefClass", class_name)

    for (setrefclass_call in all_setrefclass_calls) {
        # Get the first string constant (the class name)
        first_str <- class_xml_name(setrefclass_call)

        if (!length(first_str)) next
        call_class_name <- class_token_text(xml_text(first_str))

        if (call_class_name != class_name) next

        # Now find the contains parameter
        contains_param <- class_xml_argument(setrefclass_call, "contains")

        if (length(contains_param) > 0) {
            parent_strs <- class_xml_all(contains_param, ".//STR_CONST")
            for (parent_str in parent_strs) {
                parent_name <- class_token_text(xml_text(parent_str))
                range_info <- get_element_range(doc, parent_str)
                if (!is.null(range_info)) {
                    supertypes <- c(supertypes, list(list(
                        name = parent_name,
                        uri = uri,
                        range = range_info,
                        classType = "RefClass"
                    )))
                }
            }
        }
    }

    supertypes
}

#' Find S3 supertypes (class inheritance)
#'
#' @noRd
find_s3_supertypes <- function(doc, xdoc, class_name, uri) {
    supertypes <- list()

    # For S3, supertypes are typically implied through method resolution
    # We can look for inherits() calls with this class
    # or look at class() assignments with c(..., class_name)

    # This is more complex for S3, so we return empty for now
    # A full implementation would require deeper analysis

    supertypes
}

#' Find subtypes (child types) that inherit from a given type
#'
#' @noRd
find_type_subtypes <- function(workspace, type_def, context_uri = type_def$uri) {
    catalog <- class_hierarchy_relatives(workspace, type_def, parents = FALSE)
    if (!is.null(catalog)) return(catalog)
    cache_key <- if (identical(context_uri, type_def$uri)) {
        paste("sub", type_def$uri, type_def$classType, type_def$name,
            sep = "\r")
    } else {
        paste(
            "sub", context_uri, type_def$uri, type_def$classType, type_def$name,
            sep = "\r")
    }
    if (!is.null(workspace$type_hierarchy_cache) &&
            workspace$type_hierarchy_cache$has(cache_key)) {
        return(workspace$type_hierarchy_cache$get(cache_key))
    }
    subtypes <- list()

    class_type <- type_def$classType
    parent_name <- type_def$name

    # Search through all documents for classes that inherit from this one
    uris <- workspace_document_uris(workspace, context_uri)
    for (doc_uri in if (class_type != "S7") uris) {
        doc <- workspace$documents$get(doc_uri, NULL)
        if (is.null(doc)) next
        parse_data <- workspace$get_parse_data(doc_uri)
        if (!is.null(parse_data$class_data) && class_type %in% c("R6", "S4", "RefClass", "S3")) {
            found_subtypes <- get0(class_index_key(class_type, parent_name),
                envir = parse_data$class_data$subtypes, inherits = FALSE, ifnotfound = list())
            subtypes <- c(subtypes, found_subtypes)
            next
        }
        xdoc <- parse_data_xml(parse_data)

        if (is.null(xdoc)) {
            next
        }

        if (class_type == "R6") {
            found_subtypes <- find_r6_subtypes(doc, xdoc, parent_name, doc_uri)
        } else if (class_type == "S4") {
            found_subtypes <- find_s4_subtypes(doc, xdoc, parent_name, doc_uri)
        } else if (class_type == "RefClass") {
            found_subtypes <- find_refclass_subtypes(doc, xdoc, parent_name, doc_uri)
        } else if (class_type == "S3") {
            found_subtypes <- find_s3_subtypes_child(doc, xdoc, parent_name, doc_uri)
        } else {
            found_subtypes <- list()
        }

        subtypes <- c(subtypes, found_subtypes)
    }

    # Final deduplication by class name across all documents
    if (length(subtypes) > 0) {
        seen_names <- character()
        unique_subtypes <- list()
        for (subtype in subtypes) {
            if (!subtype$name %in% seen_names) {
                seen_names <- c(seen_names, subtype$name)
                unique_subtypes <- c(unique_subtypes, list(subtype))
            }
        }
        subtypes <- unique_subtypes
    }

    if (!is.null(workspace$type_hierarchy_cache)) {
        workspace$type_hierarchy_cache$set(cache_key, subtypes)
    }
    subtypes
}

#' Find R6 subtypes
#'
#' @noRd
find_r6_subtypes <- function(doc, xdoc, parent_name, uri) {
    subtypes <- list()

    # Find full R6Class call expressions (handle namespaced calls like R6::R6Class)
    all_class_defs <- class_xml_calls(xdoc, "R6Class", parent_name, parent = TRUE)

    for (class_def in all_class_defs) {
        inherit_param <- class_xml_argument(class_def, "inherit")
        if (!length(inherit_param)) next

        # Extract class name from SYMBOL or STR_CONST within the expr
        inherit_symbol_name <- class_xml_first(inherit_param, "./SYMBOL | ./expr//SYMBOL")
        if (length(inherit_symbol_name)) {
            inherit_name <- class_token_text(xml_text(inherit_symbol_name))
        } else {
            inherit_str <- class_xml_first(inherit_param, "./STR_CONST | ./expr//STR_CONST")
            if (length(inherit_str)) {
                inherit_name <- class_token_text(xml_text(inherit_str))
            } else {
                inherit_name <- class_token_text(xml_text(inherit_param))
            }
        }

        if (inherit_name != parent_name) next

        # Extract the actual class name
        class_str <- class_xml_name(class_def)
        if (length(class_str)) {
            class_name <- class_token_text(xml_text(class_str))
            range_info <- get_element_range(doc, class_str)
        } else {
            # Try to find from LHS of assignment
            class_sym <- class_xml_first(class_def,
                "ancestor::expr/expr[1]/SYMBOL")
            if (length(class_sym)) {
                class_name <- class_token_text(xml_text(class_sym))
                range_info <- get_element_range(doc, class_sym)
            } else {
                next
            }
        }

        if (!is.null(range_info)) {
            subtypes <- c(subtypes, list(list(
                name = class_name,
                uri = uri,
                range = range_info,
                classType = "R6"
            )))
        }
    }

    # Deduplicate by class name
    if (length(subtypes) > 0) {
        seen_names <- character()
        unique_subtypes <- list()
        for (subtype in subtypes) {
            if (!subtype$name %in% seen_names) {
                seen_names <- c(seen_names, subtype$name)
                unique_subtypes <- c(unique_subtypes, list(subtype))
            }
        }
        subtypes <- unique_subtypes
    }

    subtypes
}

#' Find S4 subtypes
#'
#' @noRd
find_s4_subtypes <- function(doc, xdoc, parent_name, uri) {
    subtypes <- list()

    # Look for all setClass calls that have contains = parent_name
    all_setclass_calls <- class_xml_calls(xdoc, "setClass", parent_name, parent = TRUE)

    for (setclass_call in all_setclass_calls) {
        # Check if this class contains parent_name
        contains_param <- class_xml_argument(setclass_call, "contains")

        if (!length(contains_param)) next

        # Check if parent_name is in the contains parameter
        parent_strs <- class_xml_all(contains_param, ".//STR_CONST")
        found_parent <- FALSE
        for (parent_str in parent_strs) {
            parent_text <- class_token_text(xml_text(parent_str))
            if (parent_text == parent_name) {
                found_parent <- TRUE
                break
            }
        }
        if (!found_parent) next

        # Get the class name from the first string constant in the setClass call
        class_str <- class_xml_name(setclass_call)

        if (length(class_str)) {
            class_name <- class_token_text(xml_text(class_str))
            range_info <- get_element_range(doc, class_str)
            if (!is.null(range_info)) {
                subtypes <- c(subtypes, list(list(
                    name = class_name,
                    uri = uri,
                    range = range_info,
                    classType = "S4"
                )))
            }
        }
    }

    subtypes
}

#' Find RefClass subtypes
#'
#' @noRd
find_refclass_subtypes <- function(doc, xdoc, parent_name, uri) {
    subtypes <- list()

    # Look for all setRefClass calls that have contains = parent_name
    all_setrefclass_calls <- class_xml_calls(xdoc, "setRefClass", parent_name, parent = TRUE)

    for (setrefclass_call in all_setrefclass_calls) {
        # Check if this class contains parent_name
        contains_param <- class_xml_argument(setrefclass_call, "contains")

        if (!length(contains_param)) next

        # Check if parent_name is in the contains parameter
        parent_strs <- class_xml_all(contains_param, ".//STR_CONST")
        found_parent <- FALSE
        for (parent_str in parent_strs) {
            parent_text <- class_token_text(xml_text(parent_str))
            if (parent_text == parent_name) {
                found_parent <- TRUE
                break
            }
        }
        if (!found_parent) next

        # Get the class name from the first string constant in the setRefClass call
        class_str <- class_xml_name(setrefclass_call)

        if (length(class_str)) {
            class_name <- class_token_text(xml_text(class_str))
            range_info <- get_element_range(doc, class_str)
            if (!is.null(range_info)) {
                subtypes <- c(subtypes, list(list(
                    name = class_name,
                    uri = uri,
                    range = range_info,
                    classType = "RefClass"
                )))
            }
        }
    }

    subtypes
}

find_s3_subtypes_child <- function(doc, xdoc, parent_name, uri) {
    subtypes <- list()

    # For S3, look for class assignments and method definitions
    # This is complex and would require deeper semantic analysis

    subtypes
}

#' Helper function to get element range in LSP format
#'
#' @noRd
get_element_range <- function(document, element) {
    if (!length(element)) {
        return(NULL)
    }

    tryCatch({
        line1 <- as.integer(xml_attr(element, "line1"))
        col1 <- as.integer(xml_attr(element, "col1"))
        line2 <- as.integer(xml_attr(element, "line2"))
        col2 <- as.integer(xml_attr(element, "col2"))

        if (any(is.na(c(line1, col1, line2, col2)))) {
            return(NULL)
        }

        selection <- range(
            start = document$to_lsp_position(row = line1 - 1, col = col1 - 1),
            end = document$to_lsp_position(row = line2 - 1, col = col2)
        )
        if (xml_name(element) == "STR_CONST") class_literal_range(selection, xml_text(element)) else selection
    }, error = function(e) {
        logger$info("Error getting element range: ", e)
        NULL
    })
}

#' Extract class members from a class definition
#'
#' Extracts public and private fields and methods for R6, S4, and RefClass definitions.
#'
#' @param document The document object
#' @param xdoc The parsed XML document
#' @param def The class definition with name, range, and classType
#' @return A list of document symbols representing class members
#' @noRd
extract_class_members <- function(document, xdoc, def) {
    class_type <- def$type
    if (is.null(class_type) || !class_type %in% c("R6", "S4", "RefClass")) {
        return(NULL)
    }

    if (class_type == "R6") {
        return(extract_r6_members(document, xdoc, def))
    } else if (class_type == "S4") {
        return(extract_s4_members(document, xdoc, def))
    } else if (class_type == "RefClass") {
        return(extract_refclass_members(document, xdoc, def))
    }

    NULL
}

#' Extract R6 class members (public and private)
#'
#' @noRd
extract_r6_members <- function(document, xdoc, def) {
    members <- list()
    class_name <- def$name

    # Find the R6Class call for this class
    all_class_defs <- class_xml_calls(xdoc, "R6Class", class_name)

    for (class_def in all_class_defs) {
        # Verify this is the right class
        # First try to get class name from the string argument
        class_str <- class_xml_name(class_def)
        class_name_value <- NULL
        if (length(class_str)) {
            class_name_value <- class_token_text(xml_text(class_str))
        }

        # If not found or doesn't match, try to get from left side of assignment
        if (is.null(class_name_value) || class_name_value != class_name) {
            # Navigate up to find the assignment expression
            assign_expr <- class_xml_first(class_def,
                "ancestor::expr[LEFT_ASSIGN or EQ_ASSIGN][1]")
            if (length(assign_expr)) {
                # Get the symbol on the left side of the assignment
                class_symbol <- class_xml_first(assign_expr,
                    "./expr[1]/SYMBOL[1]")
                if (length(class_symbol)) {
                    class_name_value <- class_token_text(xml_text(class_symbol))
                }
            }
        }

        if (is.null(class_name_value) || class_name_value != class_name) {
            next
        }

        for (access in c("public", "private", "active")) {
            members_node <- class_xml_argument(class_def, access)
            if (length(members_node)) {
                members <- c(members, extract_r6_list_members(document, members_node, access))
            }
        }

        break
    }

    members
}

#' Extract members from an R6 list (public or private)
#'
#' @noRd
extract_r6_list_members <- function(document, list_node, access_modifier) {
    members <- list()

    # Only direct argument labels belong to this section. A field can itself
    # contain arbitrarily nested lists or functions.
    if (!xml_find_lgl_(list_node, "boolean(./expr/SYMBOL_FUNCTION_CALL[text()='list'])")) return(members)
    labels <- class_xml_all(list_node,
        "./EQ_SUB/preceding-sibling::*[not(self::COMMENT)][1][self::SYMBOL_SUB or self::STR_CONST]")
    for (label in labels) {
        value <- class_xml_first(label, "following-sibling::expr[1]")
        if (!length(value)) next
        kind <- if (access_modifier == "active") SymbolKind$Property else if (
            xml_find_lgl_(value, "boolean(./FUNCTION | ./OP-LAMBDA)")) SymbolKind$Method else SymbolKind$Field
        selected <- get_element_range(document, label)
        body <- get_element_range(document, value)
        if (!is.null(selected) && !is.null(body)) members[[length(members) + 1L]] <- document_symbol(
            class_token_text(xml_text(label)), kind, range(selected$start, body$end), selected, detail = access_modifier)
    }

    members
}

#' Extract S4 class members (slots and methods)
#'
#' @noRd
extract_s4_members <- function(document, xdoc, def) {
    members <- list()
    class_name <- def$name

    # Look for setClass calls with this class name
    all_setclass_calls <- class_xml_calls(xdoc, "setClass", class_name)

    for (setclass_call in all_setclass_calls) {
        # Get the first string constant (the class name)
        first_str <- class_xml_name(setclass_call)

        if (!length(first_str)) next
        call_class_name <- class_token_text(xml_text(first_str))

        if (call_class_name != class_name) next

        # Extract slots/representation
        slots_node <- class_xml_argument(setclass_call, "slots")
        if (!length(slots_node)) slots_node <- class_xml_argument(setclass_call, "representation")

        if (length(slots_node)) {
            # Find all named slots
            slot_names <- class_xml_all(slots_node,
                ".//EQ_SUB/preceding-sibling::*[not(self::COMMENT)][1][self::SYMBOL_SUB or self::STR_CONST]")
            for (slot_name_node in slot_names) {
                slot_name_text <- xml_text(slot_name_node)
                slot_name <- class_token_text(slot_name_text)
                slot_range <- get_element_range(document, slot_name_node)

                if (!is.null(slot_range) && nzchar(slot_name)) {
                    members <- c(members, list(document_symbol(
                        name = slot_name,
                        detail = "slot",
                        kind = SymbolKind$Field,
                        range = slot_range,
                        selectionRange = slot_range
                    )))
                }
            }
        }

        break
    }

    # Look for methods defined for this class using setMethod
    all_setmethod_calls <- class_xml_calls(xdoc, "setMethod", class_name)

    for (setmethod_call in all_setmethod_calls) {
        # Check if this method is for our class
        class_strs <- class_xml_all(
            setmethod_call,
            ".//SYMBOL_FUNCTION_CALL[text() = 'setMethod']/parent::expr/following-sibling::expr//STR_CONST"
        )

        found_class <- FALSE
        method_name <- NULL
        for (i in seq_along(class_strs)) {
            str_value <- class_token_text(xml_text(class_strs[[i]]))
            if (i == 1) {
                method_name <- str_value
            } else if (str_value == class_name) {
                found_class <- TRUE
                break
            }
        }

        if (found_class && !is.null(method_name)) {
            method_range <- get_element_range(document, class_strs[[1]])
            if (!is.null(method_range)) {
                members <- c(members, list(document_symbol(
                    name = method_name,
                    detail = "method",
                    kind = SymbolKind$Method,
                    range = method_range,
                    selectionRange = method_range
                )))
            }
        }
    }

    members
}

#' Extract RefClass members (fields and methods)
#'
#' @noRd
extract_refclass_members <- function(document, xdoc, def) {
    members <- list()
    class_name <- def$name

    # Look for setRefClass calls with this class name
    all_setrefclass_calls <- class_xml_calls(xdoc, "setRefClass", class_name)

    for (setrefclass_call in all_setrefclass_calls) {
        # Get the first string constant (the class name)
        first_str <- class_xml_name(setrefclass_call)

        if (!length(first_str)) next
        call_class_name <- class_token_text(xml_text(first_str))

        if (call_class_name != class_name) next

        # Extract fields
        fields_node <- class_xml_argument(setrefclass_call, "fields")

        if (length(fields_node)) {
            field_names <- class_xml_all(fields_node,
                "./EQ_SUB/preceding-sibling::*[not(self::COMMENT)][1][self::SYMBOL_SUB or self::STR_CONST]")
            for (field_name_node in field_names) {
                field_name <- class_token_text(xml_text(field_name_node))
                field_range <- get_element_range(document, field_name_node)

                if (!is.null(field_range)) {
                    members <- c(members, list(document_symbol(
                        name = field_name,
                        detail = "field",
                        kind = SymbolKind$Field,
                        range = field_range,
                        selectionRange = field_range
                    )))
                }
            }
        }

        # Extract methods
        methods_node <- class_xml_first(
            setrefclass_call,
            "./SYMBOL_SUB[text() = 'methods']/following-sibling::*[not(self::COMMENT)][1][self::EQ_SUB]/following-sibling::expr[1]"
        )

        if (length(methods_node)) {
            method_names <- class_xml_all(methods_node,
                "./EQ_SUB/preceding-sibling::*[not(self::COMMENT)][1][self::SYMBOL_SUB or self::STR_CONST]")
            for (method_name_node in method_names) {
                method_name <- class_token_text(xml_text(method_name_node))
                method_range <- get_element_range(document, method_name_node)

                if (!is.null(method_range)) {
                    members <- c(members, list(document_symbol(
                        name = method_name,
                        detail = "method",
                        kind = SymbolKind$Method,
                        range = method_range,
                        selectionRange = method_range
                    )))
                }
            }
        }

        break
    }

    members
}
