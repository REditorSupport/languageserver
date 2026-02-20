#' Prepare type hierarchy information
#'
#' Detects type definitions at the cursor position and returns information about them.
#' Supports S3, S4, RefClass, and R6Class definitions.
#'
#' @noRd
prepare_type_hierarchy_reply <- function(id, uri, workspace, document, point) {
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
                    classType = type_info$classType
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
                        classType = supertype$classType
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
        subtypes <- find_type_subtypes(workspace, item$data$definition)

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
                        classType = subtype$classType
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
    xdoc <- workspace$get_parse_data(uri)$xml_doc
    if (is.null(xdoc)) {
        return(NULL)
    }

    row <- point$row + 1
    col <- point$col + 1

    token <- xdoc_find_token(xdoc, row, col)
    if (!length(token)) {
        return(NULL)
    }

    token_name <- xml_name(token)

    token_value <- token_text
    if (!nzchar(token_value)) {
        token_value <- xml_text(token)
    }
    if (token_name == "STR_CONST") {
        token_value <- gsub('["\'`]', "", token_value)
    }

    # Only process SYMBOL, SYMBOL_FUNCTION_CALL, or STR_CONST
    if (!(token_name %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "STR_CONST"))) {
        return(NULL)
    }

    enclosing_scopes <- xdoc_find_enclosing_scopes(xdoc, row, col, top = TRUE)

    # Check for R6Class definition using token context first
    if (token_name %in% c("SYMBOL", "STR_CONST")) {
        r6_expr <- xml_find_first(token,
            "ancestor::expr[.//SYMBOL_FUNCTION_CALL[text() = 'R6Class']]")
        if (length(r6_expr)) {
            class_str <- xml_find_first(r6_expr,
                ".//SYMBOL_FUNCTION_CALL[text() = 'R6Class']/following-sibling::expr[1]//STR_CONST[1]")
            class_sym <- xml_find_first(r6_expr,
                ".//LEFT_ASSIGN/preceding-sibling::expr[1]/SYMBOL | .//EQ_ASSIGN/preceding-sibling::expr[1]/SYMBOL")
            class_name_value <- NULL
            if (length(class_str)) {
                class_name_value <- gsub('["\'`]', "", xml_text(class_str))
            } else if (length(class_sym)) {
                class_name_value <- xml_text(class_sym)
            }

            if (!is.null(class_name_value)) {
                range_info <- get_element_range(document, r6_expr)
                if (!is.null(range_info)) {
                    return(list(
                        name = class_name_value,
                        uri = uri,
                        range = range_info,
                        classType = "R6"
                    ))
                }
            }
        }
    }

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
        "//expr[LEFT_ASSIGN or EQ_ASSIGN][
      preceding-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}']
    ][
      following-sibling::expr[1]//SYMBOL_FUNCTION_CALL[
        text() = 'R6Class'
      ]
    ]",
        token_quote = token_quote
    )

    defs <- xml_find_all(scopes, xpath)
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

    # Pattern: R6Class("ClassName", ...) with cursor on string
    xpath <- glue(
        "//SYMBOL_FUNCTION_CALL[text() = 'R6Class']/following-sibling::expr[1]//STR_CONST[contains(text(), {dquote}{token_text}{dquote})]",
        token_text = token_text,
        dquote = '"'
    )
    defs <- xml_find_all(scopes, xpath)
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

    NULL
}

#' Detect S4 class definitions (setClass)
#'
#' Matches patterns like: setClass("ClassName", ...)
#'
#' @noRd
detect_s4class <- function(scopes, token_text, document, uri) {
    # Look for setClass pattern - string containing the class name
    xpath <- glue(
        "//SYMBOL_FUNCTION_CALL[text() = 'setClass']/following-sibling::expr[1]//STR_CONST[contains(text(), {dquote}{token_text}{dquote})]",
        token_text = token_text,
        dquote = '"'
    )

    defs <- xml_find_all(scopes, xpath)
    if (length(defs) > 0) {
        defn <- defs[[1]]
        range_info <- get_element_range(document, defn)
        if (!is.null(range_info)) {
            return(list(
                name = token_text,
                uri = uri,
                range = range_info,
                classType = "S4"
            ))
        }
    }

    NULL
}

#' Detect RefClass definitions (setRefClass)
#'
#' Matches patterns like: setRefClass("ClassName", ...)
#'
#' @noRd
detect_refclass <- function(scopes, token_text, document, uri) {
    # Look for setRefClass pattern - string containing the class name
    xpath <- glue(
        "//SYMBOL_FUNCTION_CALL[text() = 'setRefClass']/following-sibling::expr[1]//STR_CONST[contains(text(), {dquote}{token_text}{dquote})]",
        token_text = token_text,
        dquote = '"'
    )

    defs <- xml_find_all(scopes, xpath)
    if (length(defs) > 0) {
        defn <- defs[[1]]
        range_info <- get_element_range(document, defn)
        if (!is.null(range_info)) {
            return(list(
                name = token_text,
                uri = uri,
                range = range_info,
                classType = "RefClass"
            ))
        }
    }

    NULL
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
    parts <- strsplit(token_text, "\\.")[[1]]
    if (length(parts) >= 2) {
        class_name <- parts[length(parts)]

        xpath <- glue(
            "//expr[LEFT_ASSIGN or EQ_ASSIGN][
        preceding-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}']]",
            token_quote = xml_single_quote(token_text)
        )

        defs <- xml_find_all(scopes, xpath)
        if (length(defs) > 0) {
            defn <- defs[[1]]
            range_info <- get_element_range(document, defn)
            if (!is.null(range_info)) {
                return(list(
                    name = class_name,
                    uri = uri,
                    range = range_info,
                    classType = "S3"
                ))
            }
        }
    }

    # Pattern: setMethod("generic", "ClassName", function(...))
    xpath <- glue(
        "//SYMBOL_FUNCTION_CALL[text() = 'setMethod']/following-sibling::expr[STR_CONST[contains(text(), {dquote}{token_text}{dquote})]]",
        token_text = token_text,
        dquote = '"'
    )

    defs <- xml_find_all(scopes, xpath)
    if (length(defs) > 0) {
        defn <- defs[[1]]
        range_info <- get_element_range(document, defn)
        if (!is.null(range_info)) {
            return(list(
                name = token_text,
                uri = uri,
                range = range_info,
                classType = "S4"
            ))
        }
    }

    NULL
}

#' Find supertypes (parent types) of a given type
#'
#' @noRd
find_type_supertypes <- function(workspace, type_def) {
    supertypes <- list()

    # Get the document where the type is defined
    doc <- workspace$documents$get(type_def$uri)
    if (is.null(doc)) {
        return(supertypes)
    }

    xdoc <- workspace$get_parse_data(type_def$uri)$xml_doc
    if (is.null(xdoc)) {
        return(supertypes)
    }

    class_type <- type_def$classType

    if (class_type == "R6") {
        supertypes <- find_r6_supertypes(doc, xdoc, type_def$name, type_def$uri)
    } else if (class_type == "S4") {
        supertypes <- find_s4_supertypes(doc, xdoc, type_def$name, type_def$uri)
    } else if (class_type == "RefClass") {
        supertypes <- find_refclass_supertypes(doc, xdoc, type_def$name, type_def$uri)
    } else if (class_type == "S3") {
        supertypes <- find_s3_supertypes(doc, xdoc, type_def$name, type_def$uri)
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

    supertypes
}

#' Find R6 supertypes (inherit parameter)
#'
#' @noRd
find_r6_supertypes <- function(doc, xdoc, class_name, uri) {
    supertypes <- list()

    # Find full R6Class call expressions (handle namespaced calls like R6::R6Class)
    all_class_defs <- xml_find_all(
        xdoc,
        "//SYMBOL_FUNCTION_CALL[text() = 'R6Class']/ancestor::expr[.//OP-LEFT-PAREN][1]"
    )

    for (class_def in all_class_defs) {
        class_str <- xml_find_first(class_def, ".//STR_CONST[1]")
        class_symbol <- xml_find_first(class_def,
            "preceding-sibling::expr[1][LEFT_ASSIGN or EQ_ASSIGN]/preceding-sibling::expr[1]/SYMBOL")
        class_name_value <- NULL
        if (length(class_str)) {
            class_name_value <- gsub('["\'`]', "", xml_text(class_str))
        } else if (length(class_symbol)) {
            class_name_value <- xml_text(class_symbol)
        }
        if (is.null(class_name_value) || class_name_value != class_name) next

        inherit_node <- xml_find_first(
            class_def,
            ".//SYMBOL_SUB[text() = 'inherit']"
        )
        if (!length(inherit_node)) next

        inherit_param <- xml_find_first(
            inherit_node,
            "following-sibling::expr[1] | following-sibling::*[1][self::EQ_ASSIGN]/following-sibling::expr[1]"
        )

        if (length(inherit_param) > 0) {
            # Extract class name from SYMBOL or STR_CONST within the expr
            inherit_symbol <- xml_find_first(inherit_param, "./SYMBOL | ./expr//SYMBOL")
            if (length(inherit_symbol)) {
                inherit_name <- xml_text(inherit_symbol)
            } else {
                inherit_str <- xml_find_first(inherit_param, "./STR_CONST | ./expr//STR_CONST")
                if (length(inherit_str)) {
                    inherit_name <- gsub('["\'`]', "", xml_text(inherit_str))
                } else {
                    inherit_name <- gsub('["\'`]', "", xml_text(inherit_param))
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
    all_setclass_calls <- xml_find_all(xdoc,
        "//SYMBOL_FUNCTION_CALL[text() = 'setClass']/ancestor::expr[1]")

    for (setclass_call in all_setclass_calls) {
        # Get the first string constant (the class name)
        first_str <- xml_find_first(setclass_call,
            ".//SYMBOL_FUNCTION_CALL[text() = 'setClass']/following-sibling::expr[1]//STR_CONST[1]")

        if (!length(first_str)) next
        call_class_name <- gsub('["\'`]', "", xml_text(first_str))

        if (call_class_name != class_name) next

        # Now find the contains parameter
        contains_param <- xml_find_first(setclass_call,
            ".//SYMBOL[text() = 'contains']/following-sibling::*[1][self::EQ_ASSIGN]/following-sibling::expr[1]")

        if (length(contains_param) > 0) {
            # Could contain one or more class names as strings
            parent_strs <- xml_find_all(contains_param, ".//STR_CONST")
            for (parent_str in parent_strs) {
                parent_name <- gsub('["\'`]', "", xml_text(parent_str))
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
    all_setrefclass_calls <- xml_find_all(xdoc,
        "//SYMBOL_FUNCTION_CALL[text() = 'setRefClass']/ancestor::expr[1]")

    for (setrefclass_call in all_setrefclass_calls) {
        # Get the first string constant (the class name)
        first_str <- xml_find_first(setrefclass_call,
            ".//SYMBOL_FUNCTION_CALL[text() = 'setRefClass']/following-sibling::expr[1]//STR_CONST[1]")

        if (!length(first_str)) next
        call_class_name <- gsub('["\'`]', "", xml_text(first_str))

        if (call_class_name != class_name) next

        # Now find the contains parameter
        contains_param <- xml_find_first(setrefclass_call,
            ".//SYMBOL[text() = 'contains']/following-sibling::*[1][self::EQ_ASSIGN]/following-sibling::expr[1]")

        if (length(contains_param) > 0) {
            parent_strs <- xml_find_all(contains_param, ".//STR_CONST")
            for (parent_str in parent_strs) {
                parent_name <- gsub('["\'`]', "", xml_text(parent_str))
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
find_type_subtypes <- function(workspace, type_def) {
    subtypes <- list()

    class_type <- type_def$classType
    parent_name <- type_def$name

    # Search through all documents for classes that inherit from this one
    for (doc_uri in workspace$documents$keys()) {
        doc <- workspace$documents$get(doc_uri)
        xdoc <- workspace$get_parse_data(doc_uri)$xml_doc

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

    subtypes
}

#' Find R6 subtypes
#'
#' @noRd
find_r6_subtypes <- function(doc, xdoc, parent_name, uri) {
    subtypes <- list()

    # Find full R6Class call expressions (handle namespaced calls like R6::R6Class)
    all_class_defs <- xml_find_all(
        xdoc,
        "//SYMBOL_FUNCTION_CALL[text() = 'R6Class']/ancestor::expr[.//OP-LEFT-PAREN][1]"
    )

    for (class_def in all_class_defs) {
        inherit_node <- xml_find_first(
            class_def,
            ".//SYMBOL_SUB[text() = 'inherit']"
        )
        if (!length(inherit_node)) next

        inherit_param <- xml_find_first(
            inherit_node,
            "following-sibling::expr[1] | following-sibling::*[1][self::EQ_ASSIGN]/following-sibling::expr[1]"
        )
        if (!length(inherit_param)) next

        # Extract class name from SYMBOL or STR_CONST within the expr
        inherit_symbol_name <- xml_find_first(inherit_param, "./SYMBOL | ./expr//SYMBOL")
        if (length(inherit_symbol_name)) {
            inherit_name <- xml_text(inherit_symbol_name)
        } else {
            inherit_str <- xml_find_first(inherit_param, "./STR_CONST | ./expr//STR_CONST")
            if (length(inherit_str)) {
                inherit_name <- gsub('["\'`]', "", xml_text(inherit_str))
            } else {
                inherit_name <- gsub('["\'`]', "", xml_text(inherit_param))
            }
        }

        if (inherit_name != parent_name) next

        # Extract the actual class name
        class_str <- xml_find_first(class_def, ".//STR_CONST[1]")
        if (length(class_str)) {
            class_name <- gsub('["\'`]', "", xml_text(class_str))
            range_info <- get_element_range(doc, class_str)
        } else {
            # Try to find from LHS of assignment
            class_sym <- xml_find_first(class_def,
                "ancestor::expr/expr[1]/SYMBOL")
            if (length(class_sym)) {
                class_name <- xml_text(class_sym)
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
    all_setclass_calls <- xml_find_all(xdoc,
        "//SYMBOL_FUNCTION_CALL[text() = 'setClass']/ancestor::expr[1]")

    for (setclass_call in all_setclass_calls) {
        # Check if this class contains parent_name
        contains_param <- xml_find_first(setclass_call,
            ".//SYMBOL[text() = 'contains']/following-sibling::*[1][self::EQ_ASSIGN]/following-sibling::expr[1]")

        if (!length(contains_param)) next

        # Check if parent_name is in the contains parameter
        parent_strs <- xml_find_all(contains_param, ".//STR_CONST")
        found_parent <- FALSE
        for (parent_str in parent_strs) {
            parent_text <- gsub('["\'`]', "", xml_text(parent_str))
            if (parent_text == parent_name) {
                found_parent <- TRUE
                break
            }
        }
        if (!found_parent) next

        # Get the class name from the first string constant in the setClass call
        class_str <- xml_find_first(setclass_call,
            ".//SYMBOL_FUNCTION_CALL[text() = 'setClass']/following-sibling::expr[1]//STR_CONST")

        if (length(class_str)) {
            class_name <- gsub('["\'`]', "", xml_text(class_str))
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
    all_setrefclass_calls <- xml_find_all(xdoc,
        "//SYMBOL_FUNCTION_CALL[text() = 'setRefClass']/ancestor::expr[1]")

    for (setrefclass_call in all_setrefclass_calls) {
        # Check if this class contains parent_name
        contains_param <- xml_find_first(setrefclass_call,
            ".//SYMBOL[text() = 'contains']/following-sibling::*[1][self::EQ_ASSIGN]/following-sibling::expr[1]")

        if (!length(contains_param)) next

        # Check if parent_name is in the contains parameter
        parent_strs <- xml_find_all(contains_param, ".//STR_CONST")
        found_parent <- FALSE
        for (parent_str in parent_strs) {
            parent_text <- gsub('["\'`]', "", xml_text(parent_str))
            if (parent_text == parent_name) {
                found_parent <- TRUE
                break
            }
        }
        if (!found_parent) next

        # Get the class name from the first string constant in the setRefClass call
        class_str <- xml_find_first(setrefclass_call,
            ".//SYMBOL_FUNCTION_CALL[text() = 'setRefClass']/following-sibling::expr[1]//STR_CONST")

        if (length(class_str)) {
            class_name <- gsub('["\'`]', "", xml_text(class_str))
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

        range(
            start = document$to_lsp_position(row = line1 - 1, col = col1 - 1),
            end = document$to_lsp_position(row = line2 - 1, col = col2)
        )
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
    all_class_defs <- xml_find_all(
        xdoc,
        "//SYMBOL_FUNCTION_CALL[text() = 'R6Class']/ancestor::expr[.//OP-LEFT-PAREN][1]"
    )

    for (class_def in all_class_defs) {
        # Verify this is the right class
        # First try to get class name from the string argument
        class_str <- xml_find_first(class_def, ".//STR_CONST[1]")
        class_name_value <- NULL
        if (length(class_str)) {
            class_name_value <- gsub('["\047`]', "", xml_text(class_str))
        }

        # If not found or doesn't match, try to get from left side of assignment
        if (is.null(class_name_value) || class_name_value != class_name) {
            # Navigate up to find the assignment expression
            assign_expr <- xml_find_first(class_def,
                "ancestor::expr[LEFT_ASSIGN or EQ_ASSIGN][1]")
            if (length(assign_expr)) {
                # Get the symbol on the left side of the assignment
                class_symbol <- xml_find_first(assign_expr,
                    "./expr[1]/SYMBOL[1]")
                if (length(class_symbol)) {
                    class_name_value <- xml_text(class_symbol)
                }
            }
        }

        if (is.null(class_name_value) || class_name_value != class_name) {
            next
        }

        # Extract public members
        public_node <- xml_find_first(class_def, ".//SYMBOL_SUB[text() = 'public']")
        if (length(public_node)) {
            public_list <- xml_find_first(
                public_node,
                "following-sibling::expr[1] | following-sibling::*[1][self::EQ_ASSIGN]/following-sibling::expr[1]"
            )
            if (length(public_list)) {
                public_members <- extract_r6_list_members(document, public_list, "public")
                members <- c(members, public_members)
            }
        }

        # Extract private members
        private_node <- xml_find_first(class_def, ".//SYMBOL_SUB[text() = 'private']")
        if (length(private_node)) {
            private_list <- xml_find_first(
                private_node,
                "following-sibling::expr[1] | following-sibling::*[1][self::EQ_ASSIGN]/following-sibling::expr[1]"
            )
            if (length(private_list)) {
                private_members <- extract_r6_list_members(document, private_list, "private")
                members <- c(members, private_members)
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

    # Find SYMBOL_SUB elements that have EQ_SUB as immediate next sibling
    # This indicates name = value patterns
    all_symbol_subs <- xml_find_all(list_node, ".//SYMBOL_SUB[following-sibling::*[1][self::EQ_SUB]]")

    # Process all SYMBOL_SUB elements and try to find their values
    for (symbol_sub in all_symbol_subs) {
        member_name <- xml_text(symbol_sub)

        # Skip if this is a nested list definition (public/private/active)
        if (member_name %in% c("public", "private", "active", "inherit", "lock_objects",
            "class", "portable", "lock_class", "cloneable", "parent_env")) {
            next
        }

        # The structure at the R6Class call level is:
        # SYMBOL_SUB (member_name), EQ_SUB, expr[value], OP-COMMA, ...
        # All as direct siblings within the list(...) call expr

        # Find the value expr - it's the first expr sibling after this SYMBOL_SUB
        # This skips over the EQ SUB that's between them
        value_expr <- xml_find_first(symbol_sub, "following-sibling::expr[1]")
        if (!length(value_expr)) {
            next
        }

        # Skip if this SYMBOL_SUB is nested inside a function body
        # Function structure in parse tree: expr[FUNCTION, params, body_expr]
        # Body expressions have FUNCTION as a preceding sibling
        # Check if symbol_sub has an ancestor expr that has FUNCTION as preceding sibling
        func_body_ancestor <- xml_find_first(symbol_sub,
            "ancestor::expr[preceding-sibling::FUNCTION]")

        if (length(func_body_ancestor)) {
            # This symbol is inside a function body, not a member definition
            next
        }

        # Also skip if this SYMBOL_SUB is nested inside a list/other structure that is a field value
        # Member-level SYMBOL_SUBs have a grandparent ABOVE list_node (the R6Class call)
        # Nested ones (like result in test2 = list(result = 1)) have list_node as their grandparent
        # Check: symbol_sub -> parent expr -> grandparent expr
        # If grandparent IS list_node, this is nested and should be skipped
        grandparent <- xml_find_first(symbol_sub, "parent::expr/parent::expr")
        if (length(grandparent)) {
            # Check if grandparent is list_node by comparing node identities
            gp_line <- xml_attr(grandparent, "line1")
            gp_col <- xml_attr(grandparent, "col1")
            ln_line <- xml_attr(list_node, "line1")
            ln_col <- xml_attr(list_node, "col1")

            if (identical(gp_line, ln_line) && identical(gp_col, ln_col)) {
                # Grandparent IS list_node, so this is nested
                next
            }
        }

        # Check if this specific value expression contains a FUNCTION keyword
        func_node <- xml_find_first(value_expr, ".//FUNCTION")
        is_function <- length(func_node) > 0

        member_kind <- if (is_function) SymbolKind$Method else SymbolKind$Field
        member_range <- get_element_range(document, symbol_sub)

        if (!is.null(member_range)) {
            members <- c(members, list(document_symbol(
                name = member_name,
                detail = access_modifier,
                kind = member_kind,
                range = member_range,
                selectionRange = member_range
            )))
        }
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
    all_setclass_calls <- xml_find_all(
        xdoc,
        "//SYMBOL_FUNCTION_CALL[text() = 'setClass']/ancestor::expr[1]"
    )

    for (setclass_call in all_setclass_calls) {
        # Get the first string constant (the class name)
        first_str <- xml_find_first(
            setclass_call,
            ".//SYMBOL_FUNCTION_CALL[text() = 'setClass']/following-sibling::expr[1]//STR_CONST[1]"
        )

        if (!length(first_str)) next
        call_class_name <- gsub('["\047`]', "", xml_text(first_str))

        if (call_class_name != class_name) next

        # Extract slots/representation
        slots_node <- xml_find_first(
            setclass_call,
            ".//SYMBOL[text() = 'slots' or text() = 'representation']/following-sibling::*[1][self::EQ_ASSIGN]/following-sibling::expr[1]"
        )

        if (length(slots_node)) {
            # Find all named slots
            slot_names <- xml_find_all(slots_node, ".//SYMBOL_SUB | .//STR_CONST")
            for (slot_name_node in slot_names) {
                slot_name_text <- xml_text(slot_name_node)
                slot_name <- gsub('["\047`]', "", slot_name_text)
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
    all_setmethod_calls <- xml_find_all(
        xdoc,
        "//SYMBOL_FUNCTION_CALL[text() = 'setMethod']/ancestor::expr[1]"
    )

    for (setmethod_call in all_setmethod_calls) {
        # Check if this method is for our class
        class_strs <- xml_find_all(
            setmethod_call,
            ".//SYMBOL_FUNCTION_CALL[text() = 'setMethod']/following-sibling::expr//STR_CONST"
        )

        found_class <- FALSE
        method_name <- NULL
        for (i in seq_along(class_strs)) {
            str_value <- gsub('["\047`]', "", xml_text(class_strs[[i]]))
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
    all_setrefclass_calls <- xml_find_all(
        xdoc,
        "//SYMBOL_FUNCTION_CALL[text() = 'setRefClass']/ancestor::expr[1]"
    )

    for (setrefclass_call in all_setrefclass_calls) {
        # Get the first string constant (the class name)
        first_str <- xml_find_first(
            setrefclass_call,
            ".//SYMBOL_FUNCTION_CALL[text() = 'setRefClass']/following-sibling::expr[1]//STR_CONST[1]"
        )

        if (!length(first_str)) next
        call_class_name <- gsub('["\047`]', "", xml_text(first_str))

        if (call_class_name != class_name) next

        # Extract fields
        fields_node <- xml_find_first(
            setrefclass_call,
            ".//SYMBOL[text() = 'fields']/following-sibling::*[1][self::EQ_ASSIGN]/following-sibling::expr[1]"
        )

        if (length(fields_node)) {
            field_names <- xml_find_all(fields_node, ".//SYMBOL_SUB")
            for (field_name_node in field_names) {
                field_name <- xml_text(field_name_node)
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
        methods_node <- xml_find_first(
            setrefclass_call,
            ".//SYMBOL[text() = 'methods']/following-sibling::*[1][self::EQ_ASSIGN]/following-sibling::expr[1]"
        )

        if (length(methods_node)) {
            method_names <- xml_find_all(methods_node, ".//SYMBOL_SUB")
            for (method_name_node in method_names) {
                method_name <- xml_text(method_name_node)
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
