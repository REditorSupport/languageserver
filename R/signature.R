signature_xpath <- paste(
    "(*|descendant-or-self::exprlist/*)[LEFT_ASSIGN/preceding-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}]]/expr[FUNCTION|OP-LAMBDA]",
    "(*|descendant-or-self::exprlist/*)[EQ_ASSIGN/preceding-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}]]/expr[FUNCTION|OP-LAMBDA]",
    sep = "|")

#' Extract parameter names from a function signature
#'
#' Parses a signature like "foo(x, y = 1, z = 2)" to extract parameter names
#'
#' @param signature character string of the function signature
#' @return character vector of parameter names
#' @noRd
extract_parameter_names <- function(signature) {
    # Extract the part between parentheses
    match <- regexec("\\((.*)\\)", signature)
    if (match[[1]][1] == -1) {
        return(character(0))
    }
    
    params_str <- regmatches(signature, match)[[1]][2]
    if (is.na(params_str) || nchar(trimws(params_str)) == 0) {
        return(character(0))
    }
    
    # Split parameters carefully, respecting nested brackets and quotes
    param_names <- character(0)
    current_param <- ""
    depth <- 0
    in_quote <- FALSE
    quote_char <- ""
    
    chars <- strsplit(params_str, "")[[1]]
    for (i in seq_along(chars)) {
        char <- chars[i]
        
        if (in_quote) {
            current_param <- paste0(current_param, char)
            if (char == quote_char) {
                in_quote <- FALSE
            }
        } else {
            if (char %in% c("'", '"', "`")) {
                in_quote <- TRUE
                quote_char <- char
                current_param <- paste0(current_param, char)
            } else if (char %in% c("(", "[", "{")) {
                depth <- depth + 1
                current_param <- paste0(current_param, char)
            } else if (char %in% c(")", "]", "}")) {
                depth <- depth - 1
                current_param <- paste0(current_param, char)
            } else if (char == "," && depth == 0) {
                # Extract parameter name
                param_trimmed <- trimws(current_param)
                # Get the name part (before = if present)
                param_name <- trimws(sub("\\s*=.*$", "", param_trimmed))
                if (nchar(param_name) > 0) {
                    param_names <- c(param_names, param_name)
                }
                current_param <- ""
            } else {
                current_param <- paste0(current_param, char)
            }
        }
    }
    
    # Don't forget the last parameter
    param_trimmed <- trimws(current_param)
    param_name <- trimws(sub("\\s*=.*$", "", param_trimmed))
    if (nchar(param_name) > 0) {
        param_names <- c(param_names, param_name)
    }
    
    return(param_names)
}

#' Detect the active parameter index, handling both positional and named arguments
#'
#' This function intelligently determines which parameter is active by:
#' 1. Checking if the current argument uses a named parameter (e.g., "z = ")
#' 2. If named, finding that parameter's index in the signature
#' 3. Otherwise, counting commas for positional arguments
#'
#' @param content character vector of document lines
#' @param start_row 0-based row index of the opening bracket
#' @param start_col 0-based column index of the opening bracket
#' @param end_row 0-based row index of the cursor
#' @param end_col 0-based column index of the cursor
#' @param signature character string of the function signature (optional, for named arg detection)
#' @return integer representing the active parameter index (0-based), or NULL if not applicable
#' @noRd
detect_active_parameter <- function(content, start_row, start_col, end_row, end_col, signature = NULL) {
    logger$info("detect_active_parameter: start_row=", start_row, ", start_col=", start_col,
                ", end_row=", end_row, ", end_col=", end_col)
    
    # First, extract the text from opening bracket to cursor
    call_text <- ""
    current_row <- start_row
    
    while (current_row <= end_row) {
        if (current_row >= length(content)) break
        
        line <- content[current_row + 1]  # R is 1-indexed
        if (is.na(line) || is.null(line)) break
        
        start_pos <- if (current_row == start_row) start_col + 2 else 1
        end_pos <- if (current_row == end_row) min(end_col + 1, nchar(line)) else nchar(line)
        
        if (start_pos <= nchar(line)) {
            text_segment <- substr(line, start_pos, end_pos)
            call_text <- paste0(call_text, if (current_row > start_row) "\n" else "", text_segment)
        }
        
        current_row <- current_row + 1
    }
    
    logger$info("detect_active_parameter: call_text='", call_text, "'")
    
    # Parse the call text to find the current argument
    # We need to find what's after the last comma at depth 0, or from the start if no comma
    comma_count <- 0
    bracket_depth <- 0
    last_comma_pos <- 0
    in_single_quote <- FALSE
    in_double_quote <- FALSE
    escaped <- FALSE
    
    chars <- strsplit(call_text, "")[[1]]
    for (i in seq_along(chars)) {
        char <- chars[i]
        
        if (escaped) {
            escaped <- FALSE
            next
        }
        
        if (char == "\\") {
            escaped <- TRUE
            next
        }
        
        if (!in_single_quote && !in_double_quote) {
            if (char == "'") {
                in_single_quote <- TRUE
            } else if (char == '"') {
                in_double_quote <- TRUE
            } else if (char == "#") {
                break
            } else if (char == "(" || char == "[" || char == "{") {
                bracket_depth <- bracket_depth + 1
            } else if (char == ")" || char == "]" || char == "}") {
                bracket_depth <- bracket_depth - 1
            } else if (char == "," && bracket_depth == 0) {
                comma_count <- comma_count + 1
                last_comma_pos <- i
            }
        } else if (in_single_quote && char == "'") {
            in_single_quote <- FALSE
        } else if (in_double_quote && char == '"') {
            in_double_quote <- FALSE
        }
    }
    
    # Extract the current argument text (after last comma or from start)
    current_arg <- if (last_comma_pos > 0) {
        substr(call_text, last_comma_pos + 2, nchar(call_text))
    } else {
        call_text
    }
    current_arg <- trimws(current_arg)
    
    logger$info("detect_active_parameter: current_arg='", current_arg, "', comma_count=", comma_count)
    
    # Check if this is a named argument (pattern: name = ...)
    # Match identifier followed by =, with optional whitespace
    named_match <- regexec("^([a-zA-Z._][a-zA-Z0-9._]*)\\s*=\\s*", current_arg)
    if (!is.null(signature) && named_match[[1]][1] != -1) {
        # Extract the parameter name
        param_name <- regmatches(current_arg, named_match)[[1]][2]
        logger$info("detect_active_parameter: named argument detected: '", param_name, "'")
        
        # Get all parameter names from the signature
        param_names <- extract_parameter_names(signature)
        logger$info("detect_active_parameter: signature param_names=", paste(param_names, collapse=", "))
        
        # Find the index of this parameter
        param_index <- match(param_name, param_names)
        if (!is.na(param_index)) {
            logger$info("detect_active_parameter: returning named parameter index=", param_index - 1)
            return(param_index - 1)  # Convert to 0-based
        } else {
            logger$info("detect_active_parameter: named parameter not found in signature, using comma count")
        }
    }
    
    # Fall back to positional (comma-based) detection
    logger$info("detect_active_parameter: returning positional index=", comma_count)
    return(comma_count)
}

#' Parse parameters from a function signature
#'
#' Extracts parameter information from a signature string like "foo(x, y = 3)"
#' and returns a list of ParameterInformation objects for LSP.
#'
#' @param signature character string of the function signature
#' @return list of ParameterInformation objects
#' @noRd
parse_signature_parameters <- function(signature) {
    logger$info("parse_signature_parameters: signature=", signature)
    
    # Extract the part between parentheses
    match <- regexec("\\((.*)\\)", signature)
    if (match[[1]][1] == -1) {
        logger$info("parse_signature_parameters: no parameters found")
        return(list())
    }
    
    params_str <- regmatches(signature, match)[[1]][2]
    if (is.na(params_str) || nchar(trimws(params_str)) == 0) {
        logger$info("parse_signature_parameters: empty parameter list")
        return(list())
    }
    
    logger$info("parse_signature_parameters: params_str=", params_str)
    
    # Find the opening parenthesis position in the original signature
    paren_pos <- regexpr("\\(", signature)
    base_offset <- paren_pos[1]  # Position of '(' in the signature
    
    # Split parameters carefully, respecting nested brackets and quotes
    params <- list()
    current_param <- ""
    depth <- 0
    in_quote <- FALSE
    quote_char <- ""
    char_pos <- 0
    
    chars <- strsplit(params_str, "")[[1]]
    for (i in seq_along(chars)) {
        char <- chars[i]
        
        if (in_quote) {
            current_param <- paste0(current_param, char)
            if (char == quote_char) {
                in_quote <- FALSE
            }
        } else {
            if (char %in% c("'", '"', "`")) {
                in_quote <- TRUE
                quote_char <- char
                current_param <- paste0(current_param, char)
            } else if (char %in% c("(", "[", "{")) {
                depth <- depth + 1
                current_param <- paste0(current_param, char)
            } else if (char %in% c(")", "]", "}")) {
                depth <- depth - 1
                current_param <- paste0(current_param, char)
            } else if (char == "," && depth == 0) {
                # Found a parameter separator at the top level
                param_trimmed <- trimws(current_param)
                if (nchar(param_trimmed) > 0) {
                    # Find where the trimmed parameter starts and ends in the original string
                    leading_space <- nchar(current_param) - nchar(sub("^\\\\s+", "", current_param))
                    trailing_space <- nchar(current_param) - nchar(sub("\\\\s+$", "", current_param))
                    
                    # Calculate the label position as [start, end] in the full signature
                    # LSP uses 0-based positions
                    param_start <- base_offset + char_pos + leading_space
                    param_end <- base_offset + char_pos + nchar(current_param) - trailing_space
                    
                    params[[length(params) + 1]] <- list(
                        label = c(param_start, param_end)
                    )
                }
                current_param <- ""
                char_pos <- i  # Next param starts after the comma
            } else {
                current_param <- paste0(current_param, char)
            }
        }
    }
    
    # Don't forget the last parameter
    param_trimmed <- trimws(current_param)
    if (nchar(param_trimmed) > 0) {
        leading_space <- nchar(current_param) - nchar(sub("^\\\\s+", "", current_param))
        trailing_space <- nchar(current_param) - nchar(sub("\\\\s+$", "", current_param))
        
        param_start <- base_offset + char_pos + leading_space
        param_end <- base_offset + nchar(params_str) - trailing_space
        
        params[[length(params) + 1]] <- list(
            label = c(param_start, param_end)
        )
    }
    
    logger$info("parse_signature_parameters: found ", length(params), " parameters")
    return(params)
}

#' the response to a textDocument/signatureHelp Request
#'
#' If the symbol at the current position is a function, return its arguments
#' (as with [base::args()]).
#'
#' @noRd
signature_reply <- function(id, uri, workspace, document, point) {

    if (!check_scope(uri, document, point)) {
        return(Response$new(id, list(signatures = NULL)))
    }

    result <- document$detect_call(point)

    SignatureInformation <- list()
    activeSignature <- NULL
    activeParameter <- NULL
    sig <- NULL

    if (nzchar(result$token)) {
        xdoc <- workspace$get_parse_data(uri)$xml_doc
        if (result$accessor == "" && !is.null(xdoc)) {
            row <- point$row + 1
            col <- point$col + 1
            enclosing_scopes <- xdoc_find_enclosing_scopes(xdoc,
                row, col, top = TRUE)
            xpath <- glue(signature_xpath, row = row,
                token_quote = xml_single_quote(result$token))
            all_defs <- xml_find_all(enclosing_scopes, xpath)
            if (length(all_defs)) {
                last_def <- all_defs[[length(all_defs)]]
                func_line1 <- as.integer(xml_attr(last_def, "line1"))
                func_col1 <- as.integer(xml_attr(last_def, "col1"))
                func_line2 <- as.integer(xml_attr(last_def, "line2"))
                func_col2 <- as.integer(xml_attr(last_def, "col2"))
                func_text <- get_range_text(document$content,
                    line1 = func_line1,
                    col1 = func_col1,
                    line2 = func_line2,
                    col2 = func_col2
                )
                func_expr <- parse(text = func_text, keep.source = FALSE)
                sig <- get_signature(result$token, func_expr[[1]])
                documentation <- ""

                doc_line1 <- detect_comments(document$content, func_line1 - 1) + 1
                if (doc_line1 < func_line1) {
                    comment <- document$content[doc_line1:(func_line1 - 1)]
                    doc <- convert_comment_to_documentation(comment)
                    doc_string <- NULL

                    if (is.character(doc)) {
                        doc_string <- doc
                    } else if (is.list(doc)) {
                        if (is.null(doc$markdown)) {
                            doc_string <- doc$description
                        } else {
                            doc_string <- doc$markdown
                        }
                    }

                    if (is.null(doc_string)) {
                        doc_string <- ""
                    }

                    documentation <- list(kind = "markdown", value = doc_string)
                }

                parameters <- parse_signature_parameters(sig)
                SignatureInformation <- list(list(
                    label = sig,
                    documentation = documentation,
                    parameters = parameters
                ))
                activeSignature <- 0
            }
        }

        if (is.null(sig)) {
            sig <- workspace$get_signature(result$token, result$package,
                exported_only = result$accessor != ":::")
            logger$info("sig: ", sig)
            if (!is.null(sig)) {
                doc <- workspace$get_documentation(result$token, result$package, isf = TRUE)
                doc_string <- NULL

                if (is.character(doc)) {
                    doc_string <- doc
                } else if (is.list(doc)) {
                    doc_string <- doc$description
                }

                if (is.null(doc_string)) {
                    doc_string <- ""
                }

                documentation <- list(kind = "markdown", value = doc_string)

                parameters <- parse_signature_parameters(sig)
                SignatureInformation <- list(list(
                    label = sig,
                    documentation = documentation,
                    parameters = parameters
                ))
                activeSignature <- 0
            }
        }
    }

    # Calculate activeParameter if we have a valid signature
    if (!is.null(activeSignature) && nzchar(result$token)) {
        logger$info("Calculating activeParameter for token: ", result$token)
        fub_result <- find_unbalanced_bracket(document$content, point$row, point$col - 1)
        loc <- fub_result[[1]]
        bracket <- fub_result[[2]]
        logger$info("Bracket location: row=", loc[1], ", col=", loc[2], ", bracket='", bracket, "'")
        
        if (loc[1] >= 0 && loc[2] >= 0 && bracket == "(") {
            activeParameter <- detect_active_parameter(
                document$content,
                loc[1],  # start_row (0-based)
                loc[2],  # start_col (0-based)
                point$row,  # end_row (0-based)
                point$col,   # end_col (0-based)
                sig          # signature for named argument detection
            )
            logger$info("activeParameter set to: ", activeParameter)
        } else {
            logger$info("Invalid bracket location or not a parenthesis")
        }
    }

    response_result <- list(signatures = SignatureInformation)
    response_result$activeSignature <- activeSignature
    response_result$activeParameter <- activeParameter
    
    logger$info("signature_reply result: activeSignature=", activeSignature,
                ", activeParameter=", activeParameter)

    Response$new(id, result = response_result)
}
