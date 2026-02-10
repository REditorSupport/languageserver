#' Count references to a symbol
#'
#' Counts how many times a symbol is referenced across all documents.
#' References are symbol uses (SYMBOL_FUNCTION_CALL) or variables (SYMBOL) that are not the definition.
#' @noRd
count_references <- function(token_name, workspace) {
    count <- 0
    
    # First, find where the definition is
    def_uri <- NULL
    def_line_start <- NULL
    def_line_end <- NULL
    
    for (doc_uri in workspace$documents$keys()) {
        defns <- workspace$get_definitions_for_uri(doc_uri)
        if (!is.null(defns[[token_name]])) {
            def_uri <- doc_uri
            def_line_start <- defns[[token_name]]$range$start$line
            def_line_end <- defns[[token_name]]$range$end$line
            break
        }
    }
    
    if (is.null(def_uri)) {
        return(0)
    }
    
    # Count SYMBOL_FUNCTION_CALL occurrences (these are definitely references)
    for (doc_uri in workspace$documents$keys()) {
        xdoc <- workspace$get_parse_data(doc_uri)$xml_doc
        if (!is.null(xdoc)) {
            # Find all SYMBOL_FUNCTION_CALL nodes, then filter by name in R
            all_calls <- xml_find_all(xdoc, "//SYMBOL_FUNCTION_CALL")
            matching_calls <- all_calls[xml_text(all_calls) == token_name]
            count <- count + length(matching_calls)
            
            # For the definition document, also count SYMBOL nodes that are not in the definition
            if (doc_uri == def_uri) {
                all_symbols <- xml_find_all(xdoc, "//SYMBOL")
                symbols <- all_symbols[xml_text(all_symbols) == token_name]
                line1 <- as.integer(xml_attr(symbols, "line1"))
                
                for (i in seq_len(length(symbols))) {
                    symbol_line <- line1[[i]] - 1
                    # Skip if it's part of the definition
                    if (symbol_line < def_line_start || symbol_line > def_line_end) {
                        count <- count + 1
                    }
                }
            }
        }
    }
    
    count
}


#' Find test files that reference a symbol
#'
#' Detects which test files contain references to a given symbol
#' @noRd
find_test_coverage <- function(token_name, workspace) {
    token_quote <- xml_single_quote(token_name)
    test_files <- c()
    
    for (doc_uri in workspace$documents$keys()) {
        # Check if this is a test file (loose pattern matching)
        uri_lower <- tolower(doc_uri)
        if (!grepl("test", uri_lower, fixed = TRUE)) {
            next
        }
        
        xdoc <- workspace$get_parse_data(doc_uri)$xml_doc
        
        if (!is.null(xdoc)) {
            # Look for references to the function
            references_xpath <- glue("//*[(self::SYMBOL or self::SYMBOL_FUNCTION_CALL) and text() = '{token_quote}']")
            symbols <- xml_find_all(xdoc, references_xpath)
            
            if (length(symbols) > 0) {
                test_files <- c(test_files, doc_uri)
            }
        }
    }
    
    # Return unique test files
    unique(test_files)
}


#' Find method implementations for a generic function
#'
#' Detects S3 method implementations for a given generic function name
#' @noRd
find_method_implementations <- function(generic_name, workspace) {
    implementations <- 0
    
    # S3 method pattern: generic_name.something
    s3_pattern <- paste0("^", generic_name, "\\.[a-zA-Z0-9._]+$")
    
    for (doc_uri in workspace$documents$keys()) {
        xdoc <- workspace$get_parse_data(doc_uri)$xml_doc
        
        if (!is.null(xdoc)) {
            # Find all function assignments by looking at SYMBOL_FORMALS
            # A SYMBOL_FORMALS that matches the S3 pattern is a method implementation
            all_formals <- xml_find_all(xdoc, "//SYMBOL_FORMALS")
            
            for (formal in all_formals) {
                formal_name <- xml_text(formal)
                # Check for S3 method pattern
                if (grepl(s3_pattern, formal_name)) {
                    implementations <- implementations + 1
                }
            }
        }
    }
    
    implementations
}


#' Generate code lens for document
#'
#' Creates CodeLens objects showing reference count, test coverage, and method implementations
#' Returns NULL if parse_data is not yet available (document still parsing)
#' @noRd
code_lens_reply <- function(id, uri, workspace, document) {
    result <- list()
    
    if (is.null(document)) {
        return(Response$new(id, result = result))
    }
    
    # Check if parse_data is available
    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data)) {
        # Document still parsing, return NULL to queue for retry
        return(NULL)
    }
    
    # Get all definitions for this document using workspace's built-in method
    definitions <- workspace$get_definitions_for_uri(uri)
    
    # Iterate over each definition
    for (symbol_name in names(definitions)) {
        defn <- definitions[[symbol_name]]
        
        if (is.null(defn) || is.null(defn$range)) {
            next
        }
        
        # Reconstruct the range as a plain list to avoid serialization issues
        symbol_range <- defn$range
        lens_range <- list(
            start = list(
                line = as.numeric(symbol_range$start$line),
                character = as.numeric(symbol_range$start$character)
            ),
            end = list(
                line = as.numeric(symbol_range$end$line),
                character = as.numeric(symbol_range$end$character)
            )
        )
        
        # 1. Reference count (always show, even if 0)
        ref_count <- count_references(symbol_name, workspace)
        ref_label <- if (ref_count == 1) "1 reference" else paste0(ref_count, " references")
        result <- c(result, list(list(
            range = lens_range,
            command = list(
                title = ref_label,
                command = ""
            )
        )))
        
        # 2. Test coverage
        test_files <- find_test_coverage(symbol_name, workspace)
        if (length(test_files) > 0) {
            test_label <- if (length(test_files) == 1) "1 test file" else paste0(length(test_files), " test files")
            result <- c(result, list(list(
                range = lens_range,
                command = list(
                    title = test_label,
                    command = ""
                )
            )))
        }
        
        # 3. Method implementations (for S3 generics)
        method_count <- find_method_implementations(symbol_name, workspace)
        if (method_count > 0) {
            method_label <- if (method_count == 1) "1 method" else paste0(method_count, " methods")
            result <- c(result, list(list(
                range = lens_range,
                command = list(
                    title = method_label,
                    command = ""
                )
            )))
        }
    }
    
    Response$new(id, result = result)
}


#' Resolve code lens (called after initial request)
#'
#' Provides additional details about a code lens entry
#' @noRd
code_lens_resolve_reply <- function(id, workspace, code_lens) {
    # The code lens command is already populated in the initial request
    # This handler returns the lens as-is; it could be enhanced with more details
    Response$new(id, result = code_lens)
}

