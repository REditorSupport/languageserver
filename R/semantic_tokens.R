# Token type indices based on SemanticTokensLegend in capabilities.R
TokenTypes <- list(
    namespace = 0,
    type = 1,
    class = 2,
    enum = 3,
    interface = 4,
    struct = 5,
    typeParameter = 6,
    parameter = 7,
    variable = 8,
    property = 9,
    enumMember = 10,
    event = 11,
    `function` = 12,
    method = 13,
    macro = 14,
    keyword = 15,
    modifier = 16,
    comment = 17,
    string = 18,
    number = 19,
    regexp = 20,
    operator = 21,
    decorator = 22
)

# Token modifier flags based on SemanticTokensLegend
TokenModifiers <- list(
    declaration = 1,     # 2^0
    definition = 2,      # 2^1
    readonly = 4,        # 2^2
    static = 8,          # 2^3
    deprecated = 16,     # 2^4
    abstract = 32,       # 2^5
    async = 64,          # 2^6
    modification = 128,  # 2^7
    documentation = 256, # 2^8
    defaultLibrary = 512 # 2^9
)

#' Encode semantic tokens to LSP format
#'
#' The LSP format uses delta encoding with 5 integers per token:
#' [deltaLine, deltaStartChar, length, tokenType, tokenModifiers]
#'
#' @param tokens A list of token objects with line, col, length, type, modifiers
#' @return An integer vector with encoded tokens
#' @noRd
encode_semantic_tokens <- function(tokens) {
    if (length(tokens) == 0) {
        return(integer(0))
    }
    
    # Sort tokens by position
    tokens <- tokens[order(
        vapply(tokens, function(x) x$line, numeric(1)),
        vapply(tokens, function(x) x$col, numeric(1))
    )]
    
    result <- integer(0)
    prev_line <- 0
    prev_col <- 0
    
    for (token in tokens) {
        delta_line <- token$line - prev_line
        delta_col <- if (delta_line == 0) token$col - prev_col else token$col
        
        result <- c(result, 
            delta_line,
            delta_col,
            token$length,
            token$type,
            token$modifiers
        )
        
        prev_line <- token$line
        prev_col <- token$col
    }
    
    result
}

#' Map R token types to semantic token types
#'
#' @param token_name The R token name from xmlparsedata
#' @param token_text The text content of the token
#' @param workspace The workspace object for namespace resolution
#' @return The semantic token type index
#' @noRd
get_token_type <- function(token_name, token_text, workspace = NULL) {
    switch(token_name,
        COMMENT = TokenTypes$comment,
        STR_CONST = TokenTypes$string,
        NUM_CONST = TokenTypes$number,
        NULL_CONST = TokenTypes$keyword,
        SYMBOL_PACKAGE = TokenTypes$namespace,
        SYMBOL_FUNCTION_CALL = TokenTypes$`function`,
        SYMBOL_FORMALS = TokenTypes$parameter,
        SYMBOL = TokenTypes$variable,
        SYMBOL_SUB = TokenTypes$variable,
        `FUNCTION` = TokenTypes$keyword,
        `IF` = TokenTypes$keyword,
        `ELSE` = TokenTypes$keyword,
        `REPEAT` = TokenTypes$keyword,
        `WHILE` = TokenTypes$keyword,
        `FOR` = TokenTypes$keyword,
        `IN` = TokenTypes$keyword,
        `BREAK` = TokenTypes$keyword,
        `NEXT` = TokenTypes$keyword,
        `RETURN` = TokenTypes$keyword,
        `TRUE` = TokenTypes$keyword,
        `FALSE` = TokenTypes$keyword,
        AND = TokenTypes$operator,
        AND2 = TokenTypes$operator,
        OR = TokenTypes$operator,
        OR2 = TokenTypes$operator,
        NS_GET = TokenTypes$operator,
        NS_GET_INT = TokenTypes$operator,
        EQ_ASSIGN = TokenTypes$operator,
        LEFT_ASSIGN = TokenTypes$operator,
        RIGHT_ASSIGN = TokenTypes$operator,
        EQ = TokenTypes$operator,
        NE = TokenTypes$operator,
        LT = TokenTypes$operator,
        LE = TokenTypes$operator,
        GT = TokenTypes$operator,
        GE = TokenTypes$operator,
        SPECIAL = TokenTypes$operator,
        TokenTypes$variable  # Default for unknown tokens
    )
}

#' Get token modifiers based on context
#'
#' @param token XML node for the token
#' @param token_name The R token name
#' @param workspace The workspace object
#' @return Integer representing combined modifier flags
#' @noRd
get_token_modifiers <- function(token, token_name, workspace = NULL) {
    modifiers <- 0
    
    # Check if it's from a default library (base R packages)
    if (token_name == "SYMBOL_FUNCTION_CALL" && !is.null(workspace)) {
        token_text <- xml2::xml_text(token)
        pkg <- workspace$guess_namespace(token_text, isf = TRUE)
        if (!is.null(pkg)) {
            if (pkg %in% c("base", "stats", "graphics", "grDevices", 
                          "utils", "datasets", "methods")) {
                modifiers <- bitwOr(modifiers, TokenModifiers$defaultLibrary)
            }
        } else {
            # If we can't find the function in any package namespace,
            # it might be a user-defined function, so don't add any special modifiers
        }
    }
    
    # Check if it's a definition (e.g., in function formals or left side of assignment)
    parent <- xml2::xml_parent(token)
    if (length(parent) > 0) {
        parent_name <- xml2::xml_name(parent)
        if (parent_name == "SYMBOL_FORMALS") {
            modifiers <- bitwOr(modifiers, TokenModifiers$declaration)
        } else if (token_name == "SYMBOL" || token_name == "SYMBOL_FUNCTION_CALL") {
            # Check if it's on the left side of an assignment
            following <- xml2::xml_find_first(token, "following-sibling::*[1]")
            if (length(following) > 0) {
                following_name <- xml2::xml_name(following)
                if (following_name %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")) {
                    modifiers <- bitwOr(modifiers, TokenModifiers$definition)
                }
            }
        }
    }
    
    modifiers
}

#' Extract semantic tokens from parsed document
#'
#' @param xdoc The XML document from xmlparsedata
#' @param document The document object
#' @param workspace The workspace object
#' @param range Optional range to limit token extraction
#' @return A list of token objects
#' @noRd
extract_semantic_tokens <- function(xdoc, document, workspace, range = NULL) {
    tokens <- list()
    
    # Select all tokens that should be highlighted
    xpath <- "//*[@line1 and @col1 and @line2 and @col2 and (
        self::COMMENT or
        self::STR_CONST or
        self::NUM_CONST or
        self::NULL_CONST or
        self::SYMBOL_PACKAGE or
        self::SYMBOL_FUNCTION_CALL or
        self::SYMBOL_FORMALS or
        self::SYMBOL or
        self::SYMBOL_SUB or
        self::FUNCTION or
        self::IF or self::ELSE or
        self::REPEAT or self::WHILE or self::FOR or self::IN or
        self::BREAK or self::NEXT or self::RETURN or
        self::TRUE or self::FALSE or
        self::AND or self::AND2 or self::OR or self::OR2 or
        self::NS_GET or self::NS_GET_INT or
        self::EQ_ASSIGN or self::LEFT_ASSIGN or self::RIGHT_ASSIGN or
        self::EQ or self::NE or self::LT or self::LE or self::GT or self::GE or
        self::SPECIAL
    )]"
    
    nodes <- xml2::xml_find_all(xdoc, xpath)
    
    for (node in nodes) {
        line1 <- as.integer(xml2::xml_attr(node, "line1")) - 1
        col1 <- as.integer(xml2::xml_attr(node, "col1")) - 1
        line2 <- as.integer(xml2::xml_attr(node, "line2")) - 1
        col2 <- as.integer(xml2::xml_attr(node, "col2"))
        
        # Skip if outside range
        if (!is.null(range)) {
            if (line1 < range$start$line || line2 > range$end$line) {
                next
            }
            if (line1 == range$start$line && col1 < range$start$character) {
                next
            }
            if (line2 == range$end$line && col2 > range$end$character) {
                next
            }
        }
        
        # Convert positions to LSP format
        lsp_start <- document$to_lsp_position(line1, col1)
        lsp_end <- document$to_lsp_position(line2, col2)
        
        token_name <- xml2::xml_name(node)
        token_text <- xml2::xml_text(node)
        
        # Handle multi-line tokens
        if (lsp_start$line == lsp_end$line) {
            tokens <- append(tokens, list(list(
                line = lsp_start$line,
                col = lsp_start$character,
                length = lsp_end$character - lsp_start$character,
                type = get_token_type(token_name, token_text, workspace),
                modifiers = get_token_modifiers(node, token_name, workspace)
            )))
        } else {
            # For multi-line tokens, only highlight the first line
            # (LSP semantic tokens don't support multi-line tokens well)
            line_text <- document$line(line1 + 1)
            length <- nchar(line_text) - lsp_start$character
            tokens <- append(tokens, list(list(
                line = lsp_start$line,
                col = lsp_start$character,
                length = length,
                type = get_token_type(token_name, token_text, workspace),
                modifiers = get_token_modifiers(node, token_name, workspace)
            )))
        }
    }
    
    tokens
}

#' Generate semantic tokens for full document
#'
#' @param id The request id
#' @param uri The document URI
#' @param workspace The workspace object
#' @param document The document object
#' @return Response object with semantic tokens
#' @noRd
document_semantic_tokens_full_reply <- function(id, uri, workspace, document) {
    parse_data <- workspace$get_parse_data(uri)
    
    if (is.null(parse_data) || is.null(parse_data$xml_doc)) {
        return(Response$new(id, result = list(data = integer(0))))
    }
    
    xdoc <- parse_data$xml_doc
    tokens <- extract_semantic_tokens(xdoc, document, workspace)
    encoded <- encode_semantic_tokens(tokens)
    
    Response$new(
        id,
        result = list(
            data = as.integer(encoded)
        )
    )
}

#' Generate semantic tokens for document range
#'
#' @param id The request id
#' @param uri The document URI
#' @param workspace The workspace object
#' @param document The document object
#' @param range The range to generate tokens for
#' @return Response object with semantic tokens
#' @noRd
document_semantic_tokens_range_reply <- function(id, uri, workspace, document, range) {
    parse_data <- workspace$get_parse_data(uri)
    
    if (is.null(parse_data) || is.null(parse_data$xml_doc)) {
        return(Response$new(id, result = list(data = integer(0))))
    }
    
    xdoc <- parse_data$xml_doc
    tokens <- extract_semantic_tokens(xdoc, document, workspace, range)
    encoded <- encode_semantic_tokens(tokens)
    
    Response$new(
        id,
        result = list(
            data = as.integer(encoded)
        )
    )
}