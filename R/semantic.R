#' Semantic Token Types and Modifiers
#'
#' Define the legend for semantic tokens
#' @noRd

# Token types for R code
SemanticTokenTypes <- list(
    namespace = 0L,
    type = 1L,
    class = 2L,
    enum = 3L,
    interface = 4L,
    struct = 5L,
    typeParameter = 6L,
    parameter = 7L,
    variable = 8L,
    property = 9L,
    enumMember = 10L,
    event = 11L,
    `function` = 12L,
    method = 13L,
    macro = 14L,
    keyword = 15L,
    modifier = 16L,
    comment = 17L,
    string = 18L,
    number = 19L,
    regexp = 20L,
    operator = 21L,
    decorator = 22L
)

# Token modifiers
SemanticTokenModifiers <- list(
    declaration = 0L,
    definition = 1L,
    readonly = 2L,
    static = 3L,
    deprecated = 4L,
    abstract = 5L,
    async = 6L,
    modification = 7L,
    documentation = 8L,
    defaultLibrary = 9L
)

#' Get the semantic tokens legend
#'
#' Returns the legend that defines token types and modifiers
#' @noRd
get_semantic_tokens_legend <- function() {
    list(
        tokenTypes = names(SemanticTokenTypes),
        tokenModifiers = names(SemanticTokenModifiers)
    )
}

#' Get semantic token type for an XML token
#'
#' Maps R parser token names to LSP semantic token types
#' @noRd
get_token_type <- function(token_name) {
    switch(token_name,
        "SYMBOL" = SemanticTokenTypes$variable,
        "SYMBOL_FUNCTION_CALL" = SemanticTokenTypes[["function"]],
        "SYMBOL_FORMALS" = SemanticTokenTypes$parameter,
        "SYMBOL_PACKAGE" = SemanticTokenTypes$namespace,
        "FUNCTION" = SemanticTokenTypes$keyword,
        "KEYWORD" = SemanticTokenTypes$keyword,
        "NUM_CONST" = SemanticTokenTypes$number,
        "INT_CONST" = SemanticTokenTypes$number,
        "FLOAT_CONST" = SemanticTokenTypes$number,
        "STRING" = SemanticTokenTypes$string,
        "STR_CONST" = SemanticTokenTypes$string,
        "COMMENT" = SemanticTokenTypes$comment,
        "LEFT_ASSIGN" = SemanticTokenTypes$operator,
        "RIGHT_ASSIGN" = SemanticTokenTypes$operator,
        "EQ_ASSIGN" = SemanticTokenTypes$operator,
        "OP-DOLLAR" = SemanticTokenTypes$operator,
        "OP-PIPE" = SemanticTokenTypes$operator,
        "OP" = SemanticTokenTypes$operator,
        "OP-LAMBDA" = SemanticTokenTypes$keyword,
        SemanticTokenTypes$variable  # default
    )
}

#' Extract semantic tokens from a document
#'
#' Analyzes the parse tree and extracts all semantic tokens from a document
#' @noRd
extract_semantic_tokens <- function(uri, workspace, document, range = NULL) {
    xdoc <- workspace$get_parse_data(uri)$xml_doc
    if (is.null(xdoc)) {
        return(list())
    }

    # Get all token elements from the parse tree
    token_elements <- xml_find_all(xdoc, "//*[
        self::SYMBOL or
        self::SYMBOL_FUNCTION_CALL or
        self::SYMBOL_FORMALS or
        self::SYMBOL_PACKAGE or
        self::FUNCTION or
        self::KEYWORD or
        self::NUM_CONST or
        self::INT_CONST or
        self::FLOAT_CONST or
        self::STRING or
        self::STR_CONST or
        self::COMMENT or
        self::LEFT_ASSIGN or
        self::RIGHT_ASSIGN or
        self::EQ_ASSIGN or
        self::OP-DOLLAR or
        self::OP-PIPE or
        self::OP or
        self::OP-LAMBDA
    ]")

    if (length(token_elements) == 0) {
        return(list())
    }

    end_pos <- NULL
    if (!is.null(range)) {
        end_pos <- document$from_lsp_position(range$end)
    }

    tokens <- vector("list", length(token_elements))
    idx <- 0L

    # Process each token
    for (token_node in token_elements) {
        token_name <- xml_name(token_node)

        line1 <- as.integer(xml_attr(token_node, "line1"))
        col1 <- as.integer(xml_attr(token_node, "col1"))
        line2 <- as.integer(xml_attr(token_node, "line2"))
        col2 <- as.integer(xml_attr(token_node, "col2"))

        # Skip if outside range (if range was specified)
        if (!is.null(end_pos) && line1 > end_pos$row + 1) {
            next
        }

        token_type <- get_token_type(token_name)
        modifiers <- 0L  # Start with no modifiers

        # Determine modifiers based on context
        if (token_name == "SYMBOL_FUNCTION_CALL") {
            # Function calls might be declared elsewhere
        } else if (token_name == "SYMBOL_FORMALS") {
            # Parameters are declarations
            modifiers <- bitwOr(modifiers, 2^SemanticTokenModifiers$declaration)
        }

        # Convert positions to UTF-16 code units for LSP
        # Parse data uses 1-based code point positions, LSP uses 0-based UTF-16 units
        line_text <- if (line1 <= length(document$content)) document$content[line1] else ""
        utf16_cols <- code_point_to_unit(line_text, c(col1 - 1, col2))
        token_col <- utf16_cols[1]
        token_length <- utf16_cols[2] - utf16_cols[1]

        idx <- idx + 1L
        tokens[[idx]] <- list(
            line = as.integer(line1 - 1),  # Convert to 0-based, ensure integer
            col = as.integer(token_col),   # UTF-16 code units, ensure integer
            length = as.integer(token_length),  # UTF-16 code units, ensure integer
            tokenType = as.integer(token_type),      # Ensure integer
            tokenModifiers = as.integer(modifiers)   # Ensure integer
        )
    }

    if (idx == 0L) {
        return(list())
    }

    if (idx < length(tokens)) {
        tokens <- tokens[seq_len(idx)]
    }

    tokens
}

#' Encode semantic tokens in LSP format
#'
#' Converts token list to LSP semantic tokens data array format
#' Uses relative position encoding for efficiency.
#' Performance: Implemented in C for large documents
#' @noRd
encode_semantic_tokens <- function(tokens) {
    if (length(tokens) == 0) {
        return(list(data = integer(0)))
    }

    # Convert tokens list to vectors for efficient processing
    # Defensive: coerce all to integer in case of mixed types
    lines <- as.integer(vapply(tokens, function(t) t$line, 0.0))
    cols <- as.integer(vapply(tokens, function(t) t$col, 0.0))
    lengths <- as.integer(vapply(tokens, function(t) t$length, 0.0))
    types <- as.integer(vapply(tokens, function(t) t$tokenType, 0.0))
    mods <- as.integer(vapply(tokens, function(t) t$tokenModifiers, 0.0))

    # Sort by position (stable sort by line, then col)
    order_idx <- order(lines, cols)
    lines <- lines[order_idx]
    cols <- cols[order_idx]
    lengths <- lengths[order_idx]
    types <- types[order_idx]
    mods <- mods[order_idx]

    # Performance: Use C implementation for encoding
    data <- .Call("encode_semantic_tokens_c",
        lines, cols, lengths, types, mods,
        PACKAGE = "languageserver")

    list(data = data)
}

#' The response to a textDocument/semanticTokens/full Request
#'
#' Returns semantic tokens for the entire document
#' @noRd
semantic_tokens_full_reply <- function(id, uri, workspace, document) {
    logger$info("semantic_tokens_full: ", uri)

    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
        (!is.null(parse_data$version) && parse_data$version != document$version)) {
        return(NULL)
    }

    tokens <- extract_semantic_tokens(uri, workspace, document)
    result <- encode_semantic_tokens(tokens)

    Response$new(
        id,
        result = result
    )
}

#' The response to a textDocument/semanticTokens/range Request
#'
#' Returns semantic tokens for a specific range in the document
#' @noRd
semantic_tokens_range_reply <- function(id, uri, workspace, document, range) {
    logger$info("semantic_tokens_range: ", uri)

    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
        (!is.null(parse_data$version) && parse_data$version != document$version)) {
        return(NULL)
    }

    tokens <- extract_semantic_tokens(uri, workspace, document, range = range)
    result <- encode_semantic_tokens(tokens)

    Response$new(
        id,
        result = result
    )
}
