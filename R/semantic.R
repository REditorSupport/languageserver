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
    decorator = 22L,
    label = 23L
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

semantic_declaration_modifier <- bitwShiftL(1L, SemanticTokenModifiers$declaration)

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

#' Create an empty semantic token index
#' @noRd
empty_semantic_data <- function() {
    list(
        lines = integer(),
        cols = integer(),
        lengths = integer(),
        types = integer(),
        modifiers = integer(),
        encoded = integer()
    )
}

#' Parse-data ids of symbols assigned `function()` or `\()`
#'
#' Parentheses around function expressions are ignored.
#' @noRd
function_assignment_symbol_ids <- function(data, syntax = NULL) {
    if (is.null(data) || !nrow(data)) return(integer())
    .Call("function_assignment_ids_c", data, syntax, PACKAGE = "languageserver")
}

# Exclude member accesses (x$return, x@slot) and namespace-qualified symbols (pkg::return, pkg:::x)
xml_not_qualified_symbol_xpath <- "
    not(
        preceding-sibling::OP-DOLLAR
        or preceding-sibling::OP-AT
        or preceding-sibling::NS_GET
        or preceding-sibling::NS_GET_INT
    )
"

xml_function_assignment_rhs_xpath <- "
    (
        parent::expr[count(*) = 1]/following-sibling::LEFT_ASSIGN/following-sibling::expr
        | parent::expr[count(*) = 1]/following-sibling::EQ_ASSIGN/following-sibling::expr
        | parent::expr[count(*) = 1]/preceding-sibling::RIGHT_ASSIGN/preceding-sibling::expr
    )[FUNCTION or OP-LAMBDA or (OP-LEFT-PAREN and not(OP-LEFT-PAREN/preceding-sibling::expr))]
"

#' Whether an XML SYMBOL is assigned `function()` or `\()`
#' @noRd
xml_symbol_is_function_assignment <- function(node) {
    if (!xml_find_lgl(node, paste0("boolean(", xml_function_assignment_rhs_xpath, ")"))) {
        return(FALSE)
    }
    rhs <- xml_find_first(node, xml_function_assignment_rhs_xpath)
    repeat {
        if (xml_find_lgl(rhs, "boolean(FUNCTION or OP-LAMBDA)")) {
            return(TRUE)
        }
        children <- xml_find_all(rhs, "*[not(self::COMMENT)]")
        if (length(children) != 3L ||
                !identical(xml_name(children[c(1L, 3L)]), c("OP-LEFT-PAREN", "OP-RIGHT-PAREN"))) {
            return(FALSE)
        }
        rhs <- children[[2L]]
    }
}

#' Build a compact semantic token index from R parse data
#'
#' This runs in the parse worker. Keeping ordinary integer vectors here avoids
#' repeatedly walking the XML document on the main language-server thread.
#' @noRd
semantic_parse_data <- function(data, content, syntax = NULL) {
    if (is.null(data) || !nrow(data)) {
        return(empty_semantic_data())
    }

    token_types <- c(
        SYMBOL = SemanticTokenTypes$variable,
        SYMBOL_FUNCTION_CALL = SemanticTokenTypes[["function"]],
        SYMBOL_FORMALS = SemanticTokenTypes$parameter,
        SYMBOL_PACKAGE = SemanticTokenTypes$namespace,
        FUNCTION = SemanticTokenTypes$keyword,
        KEYWORD = SemanticTokenTypes$keyword,
        IF = SemanticTokenTypes$keyword,
        ELSE = SemanticTokenTypes$keyword,
        FOR = SemanticTokenTypes$keyword,
        IN = SemanticTokenTypes$keyword,
        WHILE = SemanticTokenTypes$keyword,
        REPEAT = SemanticTokenTypes$keyword,
        NEXT = SemanticTokenTypes$keyword,
        BREAK = SemanticTokenTypes$keyword,
        NUM_CONST = SemanticTokenTypes$number,
        INT_CONST = SemanticTokenTypes$number,
        FLOAT_CONST = SemanticTokenTypes$number,
        STRING = SemanticTokenTypes$string,
        STR_CONST = SemanticTokenTypes$string,
        COMMENT = SemanticTokenTypes$comment,
        LEFT_ASSIGN = SemanticTokenTypes$operator,
        RIGHT_ASSIGN = SemanticTokenTypes$operator,
        EQ_ASSIGN = SemanticTokenTypes$operator,
        EQ_SUB = SemanticTokenTypes$operator,
        EQ_FORMALS = SemanticTokenTypes$operator,
        SPECIAL = SemanticTokenTypes$operator,
        NS_GET = SemanticTokenTypes$operator,
        NS_GET_INT = SemanticTokenTypes$operator,
        GT = SemanticTokenTypes$operator,
        GE = SemanticTokenTypes$operator,
        LT = SemanticTokenTypes$operator,
        LE = SemanticTokenTypes$operator,
        EQ = SemanticTokenTypes$operator,
        NE = SemanticTokenTypes$operator,
        AND = SemanticTokenTypes$operator,
        OR = SemanticTokenTypes$operator,
        AND2 = SemanticTokenTypes$operator,
        OR2 = SemanticTokenTypes$operator,
        `$` = SemanticTokenTypes$operator,
        `'$'` = SemanticTokenTypes$operator,
        `@` = SemanticTokenTypes$operator,
        `'@'` = SemanticTokenTypes$operator,
        `~` = SemanticTokenTypes$operator,
        `'~'` = SemanticTokenTypes$operator,
        `?` = SemanticTokenTypes$operator,
        `'?'` = SemanticTokenTypes$operator,
        `:` = SemanticTokenTypes$operator,
        `':'` = SemanticTokenTypes$operator,
        `+` = SemanticTokenTypes$operator,
        `'+'` = SemanticTokenTypes$operator,
        `-` = SemanticTokenTypes$operator,
        `'-'` = SemanticTokenTypes$operator,
        `*` = SemanticTokenTypes$operator,
        `'*'` = SemanticTokenTypes$operator,
        `/` = SemanticTokenTypes$operator,
        `'/'` = SemanticTokenTypes$operator,
        `^` = SemanticTokenTypes$operator,
        `'^'` = SemanticTokenTypes$operator,
        `!` = SemanticTokenTypes$operator,
        `'!'` = SemanticTokenTypes$operator,
        PIPE = SemanticTokenTypes$operator,
        PIPEBIND = SemanticTokenTypes$operator,
        `\\` = SemanticTokenTypes$keyword,
        `'\\'` = SemanticTokenTypes$keyword,
        `'\\\\'` = SemanticTokenTypes$keyword
    )

    selected <- data$terminal & data$token %in% names(token_types)
    rows <- which(selected)
    if (!length(rows)) {
        return(empty_semantic_data())
    }

    # Nearly all parser tokens are single-line, so build those vectors in one
    # operation. UTF-16 conversion is only needed on lines containing UTF-8
    # multibyte characters; ASCII positions are already LSP code units.
    single_rows <- rows[data$line1[rows] == data$line2[rows]]
    tok <- data$token[single_rows]
    txt <- data$text[single_rows]
    row_ids <- data$id[single_rows]
    row_parents <- data$parent[single_rows]

    lines <- as.integer(data$line1[single_rows] - 1L)
    cols <- as.integer(data$col1[single_rows] - 1L)
    types <- as.integer(unname(token_types[tok]))
    modifiers <- as.integer(ifelse(
        tok == "SYMBOL_FORMALS",
        semantic_declaration_modifier,
        0L
    ))

    member_or_ns_parents <- data$parent[
        data$token %in% c("'$'", "'@'", "$", "@", "NS_GET", "NS_GET_INT") &
            data$parent > 0L
    ]
    is_return <- tok == "SYMBOL_FUNCTION_CALL" &
        txt == "return" &
        !(row_parents %in% member_or_ns_parents)
    types[is_return] <- SemanticTokenTypes$keyword

    fun_lhs_ids <- function_assignment_symbol_ids(data, syntax)
    if (length(fun_lhs_ids)) {
        fun_lhs <- row_ids %in% fun_lhs_ids
        types[fun_lhs] <- SemanticTokenTypes[["function"]]
        modifiers[fun_lhs] <- semantic_declaration_modifier
    }

    non_ascii_lines <- nchar(content, type = "bytes") !=
        nchar(content, type = "chars")
    units <- syntax_unit_columns(content, data$line1[single_rows],
        cols, data$col2[single_rows], non_ascii_lines)
    cols <- units$start
    lengths <- as.integer(units$end - units$start)

    # Semantic tokens are single-line. Split the rare multiline string into
    # one token per non-empty source line.
    multiline_rows <- setdiff(rows, single_rows)
    for (row_index in multiline_rows) {
        token_type <- unname(token_types[[data$token[[row_index]]]])
        modifier <- if (data$token[[row_index]] == "SYMBOL_FORMALS") {
            semantic_declaration_modifier
        } else {
            0L
        }
        for (line_number in seq.int(
            data$line1[[row_index]], data$line2[[row_index]])) {
            line_text <- if (line_number <= length(content)) content[[line_number]] else ""
            start_point <- if (line_number == data$line1[[row_index]]) {
                data$col1[[row_index]] - 1L
            } else {
                0L
            }
            end_point <- if (line_number == data$line2[[row_index]]) {
                data$col2[[row_index]]
            } else {
                nchar(line_text)
            }
            utf16 <- code_point_to_unit(line_text, c(start_point, end_point))
            token_length <- utf16[[2L]] - utf16[[1L]]
            if (token_length <= 0L) next

            lines <- c(lines, line_number - 1L)
            cols <- c(cols, utf16[[1L]])
            lengths <- c(lengths, token_length)
            types <- c(types, token_type)
            modifiers <- c(modifiers, modifier)
        }
    }

    keep <- lengths > 0L
    lines <- lines[keep]
    cols <- cols[keep]
    lengths <- lengths[keep]
    types <- types[keep]
    modifiers <- modifiers[keep]
    if (!length(lines)) return(empty_semantic_data())

    order_index <- order(lines, cols)
    lines <- lines[order_index]
    cols <- cols[order_index]
    lengths <- lengths[order_index]
    types <- types[order_index]
    modifiers <- modifiers[order_index]

    encoded <- .Call(
        "encode_semantic_tokens_c",
        lines, cols, lengths, types, modifiers,
        PACKAGE = "languageserver"
    )

    list(
        lines = lines,
        cols = cols,
        lengths = lengths,
        types = types,
        modifiers = modifiers,
        encoded = encoded
    )
}

#' Select semantic token data for an LSP range
#' @noRd
semantic_data_for_range <- function(data, range) {
    if (is.null(data) || !length(data$lines)) return(empty_semantic_data())
    .Call(
        "semantic_token_range_c", data,
        as.integer(c(range$start$line, range$start$character)),
        as.integer(c(range$end$line, range$end$character)),
        PACKAGE = "languageserver"
    )
}

#' Compute one compact semantic-token delta edit
#' @noRd
semantic_token_delta <- function(previous, current) {
    .Call("semantic_token_delta_c", previous, current, PACKAGE = "languageserver")
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
        "IF" = SemanticTokenTypes$keyword,
        "ELSE" = SemanticTokenTypes$keyword,
        "FOR" = SemanticTokenTypes$keyword,
        "IN" = SemanticTokenTypes$keyword,
        "WHILE" = SemanticTokenTypes$keyword,
        "REPEAT" = SemanticTokenTypes$keyword,
        "NEXT" = SemanticTokenTypes$keyword,
        "BREAK" = SemanticTokenTypes$keyword,
        "NUM_CONST" = SemanticTokenTypes$number,
        "INT_CONST" = SemanticTokenTypes$number,
        "FLOAT_CONST" = SemanticTokenTypes$number,
        "STRING" = SemanticTokenTypes$string,
        "STR_CONST" = SemanticTokenTypes$string,
        "COMMENT" = SemanticTokenTypes$comment,
        "LEFT_ASSIGN" = SemanticTokenTypes$operator,
        "RIGHT_ASSIGN" = SemanticTokenTypes$operator,
        "EQ_ASSIGN" = SemanticTokenTypes$operator,
        "EQ_SUB" = SemanticTokenTypes$operator,
        "EQ_FORMALS" = SemanticTokenTypes$operator,
        "SPECIAL" = SemanticTokenTypes$operator,
        "NS_GET" = SemanticTokenTypes$operator,
        "NS_GET_INT" = SemanticTokenTypes$operator,
        "GT" = SemanticTokenTypes$operator,
        "GE" = SemanticTokenTypes$operator,
        "LT" = SemanticTokenTypes$operator,
        "LE" = SemanticTokenTypes$operator,
        "EQ" = SemanticTokenTypes$operator,
        "NE" = SemanticTokenTypes$operator,
        "AND" = SemanticTokenTypes$operator,
        "OR" = SemanticTokenTypes$operator,
        "AND2" = SemanticTokenTypes$operator,
        "OR2" = SemanticTokenTypes$operator,
        "PIPE" = SemanticTokenTypes$operator,
        "PIPEBIND" = SemanticTokenTypes$operator,
        "OP-DOLLAR" = SemanticTokenTypes$operator,
        "OP-AT" = SemanticTokenTypes$operator,
        "OP-TILDE" = SemanticTokenTypes$operator,
        "OP-QUESTION" = SemanticTokenTypes$operator,
        "OP-COLON" = SemanticTokenTypes$operator,
        "OP-PLUS" = SemanticTokenTypes$operator,
        "OP-MINUS" = SemanticTokenTypes$operator,
        "OP-STAR" = SemanticTokenTypes$operator,
        "OP-SLASH" = SemanticTokenTypes$operator,
        "OP-CARET" = SemanticTokenTypes$operator,
        "OP-EXCLAMATION" = SemanticTokenTypes$operator,
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
    parse_data <- workspace$get_parse_data(uri)
    if (!is.null(parse_data$semantic_data)) {
        data <- parse_data$semantic_data
        if (!is.null(range)) data <- semantic_data_for_range(data, range)
        if (!length(data$lines)) return(list())
        return(lapply(seq_along(data$lines), function(i) {
            list(
                line = data$lines[[i]],
                col = data$cols[[i]],
                length = data$lengths[[i]],
                tokenType = data$types[[i]],
                tokenModifiers = data$modifiers[[i]]
            )
        }))
    }

    xdoc <- parse_data_xml(workspace$get_parse_data(uri))
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
        self::IF or
        self::ELSE or
        self::FOR or
        self::IN or
        self::WHILE or
        self::REPEAT or
        self::NEXT or
        self::BREAK or
        self::NUM_CONST or
        self::INT_CONST or
        self::FLOAT_CONST or
        self::STRING or
        self::STR_CONST or
        self::COMMENT or
        self::LEFT_ASSIGN or
        self::RIGHT_ASSIGN or
        self::EQ_ASSIGN or
        self::EQ_SUB or
        self::EQ_FORMALS or
        self::SPECIAL or
        self::NS_GET or
        self::NS_GET_INT or
        self::GT or
        self::GE or
        self::LT or
        self::LE or
        self::EQ or
        self::NE or
        self::AND or
        self::OR or
        self::AND2 or
        self::OR2 or
        self::PIPE or
        self::PIPEBIND or
        self::OP-DOLLAR or
        self::OP-AT or
        self::OP-TILDE or
        self::OP-QUESTION or
        self::OP-COLON or
        self::OP-PLUS or
        self::OP-MINUS or
        self::OP-STAR or
        self::OP-SLASH or
        self::OP-CARET or
        self::OP-EXCLAMATION or
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
        tok_text <- xml_text(token_node)

        line1 <- as.integer(xml_attr(token_node, "line1"))
        col1 <- as.integer(xml_attr(token_node, "col1"))
        line2 <- as.integer(xml_attr(token_node, "line2"))
        col2 <- as.integer(xml_attr(token_node, "col2"))

        # Skip if outside range (if range was specified)
        if (!is.null(end_pos) && line1 > end_pos$row + 1) {
            next
        }

        token_type <- get_token_type(token_name)
        modifiers <- 0L

        # Determine types and modifiers based on context
        if (token_name == "SYMBOL" && xml_symbol_is_function_assignment(token_node)) {
            token_type <- SemanticTokenTypes[["function"]]
            modifiers <- semantic_declaration_modifier
        } else if (token_name == "SYMBOL_FUNCTION_CALL") {
            if (identical(tok_text, "return") &&
                    xml_find_lgl(token_node, xml_not_qualified_symbol_xpath)) {
                token_type <- SemanticTokenTypes$keyword
            }
        } else if (token_name == "SYMBOL_FORMALS") {
            modifiers <- semantic_declaration_modifier
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

    # Pre-allocate vectors for better performance
    n <- length(tokens)
    lines <- integer(n)
    cols <- integer(n)
    lengths <- integer(n)
    types <- integer(n)
    mods <- integer(n)
    
    # Single loop extraction instead of 5 vapply calls
    # Explicitly coerce to maintain integer type
    for (i in seq_along(tokens)) {
        t <- tokens[[i]]
        lines[i] <- as.integer(t$line)
        cols[i] <- as.integer(t$col)
        lengths[i] <- as.integer(t$length)
        types[i] <- as.integer(t$tokenType)
        mods[i] <- as.integer(t$tokenModifiers)
    }

    # Only sort if necessary (XML traversal usually produces document order)
    # Create ordering key: line * large_number + col for single-pass sort check
    if (n > 1) {
        # Use numeric (64-bit) to avoid integer overflow on large files
        # Max line in typical files is hundreds, so numeric is safe and precise
        order_key <- lines * 1000000.0 + cols
        if (is.unsorted(order_key, strictly = FALSE)) {
            logger$info("encode_semantic_tokens: explicit ordering required for ", n, " tokens")
            order_idx <- order(lines, cols)
            lines <- lines[order_idx]
            cols <- cols[order_idx]
            lengths <- lengths[order_idx]
            types <- types[order_idx]
            mods <- mods[order_idx]
        }
    }

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

    semantic_data <- parse_data$semantic_data
    if (is.null(semantic_data)) {
        tokens <- extract_semantic_tokens(uri, workspace, document)
        result <- encode_semantic_tokens(tokens)
    } else {
        result <- list(data = semantic_data$encoded)
    }
    if (!is.null(parse_data$content_hash)) {
        result$resultId <- parse_data$content_hash
    }

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

    semantic_data <- parse_data$semantic_data
    if (is.null(semantic_data)) {
        tokens <- extract_semantic_tokens(uri, workspace, document, range = range)
        result <- encode_semantic_tokens(tokens)
    } else {
        selected <- semantic_data_for_range(semantic_data, range)
        result <- list(data = selected$encoded)
    }

    Response$new(
        id,
        result = result
    )
}

#' The response to a textDocument/semanticTokens/full/delta Request
#' @noRd
semantic_tokens_delta_reply <- function(id, uri, workspace, document,
    previous_result_id) {
    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
            (!is.null(parse_data$version) && parse_data$version != document$version)) {
        return(NULL)
    }

    current <- parse_data$semantic_data
    if (is.null(current)) {
        return(semantic_tokens_full_reply(id, uri, workspace, document))
    }

    previous <- NULL
    if (!is.null(previous_result_id) &&
            workspace$parse_cache$has(previous_result_id)) {
        cached <- workspace$parse_cache$get(previous_result_id)
        previous <- cached$semantic_data$encoded
    }

    if (is.null(previous)) {
        return(Response$new(id, result = list(
            resultId = parse_data$content_hash,
            data = current$encoded
        )))
    }

    Response$new(id, result = list(
        resultId = parse_data$content_hash,
        edits = semantic_token_delta(previous, current$encoded)
    ))
}
