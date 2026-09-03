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
#' Same cases as `scope_completion_functs_xpath` in `completion.R`.
#' @noRd
function_assignment_symbol_ids <- function(data) {
    fun_expr_ids <- unique(data$parent[data$token %in% c("FUNCTION", "'\\\\'")])
    if (!length(fun_expr_ids)) {
        return(integer())
    }

    assign_rows <- which(
        data$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")
    )
    lhs_ids <- integer()
    for (row in assign_rows) {
        children <- data[data$parent == data$parent[[row]], , drop = FALSE]
        assign_pos <- match(data$id[[row]], children$id)
        if (is.na(assign_pos)) {
            next
        }
        before_idx <- seq_len(assign_pos - 1L)
        after_idx <- if (assign_pos < nrow(children)) {
            seq.int(assign_pos + 1L, nrow(children))
        } else {
            integer()
        }
        if (data$token[[row]] == "RIGHT_ASSIGN") {
            rhs <- children[before_idx, , drop = FALSE]
            lhs <- children[after_idx, , drop = FALSE]
        } else {
            lhs <- children[before_idx, , drop = FALSE]
            rhs <- children[after_idx, , drop = FALSE]
        }
        if (!any(rhs$id %in% fun_expr_ids)) {
            next
        }
        for (expr_id in lhs$id[lhs$token %in% c("expr", "expr_or_assign_or_help")]) {
            child_rows <- which(data$parent == expr_id)
            terminal_rows <- child_rows[data$terminal[child_rows]]
            if (length(terminal_rows) == 1L &&
                    data$token[[terminal_rows]] == "SYMBOL") {
                lhs_ids <- c(lhs_ids, data$id[[terminal_rows]])
            }
        }
    }
    unique(lhs_ids)
}

#' Whether an XML SYMBOL is assigned `function()` or `\()`
#' @noRd
xml_symbol_is_function_assignment <- function(node) {
    !inherits(
        xml_find_first(node, paste(
            "./parent::expr[count(*)=1]/following-sibling::*[self::LEFT_ASSIGN or self::EQ_ASSIGN][following-sibling::expr/*[self::FUNCTION or self::OP-LAMBDA]]",
            "./parent::expr[count(*)=1]/preceding-sibling::RIGHT_ASSIGN[preceding-sibling::expr/*[self::FUNCTION or self::OP-LAMBDA]]",
            sep = "|"
        )),
        "xml_missing"
    )
}

#' Build a compact semantic token index from R parse data
#'
#' This runs in the parse worker. Keeping ordinary integer vectors here avoids
#' repeatedly walking the XML document on the main language-server thread.
#' @noRd
semantic_parse_data <- function(data, content) {
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
        NUM_CONST = SemanticTokenTypes$number,
        INT_CONST = SemanticTokenTypes$number,
        FLOAT_CONST = SemanticTokenTypes$number,
        STRING = SemanticTokenTypes$string,
        STR_CONST = SemanticTokenTypes$string,
        COMMENT = SemanticTokenTypes$comment,
        LEFT_ASSIGN = SemanticTokenTypes$operator,
        RIGHT_ASSIGN = SemanticTokenTypes$operator,
        EQ_ASSIGN = SemanticTokenTypes$operator,
        `$` = SemanticTokenTypes$operator,
        PIPE = SemanticTokenTypes$operator,
        `\\` = SemanticTokenTypes$keyword
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
    lines <- as.integer(data$line1[single_rows] - 1L)
    cols <- as.integer(data$col1[single_rows] - 1L)
    lengths <- as.integer(data$col2[single_rows] - data$col1[single_rows] + 1L)
    types <- as.integer(unname(token_types[data$token[single_rows]]))
    modifiers <- ifelse(
        data$token[single_rows] == "SYMBOL_FORMALS",
        bitwShiftL(1L, SemanticTokenModifiers$declaration),
        0L
    )
    modifiers <- as.integer(modifiers)

    fun_lhs_ids <- function_assignment_symbol_ids(data)
    if (length(fun_lhs_ids)) {
        fun_lhs <- data$id[single_rows] %in% fun_lhs_ids
        types[fun_lhs] <- SemanticTokenTypes[["function"]]
        modifiers[fun_lhs] <- bitwOr(
            modifiers[fun_lhs],
            bitwShiftL(1L, SemanticTokenModifiers$declaration)
        )
    }

    non_ascii_lines <- nchar(content, type = "bytes") !=
        nchar(content, type = "chars")
    convert <- which(non_ascii_lines[data$line1[single_rows]])
    for (i in convert) {
        row_index <- single_rows[[i]]
        line_text <- content[[data$line1[[row_index]]]]
        utf16 <- code_point_to_unit(
            line_text,
            c(data$col1[[row_index]] - 1L, data$col2[[row_index]])
        )
        cols[[i]] <- utf16[[1L]]
        lengths[[i]] <- utf16[[2L]] - utf16[[1L]]
    }

    # Semantic tokens are single-line. Split the rare multiline string into
    # one token per non-empty source line.
    multiline_rows <- setdiff(rows, single_rows)
    for (row_index in multiline_rows) {
        token_type <- unname(token_types[[data$token[[row_index]]]])
        modifier <- if (data$token[[row_index]] == "SYMBOL_FORMALS") {
            bitwShiftL(1L, SemanticTokenModifiers$declaration)
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
    if (is.null(data) || !length(data$lines)) {
        return(empty_semantic_data())
    }

    start <- range$start
    end <- range$end
    after_start <- data$lines > start$line |
        data$lines == start$line & data$cols + data$lengths > start$character
    before_end <- data$lines < end$line |
        data$lines == end$line & data$cols < end$character
    keep <- which(after_start & before_end)
    if (!length(keep)) {
        return(empty_semantic_data())
    }

    lines <- data$lines[keep]
    cols <- data$cols[keep]
    lengths <- data$lengths[keep]
    types <- data$types[keep]
    modifiers <- data$modifiers[keep]
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

#' Compute one compact semantic-token delta edit
#' @noRd
semantic_token_delta <- function(previous, current) {
    if (identical(previous, current)) {
        return(list())
    }

    previous_count <- length(previous) %/% 5L
    current_count <- length(current) %/% 5L
    prefix <- 0L
    shared <- min(previous_count, current_count)
    while (prefix < shared) {
        offset <- prefix * 5L
        indices <- seq.int(offset + 1L, offset + 5L)
        if (!identical(previous[indices], current[indices])) break
        prefix <- prefix + 1L
    }

    suffix <- 0L
    while (suffix < previous_count - prefix &&
            suffix < current_count - prefix) {
        previous_start <- (previous_count - suffix - 1L) * 5L + 1L
        current_start <- (current_count - suffix - 1L) * 5L + 1L
        if (!identical(
            previous[seq.int(previous_start, previous_start + 4L)],
            current[seq.int(current_start, current_start + 4L)])) {
            break
        }
        suffix <- suffix + 1L
    }

    current_first <- prefix * 5L + 1L
    current_last <- (current_count - suffix) * 5L
    replacement <- if (current_first <= current_last) {
        current[seq.int(current_first, current_last)]
    } else {
        integer()
    }

    edit <- list(
        start = prefix * 5L,
        deleteCount = (previous_count - prefix - suffix) * 5L
    )
    if (length(replacement)) edit$data <- replacement
    list(edit)
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
        if (token_name == "SYMBOL" && xml_symbol_is_function_assignment(token_node)) {
            token_type <- SemanticTokenTypes[["function"]]
            modifiers <- bitwOr(modifiers, 2^SemanticTokenModifiers$declaration)
        } else if (token_name == "SYMBOL_FUNCTION_CALL") {
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
