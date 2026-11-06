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
semantic_documentation_modifier <- bitwShiftL(1L, SemanticTokenModifiers$documentation)
semantic_readonly_default_modifier <- bitwOr(
    bitwShiftL(1L, SemanticTokenModifiers$readonly),
    bitwShiftL(1L, SemanticTokenModifiers$defaultLibrary)
)

semantic_boolean_constants <- c("TRUE", "FALSE")
semantic_numeric_builtin_constants <- c(
    "Inf", "NaN", "NA",
    "NA_integer_", "NA_real_", "NA_complex_", "NA_character_"
)

is_semantic_ellipsis_symbol <- function(text) {
    text == "..." | grepl("^\\.\\.[0-9]+$", text)
}

#' Row indices of the target symbol for each assignment operator row
#' @noRd
assignment_target_symbol_rows <- function(data, assign_rows, id_parent) {
    if (!length(assign_rows)) return(integer())
    is_right <- data$token[assign_rows] == "RIGHT_ASSIGN"
    expr_rows <- which(data$token == "expr" & data$parent %in% data$parent[assign_rows])
    left_exprs <- expr_rows[data$parent[expr_rows] %in% data$parent[assign_rows[!is_right]]]
    right_exprs <- expr_rows[data$parent[expr_rows] %in% data$parent[assign_rows[is_right]]]
    target_ids <- data$id[c(
        left_exprs[!duplicated(data$parent[left_exprs])],
        right_exprs[!duplicated(data$parent[right_exprs], fromLast = TRUE)]
    )]
    sym_rows <- which(data$token == "SYMBOL" & data$parent > 0L)
    p <- data$parent[sym_rows]
    repeat {
        move <- p > 0L & !(p %in% target_ids)
        if (!any(move)) break
        p[move] <- id_parent[p[move]]
    }
    m <- match(target_ids, p, nomatch = 0L)
    sym_rows[m[m > 0L]]
}

#' Innermost enclosing function `<expr>` id for each row (0L if top-level)
#' @noRd
enclosing_function_scope_ids <- function(rows, data, id_parent) {
    fn_parents <- unique(data$parent[
        data$token %in% c("FUNCTION", "\\", "'\\'", "'\\\\'") &
            data$parent > 0L
    ])
    p <- data$parent[rows]
    repeat {
        move <- p > 0L & !(p %in% fn_parents)
        if (!any(move)) break
        p[move] <- id_parent[p[move]]
    }
    p
}

#' Local variable and loop-variable binding scopes for candidate symbols
#' @noRd
local_symbol_binding_scopes <- function(data, cand_rows, cand_names, id_parent) {
    assign_rows <- which(
        data$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN") &
            data$text %in% c("<-", "=", "->") &
            data$parent > 0L
    )
    target_rows <- assignment_target_symbol_rows(data, assign_rows, id_parent)
    assigned_idx <- which(cand_rows %in% target_rows)

    forcond_ids <- data$id[data$token == "forcond"]
    for_idx <- if (length(forcond_ids)) {
        which(data$parent[cand_rows] %in% forcond_ids)
    } else {
        integer()
    }

    bind_idx <- unique(c(assigned_idx, for_idx))
    if (!length(bind_idx)) {
        return(list(scope_ids = integer(), names = character()))
    }

    fn_scope <- enclosing_function_scope_ids(cand_rows[bind_idx], data, id_parent)
    bind_names <- cand_names[bind_idx]
    in_fn <- fn_scope > 0L
    scope_ids <- fn_scope[in_fn]
    scope_names <- bind_names[in_fn]

    if (length(for_idx)) {
        for_expr_ids <- id_parent[data$parent[cand_rows[for_idx]]]
        valid_for <- for_expr_ids > 0L
        scope_ids <- c(scope_ids, for_expr_ids[valid_for])
        scope_names <- c(scope_names, cand_names[for_idx[valid_for]])
    }

    list(
        scope_ids = as.integer(scope_ids),
        names = scope_names
    )
}

xml_not_qualified_symbol_xpath <- "
    not(
        preceding-sibling::OP-DOLLAR
        or preceding-sibling::OP-AT
        or preceding-sibling::NS_GET
        or preceding-sibling::NS_GET_INT
    )
"

# Match bare and replacement assignment receiver symbols (excluding <<- / ->> / := and member/ns RHS)
xml_scope_local_symbols_xpath <- paste0(
    "(
        .//LEFT_ASSIGN[text() = '<-']/preceding-sibling::expr[1]
        | .//EQ_ASSIGN/preceding-sibling::expr[1]
        | .//RIGHT_ASSIGN[text() = '->']/following-sibling::expr[1]
    )/descendant::SYMBOL[",
    xml_not_qualified_symbol_xpath,
    "][1]
    | .//forcond/SYMBOL"
)

xml_superassign_target_expr_xpath <- "
    ancestor::expr[
        following-sibling::LEFT_ASSIGN[text() = '<<-']
        or preceding-sibling::RIGHT_ASSIGN[text() = '->>']
    ][1]
"

#' Resolve the nearest lexical binding kind ("formal", "local", or "none") for an XML SYMBOL
#' @noRd
xml_symbol_binding_kind <- function(token_node, sym_name) {
    if (xml_find_lgl(token_node, paste0("boolean(", xml_superassign_target_expr_xpath, ")"))) {
        target_expr <- xml_find_first(token_node, xml_superassign_target_expr_xpath)
        first_sym <- xml_find_first(
            target_expr,
            paste0("descendant::SYMBOL[", xml_not_qualified_symbol_xpath, "][1]")
        )
        if (identical(first_sym, token_node)) {
            if (xml_find_lgl(target_expr, "not(ancestor::expr[FUNCTION or OP-LAMBDA])")) {
                return("none")
            }
            token_node <- xml_find_first(target_expr, "ancestor::expr[FUNCTION or OP-LAMBDA][1]")
        }
    }
    ancestor_formals <- setdiff(
        reference_symbol_names(
            xml_text(xml_find_all(token_node, "ancestor::expr/SYMBOL_FORMALS"))
        ),
        "..."
    )
    if (!(sym_name %in% ancestor_formals)) {
        return("none")
    }
    if (xml_find_lgl(
        token_node,
        "not(ancestor::expr[FUNCTION or OP-LAMBDA][2] or ancestor::expr/forcond)"
    )) {
        return("formal")
    }
    scopes <- xml_find_all(
        token_node,
        "ancestor::expr[FUNCTION or OP-LAMBDA or forcond]"
    )
    loop_syms <- xml_find_chr(scopes, "string(forcond/SYMBOL)")
    is_for <- nzchar(loop_syms)
    fn_depths <- cumsum(!is_for)
    for (i in rev(seq_along(scopes))) {
        if (is_for[[i]]) {
            if (reference_symbol_names(loop_syms[[i]]) == sym_name) {
                return("local")
            }
            next
        }
        scope_node <- scopes[[i]]
        formal_names <- setdiff(
            reference_symbol_names(
                xml_text(xml_find_all(scope_node, "SYMBOL_FORMALS"))
            ),
            "..."
        )
        if (sym_name %in% formal_names) return("formal")

        local_nodes <- xml_find_all(scope_node, xml_scope_local_symbols_xpath)
        if (length(local_nodes)) {
            local_names <- reference_symbol_names(xml_text(local_nodes))
            matching_nodes <- local_nodes[local_names == sym_name]
            if (length(matching_nodes)) {
                not_nested_xpath <- sprintf(
                    "not(ancestor::expr[FUNCTION or OP-LAMBDA][%d])",
                    fn_depths[[i]] + 1L
                )
                if (any(xml_find_lgl(matching_nodes, not_nested_xpath))) {
                    return("local")
                }
            }
        }
    }
    "none"
}

#' Parse-data ids of body symbols resolving to enclosing `SYMBOL_FORMALS`
#' @noRd
formal_parameter_symbol_ids <- function(data, member_or_ns_parents = NULL) {
    formal_rows <- which(
        data$token == "SYMBOL_FORMALS" &
            data$parent > 0L &
            !(data$text %in% c("...", "`...`"))
    )
    if (!length(formal_rows)) return(integer())
    formal_names <- reference_symbol_names(data$text[formal_rows])

    unique_names <- unique(formal_names)
    if (is.null(member_or_ns_parents)) {
        member_or_ns_parents <- data$parent[
            data$token %in% c("'$'", "'@'", "$", "@", "NS_GET", "NS_GET_INT") &
                data$parent > 0L
        ]
    }
    sym_rows <- which(
        data$token == "SYMBOL" &
            data$parent > 0L &
            !(data$parent %in% member_or_ns_parents)
    )
    sym_names <- reference_symbol_names(data$text[sym_rows])
    in_formals <- sym_names %in% unique_names
    cand_rows <- sym_rows[in_formals]
    if (!length(cand_rows)) return(integer())
    cand_names <- sym_names[in_formals]

    max_id <- max(data$id, data$parent, 0L)
    id_parent <- integer(max_id)
    valid_nodes <- data$id > 0L & data$parent > 0L
    id_parent[data$id[valid_nodes]] <- data$parent[valid_nodes]

    local_bindings <- local_symbol_binding_scopes(
        data, cand_rows, cand_names, id_parent
    )

    formal_parents <- data$parent[formal_rows]
    unique_scopes <- unique(c(formal_parents, local_bindings$scope_ids))
    scope_idx <- integer(max_id)
    scope_idx[unique_scopes] <- seq_along(unique_scopes)

    n_names <- length(unique_names)
    n_scopes <- length(unique_scopes)
    formal_name_idx <- match(formal_names, unique_names)
    has_formal <- logical(n_names * n_scopes)
    has_formal[(scope_idx[formal_parents] - 1L) * n_names + formal_name_idx] <- TRUE

    has_local <- logical(n_names * n_scopes)
    if (length(local_bindings$scope_ids)) {
        local_name_idx <- match(local_bindings$names, unique_names)
        has_local[(scope_idx[local_bindings$scope_ids] - 1L) * n_names + local_name_idx] <- TRUE
    }

    super_assign_rows <- which(
        data$token %in% c("LEFT_ASSIGN", "RIGHT_ASSIGN") &
            data$text %in% c("<<-", "->>") &
            data$parent > 0L
    )
    super_target_rows <- assignment_target_symbol_rows(data, super_assign_rows, id_parent)

    cand_name_idx <- match(cand_names, unique_names)
    anc <- data$parent[cand_rows]
    is_super <- cand_rows %in% super_target_rows
    if (any(is_super)) {
        fn_ids <- enclosing_function_scope_ids(cand_rows[is_super], data, id_parent)
        anc[is_super] <- ifelse(fn_ids > 0L, id_parent[fn_ids], 0L)
    }
    matched <- logical(length(cand_rows))
    active <- which(anc > 0L)
    anc <- anc[active]
    cand_name_idx <- cand_name_idx[active]
    steps <- 0L
    max_steps <- nrow(data)

    while (length(active) && steps <= max_steps) {
        s <- scope_idx[anc]
        at_scope <- which(s > 0L)
        if (length(at_scope)) {
            lookup_idx <- (s[at_scope] - 1L) * n_names + cand_name_idx[at_scope]
            hit <- (has_formal | has_local)[lookup_idx]
            if (any(hit)) {
                hit_idx <- at_scope[hit]
                matched[active[at_scope[has_formal[lookup_idx]]]] <- TRUE
                active <- active[-hit_idx]
                anc <- anc[-hit_idx]
                cand_name_idx <- cand_name_idx[-hit_idx]
                if (!length(active)) break
            }
        }
        anc <- id_parent[anc]
        keep <- anc > 0L
        if (!all(keep)) {
            active <- active[keep]
            anc <- anc[keep]
            cand_name_idx <- cand_name_idx[keep]
        }
        steps <- steps + 1L
    }

    as.integer(data$id[cand_rows[matched]])
}

#' Split a Roxygen2 comment line (`#' ...`) into semantic token spans
#' @noRd
roxygen_comment_spans <- function(comment_text, base_col0) {
    total_len <- nchar(comment_text)
    m <- regexec(
        "^#'\\s*(@[[:alpha:]][[:alnum:]._]*)(?:\\s+(`(?:\\\\.|[^`\\\\])+`|[^[:space:]]+))?",
        comment_text,
        perl = TRUE
    )[[1L]]
    if (m[[1L]] == -1L) {
        return(list(
            starts = as.integer(base_col0),
            ends = as.integer(base_col0 + total_len),
            types = as.integer(SemanticTokenTypes$comment),
            modifiers = as.integer(semantic_documentation_modifier)
        ))
    }
    match_lens <- attr(m, "match.length")
    tag_start <- m[[2L]] - 1L
    tag_end <- tag_start + match_lens[[2L]]
    tag_text <- substr(comment_text, m[[2L]], tag_end)

    starts <- c(base_col0, base_col0 + tag_start)
    ends <- c(base_col0 + tag_start, base_col0 + tag_end)
    types <- c(SemanticTokenTypes$comment, SemanticTokenTypes$macro)
    last_end <- tag_end

    if (identical(tag_text, "@param") && length(m) >= 3L && m[[3L]] > 0L) {
        param_start <- m[[3L]] - 1L
        param_end <- param_start + match_lens[[3L]]
        starts <- c(starts, base_col0 + param_start)
        ends <- c(ends, base_col0 + param_end)
        types <- c(types, SemanticTokenTypes$parameter)
        last_end <- param_end
    }

    if (last_end < total_len) {
        starts <- c(starts, base_col0 + last_end)
        ends <- c(ends, base_col0 + total_len)
        types <- c(types, SemanticTokenTypes$comment)
    }

    list(
        starts = as.integer(starts),
        ends = as.integer(ends),
        types = as.integer(types),
        modifiers = rep.int(as.integer(semantic_documentation_modifier), length(starts))
    )
}

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
        if (length(children) != 3L) {
            return(FALSE)
        }
        if (!identical(xml_name(children[c(1L, 3L)]), c("OP-LEFT-PAREN", "OP-RIGHT-PAREN"))) {
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
        SYMBOL_SUB = SemanticTokenTypes$parameter,
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
        NULL_CONST = SemanticTokenTypes$keyword,
        PLACEHOLDER = SemanticTokenTypes$variable,
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
    is_roxygen <- data$token[single_rows] == "COMMENT" &
        startsWith(data$text[single_rows], "#'")
    roxygen_rows <- single_rows[is_roxygen]
    std_rows <- single_rows[!is_roxygen]

    tok <- data$token[std_rows]
    txt <- data$text[std_rows]
    row_ids <- data$id[std_rows]
    row_parents <- data$parent[std_rows]

    line1_vec <- as.integer(data$line1[std_rows])
    start_cp <- as.integer(data$col1[std_rows] - 1L)
    end_cp <- as.integer(data$col2[std_rows])
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

    is_null_or_placeholder <- tok %in% c("NULL_CONST", "PLACEHOLDER")
    modifiers[is_null_or_placeholder] <- semantic_readonly_default_modifier

    is_bool_num <- tok == "NUM_CONST" & txt %in% semantic_boolean_constants
    types[is_bool_num] <- SemanticTokenTypes$keyword
    modifiers[is_bool_num] <- semantic_readonly_default_modifier

    is_na_inf <- tok == "NUM_CONST" & txt %in% semantic_numeric_builtin_constants
    modifiers[is_na_inf] <- semantic_readonly_default_modifier

    plain_sym <- tok == "SYMBOL" & !(row_parents %in% member_or_ns_parents)
    param_sym_ids <- formal_parameter_symbol_ids(data, member_or_ns_parents)
    if (length(param_sym_ids)) {
        is_param_ref <- plain_sym & (row_ids %in% param_sym_ids)
        types[is_param_ref] <- SemanticTokenTypes$parameter
    }
    is_ellipsis_sym <- plain_sym & is_semantic_ellipsis_symbol(txt)
    types[is_ellipsis_sym] <- SemanticTokenTypes$variable
    modifiers[is_ellipsis_sym] <- semantic_readonly_default_modifier

    fun_lhs_ids <- function_assignment_symbol_ids(data, syntax)
    if (length(fun_lhs_ids)) {
        fun_lhs <- row_ids %in% fun_lhs_ids
        types[fun_lhs] <- SemanticTokenTypes[["function"]]
        modifiers[fun_lhs] <- semantic_declaration_modifier
    }

    if (length(roxygen_rows)) {
        span_list <- lapply(roxygen_rows, function(row_index) {
            roxygen_comment_spans(
                data$text[[row_index]],
                data$col1[[row_index]] - 1L
            )
        })
        span_counts <- vapply(span_list, \(s) length(s$starts), integer(1L))
        rx_starts <- unlist(lapply(span_list, `[[`, "starts"), use.names = FALSE)
        line1_vec <- c(
            line1_vec,
            rep.int(as.integer(data$line1[roxygen_rows]), span_counts)
        )
        start_cp <- c(start_cp, rx_starts)
        end_cp <- c(end_cp, unlist(lapply(span_list, `[[`, "ends"), use.names = FALSE))
        types <- c(types, unlist(lapply(span_list, `[[`, "types"), use.names = FALSE))
        modifiers <- c(
            modifiers,
            rep.int(as.integer(semantic_documentation_modifier), length(rx_starts))
        )
    }

    lines <- as.integer(line1_vec - 1L)
    non_ascii_lines <- nchar(content, type = "bytes") !=
        nchar(content, type = "chars")
    units <- syntax_unit_columns(
        content, line1_vec, start_cp, end_cp, non_ascii_lines
    )
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

semantic_xml_token_types <- c(
    SYMBOL = SemanticTokenTypes$variable,
    SYMBOL_FUNCTION_CALL = SemanticTokenTypes[["function"]],
    SYMBOL_FORMALS = SemanticTokenTypes$parameter,
    SYMBOL_SUB = SemanticTokenTypes$parameter,
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
    NULL_CONST = SemanticTokenTypes$keyword,
    PLACEHOLDER = SemanticTokenTypes$variable,
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
    PIPE = SemanticTokenTypes$operator,
    PIPEBIND = SemanticTokenTypes$operator,
    `OP-DOLLAR` = SemanticTokenTypes$operator,
    `OP-AT` = SemanticTokenTypes$operator,
    `OP-TILDE` = SemanticTokenTypes$operator,
    `OP-QUESTION` = SemanticTokenTypes$operator,
    `OP-COLON` = SemanticTokenTypes$operator,
    `OP-PLUS` = SemanticTokenTypes$operator,
    `OP-MINUS` = SemanticTokenTypes$operator,
    `OP-STAR` = SemanticTokenTypes$operator,
    `OP-SLASH` = SemanticTokenTypes$operator,
    `OP-CARET` = SemanticTokenTypes$operator,
    `OP-EXCLAMATION` = SemanticTokenTypes$operator,
    `OP-PIPE` = SemanticTokenTypes$operator,
    OP = SemanticTokenTypes$operator,
    `OP-LAMBDA` = SemanticTokenTypes$keyword
)

semantic_tokens_xpath <- paste0(
    "//*[",
    paste0("self::", names(semantic_xml_token_types), collapse = " or "),
    "]"
)

#' Get semantic token type for an XML token
#'
#' Maps R parser token names to LSP semantic token types
#' @noRd
get_token_type <- function(token_name) {
    idx <- match(token_name, names(semantic_xml_token_types), nomatch = 0L)
    if (idx > 0L) semantic_xml_token_types[[idx]] else SemanticTokenTypes$variable
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
    token_elements <- xml_find_all(xdoc, semantic_tokens_xpath)

    if (length(token_elements) == 0) {
        return(list())
    }

    end_pos <- NULL
    if (!is.null(range)) {
        end_pos <- document$from_lsp_position(range$end)
    }

    tokens <- list()
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

        line_text <- if (line1 <= length(document$content)) document$content[line1] else ""

        if (token_name == "COMMENT" && startsWith(tok_text, "#'")) {
            spans <- roxygen_comment_spans(tok_text, col1 - 1L)
            n_spans <- length(spans$starts)
            utf16_cols <- code_point_to_unit(
                line_text,
                c(spans$starts, spans$ends)
            )
            for (k in seq_len(n_spans)) {
                token_col <- utf16_cols[[k]]
                token_length <- utf16_cols[[n_spans + k]] - token_col
                idx <- idx + 1L
                tokens[[idx]] <- list(
                    line = as.integer(line1 - 1L),
                    col = as.integer(token_col),
                    length = as.integer(token_length),
                    tokenType = as.integer(spans$types[[k]]),
                    tokenModifiers = as.integer(spans$modifiers[[k]])
                )
            }
            next
        }

        token_type <- get_token_type(token_name)
        modifiers <- 0L

        # Determine types and modifiers based on context
        if (token_name == "SYMBOL") {
            if (xml_symbol_is_function_assignment(token_node)) {
                token_type <- SemanticTokenTypes[["function"]]
                modifiers <- semantic_declaration_modifier
            } else if (xml_find_lgl(token_node, xml_not_qualified_symbol_xpath)) {
                if (is_semantic_ellipsis_symbol(tok_text)) {
                    token_type <- SemanticTokenTypes$variable
                    modifiers <- semantic_readonly_default_modifier
                } else {
                    sym_name <- reference_symbol_names(tok_text)
                    if (identical(xml_symbol_binding_kind(token_node, sym_name), "formal")) {
                        token_type <- SemanticTokenTypes$parameter
                    }
                }
            }
        } else if (token_name == "SYMBOL_FUNCTION_CALL") {
            if (identical(tok_text, "return") && xml_find_lgl(token_node, xml_not_qualified_symbol_xpath)) {
                token_type <- SemanticTokenTypes$keyword
            }
        } else if (token_name == "SYMBOL_FORMALS") {
            modifiers <- semantic_declaration_modifier
        } else if (token_name %in% c("NULL_CONST", "PLACEHOLDER")) {
            modifiers <- semantic_readonly_default_modifier
        } else if (token_name == "NUM_CONST") {
            if (tok_text %in% semantic_boolean_constants) {
                token_type <- SemanticTokenTypes$keyword
                modifiers <- semantic_readonly_default_modifier
            } else if (tok_text %in% semantic_numeric_builtin_constants) {
                modifiers <- semantic_readonly_default_modifier
            }
        }

        # Convert positions to UTF-16 code units for LSP
        # Parse data uses 1-based code point positions, LSP uses 0-based UTF-16 units
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
    if (is.null(parse_data)) {
        return(NULL)
    }
    if (!is.null(parse_data$version) && parse_data$version != document$version) {
        return(NULL)
    }

    current <- parse_data$semantic_data
    if (is.null(current)) {
        return(semantic_tokens_full_reply(id, uri, workspace, document))
    }

    previous <- NULL
    if (!is.null(previous_result_id) && workspace$parse_cache$has(previous_result_id)) {
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
