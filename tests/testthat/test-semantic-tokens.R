test_that("Unicode columns are converted once per line with scalar-equivalent positions", {
    content <- c(paste0("\"", intToUtf8(0x1f600), "\"; ", intToUtf8(0x03b1), " <- 1; ",
        intToUtf8(0x03b1), " + ", intToUtf8(0x03b1)), "plain <- plain", "é <- é")
    lines <- c(1L, 1L, 2L, 1L, 3L, NA_integer_)
    starts <- c(0L, 5L, 0L, 12L, 0L, NA_integer_)
    ends <- c(3L, 6L, 5L, 13L, 1L, NA_integer_)
    scalar <- function(points) {
        vapply(seq_along(points), function(i) {
            if (is.na(lines[[i]])) return(NA_integer_)
            code_point_to_unit(content[[lines[[i]]]], points[[i]])
        }, integer(1L))
    }
    expected <- list(start = scalar(starts), end = scalar(ends))
    calls <- list()
    convert <- syntax_unit_columns
    stub(convert, "code_point_to_unit", function(line, pts) {
        calls[[length(calls) + 1L]] <<- line
        code_point_to_unit(line, pts)
    })
    expect_identical(convert(content, lines, starts, ends), expected)
    expect_identical(unlist(calls, use.names = FALSE), content[c(1L, 3L)])
    expect_identical(syntax_unit_columns(character(), integer(), integer(), integer()),
        list(start = integer(), end = integer()))
})

test_that("Semantic tokens full works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test <- function(x, y) {",
            "  x + y",
            "}"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_semantic_tokens_full(temp_file)
    expect_true(!is.null(result$data))
    expect_true(length(result$data) > 0)
    # data should be multiples of 5 (line delta, start delta, length, type, modifiers)
    expect_equal(length(result$data) %% 5, 0)
})

test_that("Semantic tokens range works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test_var <- 42",
            "another_var <- test_var + 1"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    # Request tokens for the first line only
    result <- client %>% respond_semantic_tokens_range(
        temp_file,
        start_pos = c(0, 0),
        end_pos = c(1, 0)
    )
    expect_true(!is.null(result$data))
    # data should be multiples of 5
    expect_equal(length(result$data) %% 5, 0)
    decoded_lines <- cumsum(matrix(result$data, ncol = 5, byrow = TRUE)[, 1])
    expect_true(all(decoded_lines == 0L))
})

test_that("Semantic token range honors both range boundaries", {
    content <- sprintf("value_%02d <- %d", 1:10, 1:10)
    fixture <- provider_fixture(content)
    response <- semantic_tokens_range_reply(
        1L, fixture$uri, fixture$workspace, fixture$document,
        list(
            start = list(line = 5L, character = 0L),
            end = list(line = 7L, character = 0L)
        )
    )

    encoded <- matrix(response$result$data, ncol = 5, byrow = TRUE)
    decoded_lines <- cumsum(encoded[, 1])
    expect_true(length(decoded_lines) > 0L)
    expect_true(all(decoded_lines >= 5L & decoded_lines < 7L))
})

test_that("Semantic token deltas reconstruct the current result", {
    previous <- as.integer(c(0, 0, 1, 8, 0, 1, 0, 1, 8, 0))
    current <- as.integer(c(0, 0, 1, 8, 0, 1, 2, 1, 8, 0))
    edits <- semantic_token_delta(previous, current)

    expect_length(edits, 1L)
    edit <- edits[[1L]]
    before <- if (edit$start) previous[seq_len(edit$start)] else integer()
    after_start <- edit$start + edit$deleteCount + 1L
    after <- if (after_start <= length(previous)) {
        previous[seq.int(after_start, length(previous))]
    } else {
        integer()
    }
    expect_identical(c(before, edit$data, after), current)
})

test_that("Incomplete documents produce current empty parse data", {
    parsed <- parse_document("file:///incomplete.R", "x <- function(")
    expect_true(parsed$parse_error)
    expect_length(parsed$semantic_data$encoded, 0L)
    expect_false(is.null(parsed$xml_data))
})

test_that("Semantic token delta requests work through the language server", {
    skip_on_cran()
    client <- language_client()
    path <- withr::local_tempfile(fileext = ".R")
    writeLines("value <- 1", path)
    client %>% did_open(path)
    uri <- path_to_uri(path)

    previous <- respond_semantic_tokens_full(client, path)
    expect_true(nzchar(previous$resultId))
    notify(client, "textDocument/didChange", list(
        textDocument = list(uri = uri, version = 2L),
        contentChanges = list(list(
            range = list(
                start = list(line = 0L, character = 9L),
                end = list(line = 0L, character = 10L)
            ),
            text = "2"
        ))
    ))

    delta <- respond_semantic_tokens_delta(
        client, path, previous$resultId,
        retry_when = function(result) is.null(result$resultId)
    )
    expect_true(nzchar(delta$resultId))
    expect_false(identical(delta$resultId, previous$resultId))
    expect_false(is.null(delta$edits) && is.null(delta$data))
})

test_that("Semantic tokens contain expected types", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "my_func <- function(param1, param2) {",
            "  result <- param1 + param2",
            "  result",
            "}"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_semantic_tokens_full(temp_file)
    expect_true(!is.null(result$data))
    expect_true(length(result$data) > 0)

    # Check that we have some tokens (data array with valid entries)
    # Each token is 5 elements: [line_delta, start_delta, length, type, modifiers]
    token_count <- length(result$data) %/% 5
    expect_true(token_count > 0)
})

test_that("Semantic parse data handles UTF-16 and multiline tokens", {
    astral <- intToUtf8(0x10400)
    content <- c(
        paste0('label <- "', astral, '"'),
        'description <- "first',
        'second"',
        "fn <- function(argument) argument + 1L"
    )
    parsed <- parse(text = content, keep.source = TRUE)
    data <- utils::getParseData(parsed, includeText = TRUE)

    semantic <- semantic_parse_data(data, content)

    expect_gt(length(semantic$lines), 0L)
    expect_identical(length(semantic$encoded), length(semantic$lines) * 5L)
    expect_true(all(diff(semantic$lines) >= 0L))
    string_rows <- which(semantic$types == SemanticTokenTypes$string)
    expect_true(all(c(0L, 1L, 2L) %in% semantic$lines[string_rows]))
    astral_string <- which(
        semantic$lines == 0L & semantic$types == SemanticTokenTypes$string
    )
    expect_equal(semantic$lengths[astral_string], 4L)

    expect_identical(
        semantic_parse_data(NULL, content),
        empty_semantic_data()
    )
    expect_identical(
        semantic_parse_data(data[!data$terminal, , drop = FALSE], content),
        empty_semantic_data()
    )
})

test_that("Function assignment names are function declarations", {
    content <- c(
        "fn <- function(x) x",
        "gn = function(x) x",
        "hn <- \\(x) x",
        "value <- 1",
        "nested <- foo(function(x) x)",
        "fn(value)"
    )
    parsed <- parse(text = content, keep.source = TRUE)
    semantic <- semantic_parse_data(
        utils::getParseData(parsed, includeText = TRUE),
        content
    )

    token_at <- function(line, name) {
        which(
            semantic$lines == line &
                semantic$cols == 0L &
                semantic$lengths == nchar(name)
        )
    }

    expect_equal(
        semantic$types[token_at(0L, "fn")],
        SemanticTokenTypes[["function"]]
    )
    expect_equal(
        semantic$modifiers[token_at(0L, "fn")],
        bitwShiftL(1L, SemanticTokenModifiers$declaration)
    )
    expect_equal(
        semantic$types[token_at(1L, "gn")],
        SemanticTokenTypes[["function"]]
    )
    expect_equal(
        semantic$types[token_at(2L, "hn")],
        SemanticTokenTypes[["function"]]
    )
    expect_equal(
        semantic$types[token_at(3L, "value")],
        SemanticTokenTypes$variable
    )
    expect_equal(
        semantic$types[token_at(4L, "nested")],
        SemanticTokenTypes$variable
    )
    expect_equal(
        semantic$types[token_at(5L, "fn")],
        SemanticTokenTypes[["function"]]
    )
})

test_that("Rightward function assignments are declarations in both semantic paths", {
    uri <- "file:///rightward-semantic.R"
    content <- c(
        "(function(x) x) -> fn",
        "((function(x) { x })) ->> gn",
        "(\\(x) x) -> hn",
        "(1) -> value",
        "(foo(function(x) x)) -> nested",
        "function(x) x -> inner",
        "(fn) -> alias",
        "left <- (function(x) x)",
        "equal = (function(x) x)"
    )
    parsed <- parse(text = content, keep.source = TRUE)
    semantic <- semantic_parse_data(
        utils::getParseData(parsed, includeText = TRUE),
        content
    )
    xdoc <- xml2::read_xml(xmlparsedata::xml_parse_data(parsed))
    document <- Document$new(uri, version = 1L, content = content)
    expected <- list(
        list(line = 0L, col = 19L, name = "fn", is_function = TRUE),
        list(line = 1L, col = 26L, name = "gn", is_function = TRUE),
        list(line = 2L, col = 12L, name = "hn", is_function = TRUE),
        list(line = 3L, col = 7L, name = "value", is_function = FALSE),
        list(line = 4L, col = 24L, name = "nested", is_function = FALSE),
        list(line = 5L, col = 17L, name = "inner", is_function = FALSE),
        list(line = 6L, col = 8L, name = "alias", is_function = FALSE),
        list(line = 7L, col = 0L, name = "left", is_function = TRUE),
        list(line = 8L, col = 0L, name = "equal", is_function = TRUE)
    )

    for (cached in c(TRUE, FALSE)) {
        workspace <- list(get_parse_data = function(request_uri) {
            expect_identical(request_uri, uri)
            list(xml_doc = xdoc, semantic_data = if (cached) semantic else NULL)
        })
        tokens <- extract_semantic_tokens(uri, workspace, document)
        for (case in expected) {
            info <- paste(if (cached) "cached" else "XML", case$name)
            matched <- Filter(function(token) {
                token$line == case$line && token$col == case$col &&
                    token$length == nchar(case$name)
            }, tokens)
            expect_equal(length(matched), 1L, info = info)
            expect_equal(
                matched[[1L]]$tokenType,
                if (case$is_function) SemanticTokenTypes[["function"]] else SemanticTokenTypes$variable,
                info = info
            )
            expect_equal(
                matched[[1L]]$tokenModifiers,
                if (case$is_function) bitwShiftL(1L, SemanticTokenModifiers$declaration) else 0L,
                info = info
            )
        }
    }
})

test_that("Semantic ranges select overlapping tokens and re-encode them", {
    fixture <- provider_fixture(c("alpha <- 1", "beta <- alpha", "gamma <- 3"))
    data <- fixture$document$parse_data$semantic_data

    selected <- semantic_data_for_range(data, list(
        start = list(line = 1L, character = 1L),
        end = list(line = 2L, character = 0L)
    ))
    expect_true(length(selected$lines) > 0L)
    expect_true(all(selected$lines == 1L))
    expect_identical(length(selected$encoded), length(selected$lines) * 5L)

    empty <- semantic_data_for_range(data, list(
        start = list(line = 20L, character = 0L),
        end = list(line = 21L, character = 0L)
    ))
    expect_identical(empty, empty_semantic_data())
    expect_identical(
        semantic_data_for_range(NULL, list()),
        empty_semantic_data()
    )
})

test_that("Semantic providers use cached data and legacy fallbacks", {
    uri <- "file:///semantic-cache.R"
    document <- Document$new(uri, version = 1L, content = "value <- 1")
    semantic_data <- list(
        lines = c(0L, 1L),
        cols = c(0L, 2L),
        lengths = c(5L, 3L),
        types = c(SemanticTokenTypes$variable, SemanticTokenTypes$number),
        modifiers = c(0L, 0L),
        encoded = as.integer(c(0, 0, 5, 8, 0, 1, 2, 3, 9, 0))
    )
    parse_data <- list(
        version = 1L,
        semantic_data = semantic_data,
        content_hash = "current"
    )
    workspace <- new.env(parent = baseenv())
    workspace$parse_cache <- collections::dict()
    workspace$get_parse_data <- function(...) parse_data

    legend <- get_semantic_tokens_legend()
    expect_identical(legend$tokenTypes, names(SemanticTokenTypes))
    expect_identical(legend$tokenModifiers, names(SemanticTokenModifiers))

    cached <- extract_semantic_tokens(uri, workspace, document)
    expect_length(cached, 2L)
    ranged <- extract_semantic_tokens(
        uri, workspace, document,
        range = range(position(0L, 0L), position(1L, 0L))
    )
    expect_length(ranged, 1L)

    parse_data$semantic_data <- empty_semantic_data()
    expect_identical(
        extract_semantic_tokens(uri, workspace, document),
        list()
    )

    parse_data <- list(
        version = 1L,
        semantic_data = NULL,
        xml_doc = NULL,
        content_hash = "current"
    )
    expect_identical(
        semantic_tokens_full_reply(1L, uri, workspace, document)$result$data,
        integer()
    )
    request_range <- range(position(0L, 0L), position(1L, 0L))
    expect_identical(
        semantic_tokens_range_reply(
            2L, uri, workspace, document, request_range
        )$result$data,
        integer()
    )
    expect_identical(
        semantic_tokens_delta_reply(
            3L, uri, workspace, document, "missing"
        )$result$data,
        integer()
    )

    parse_data$semantic_data <- semantic_data
    delta <- semantic_tokens_delta_reply(
        4L, uri, workspace, document, "missing"
    )
    expect_identical(delta$result$resultId, "current")
    expect_identical(delta$result$data, semantic_data$encoded)
})

test_that("Semantic token types cover every parser token category", {
    cases <- c(
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
        `OP-LAMBDA` = SemanticTokenTypes$keyword,
        UNKNOWN = SemanticTokenTypes$variable
    )

    actual <- vapply(names(cases), get_token_type, integer(1L))
    expect_identical(unname(actual), unname(as.integer(cases)))
})

test_that("Legacy XML semantic extraction handles ranges and declarations", {
    uri <- "file:///legacy-semantic.R"
    content <- c(
        "fn <- function(argument) {",
        "  value <- argument + 1L",
        "  value",
        "}"
    )
    parsed <- parse(text = content, keep.source = TRUE)
    xdoc <- xml2::read_xml(xmlparsedata::xml_parse_data(parsed))
    workspace <- list(get_parse_data = function(request_uri) {
        expect_identical(request_uri, uri)
        list(xml_doc = xdoc, semantic_data = NULL)
    })
    document <- Document$new(uri, version = 1L, content = content)

    tokens <- extract_semantic_tokens(uri, workspace, document)
    expect_gt(length(tokens), 0L)
    fn_declaration <- Filter(function(token) {
        token$line == 0L && token$col == 0L && token$length == nchar("fn")
    }, tokens)
    expect_length(fn_declaration, 1L)
    expect_equal(fn_declaration[[1L]]$tokenType, SemanticTokenTypes[["function"]])
    expect_true(fn_declaration[[1L]]$tokenModifiers != 0L)
    parameter <- Filter(function(token) {
        token$tokenType == SemanticTokenTypes$parameter
    }, tokens)
    expect_true(any(vapply(parameter, function(token) {
        token$tokenModifiers != 0L
    }, logical(1L))))

    ranged <- extract_semantic_tokens(
        uri, workspace, document,
        range = range(position(0L, 0L), position(1L, 0L))
    )
    expect_true(length(ranged) > 0L)
    expect_true(all(vapply(ranged, function(token) token$line <= 1L, logical(1L))))

    no_xml <- list(get_parse_data = function(...) list(xml_doc = NULL))
    expect_identical(
        extract_semantic_tokens(uri, no_xml, document),
        list()
    )
})

test_that("Semantic encoding sorts tokens and supports empty results", {
    tokens <- list(
        list(line = 2L, col = 0L, length = 1L,
            tokenType = SemanticTokenTypes$number, tokenModifiers = 0L),
        list(line = 0L, col = 4L, length = 3L,
            tokenType = SemanticTokenTypes$variable, tokenModifiers = 0L),
        list(line = 0L, col = 0L, length = 2L,
            tokenType = SemanticTokenTypes$parameter, tokenModifiers = 1L)
    )

    encoded <- encode_semantic_tokens(tokens)$data
    matrix_data <- matrix(encoded, ncol = 5L, byrow = TRUE)
    expect_identical(matrix_data[, 1L], c(0L, 0L, 2L))
    expect_identical(matrix_data[, 2L], c(0L, 4L, 0L))
    expect_identical(encode_semantic_tokens(list())$data, integer())
})

test_that("Semantic deltas handle equality, insertion, and deletion", {
    token_a <- as.integer(c(0, 0, 1, 8, 0))
    token_b <- as.integer(c(1, 0, 1, 8, 0))
    token_c <- as.integer(c(1, 2, 1, 8, 0))

    expect_identical(semantic_token_delta(token_a, token_a), list())

    inserted <- semantic_token_delta(
        c(token_a, token_c),
        c(token_a, token_b, token_c)
    )[[1L]]
    expect_equal(inserted$start, 5L)
    expect_equal(inserted$deleteCount, 0L)
    expect_identical(inserted$data, token_b)

    deleted <- semantic_token_delta(
        c(token_a, token_b, token_c),
        c(token_a, token_c)
    )[[1L]]
    expect_equal(deleted$start, 5L)
    expect_equal(deleted$deleteCount, 5L)
    expect_null(deleted$data)
})

test_that("Function assignments with comments around operators emit declaration tokens", {
    uri <- "file:///commented-fn.R"
    content <- c(
        "first <-",
        "  # comment after left assign",
        "  function(x) x",
        "second =",
        "  # comment after equal assign",
        "  function(y) y",
        "(function(z) z) ->",
        "  # comment after right assign",
        "  third",
        "(",
        "  fourth # comment before left assign",
        "    <- function(w) w",
        ")",
        "(",
        "  (function(v) v) # comment before right assign",
        "    -> fifth",
        ")"
    )
    parse_data <- parse_document(uri, content)
    expect_false(parse_data$parse_error)
    document <- Document$new(uri, version = 1L, content = content)
    workspace <- list(get_parse_data = function(...) parse_data)

    tokens <- extract_semantic_tokens(uri, workspace, document)
    fn_declarations <- Filter(function(token) {
        token$tokenType == SemanticTokenTypes[["function"]] &&
            token$tokenModifiers != 0L
    }, tokens)
    expect_identical(
        vapply(fn_declarations, `[[`, integer(1L), "line"),
        c(0L, 3L, 8L, 10L, 15L)
    )
})

test_that("Control-flow keywords and return() calls emit keyword semantic tokens", {
    uri <- "file:///control-flow-semantic.R"
    content <- c(
        "fn <- function(x) {",
        "  if (x > 0) {",
        "    for (i in seq_len(x)) {",
        "      while (i < 10) {",
        "        repeat {",
        "          if (i == 5) next else break",
        "        }",
        "      }",
        "    }",
        "    return(x)",
        "  }",
        "}"
    )
    parsed <- parse(text = content, keep.source = TRUE)
    semantic <- semantic_parse_data(
        utils::getParseData(parsed, includeText = TRUE),
        content
    )
    xdoc <- xml2::read_xml(xmlparsedata::xml_parse_data(parsed))
    document <- Document$new(uri, version = 1L, content = content)

    expected_keywords <- list(
        list(line = 1L, col = 2L, word = "if"),
        list(line = 2L, col = 4L, word = "for"),
        list(line = 2L, col = 11L, word = "in"),
        list(line = 3L, col = 6L, word = "while"),
        list(line = 4L, col = 8L, word = "repeat"),
        list(line = 5L, col = 10L, word = "if"),
        list(line = 5L, col = 22L, word = "next"),
        list(line = 5L, col = 27L, word = "else"),
        list(line = 5L, col = 32L, word = "break"),
        list(line = 9L, col = 4L, word = "return")
    )

    for (cached in c(TRUE, FALSE)) {
        workspace <- list(get_parse_data = function(request_uri) {
            expect_identical(request_uri, uri)
            list(
                version = 1L,
                xml_doc = xdoc,
                semantic_data = if (cached) semantic else NULL
            )
        })
        tokens <- extract_semantic_tokens(uri, workspace, document)
        for (kw in expected_keywords) {
            matched <- Filter(function(token) {
                token$line == kw$line &&
                    token$col == kw$col &&
                    token$length == nchar(kw$word)
            }, tokens)
            expect_length(matched, 1L)
            expect_equal(matched[[1L]]$tokenType, SemanticTokenTypes$keyword)
            expect_equal(matched[[1L]]$tokenModifiers, 0L)
        }
        range_reply <- semantic_tokens_range_reply(
            1L,
            uri,
            workspace,
            document,
            list(
                start = list(line = 5L, character = 0L),
                end = list(line = 5L, character = 40L)
            )
        )
        expect_true(length(range_reply$result$data) > 0L)
    }

    edge_uri <- "file:///control-flow-edge-cases.R"
    edge_content <- c(
        "obj <- list(return = function(x) x + 1L)",
        "fn <- \\(x, return = 1L) {",
        "  obj$return(1L)",
        "  pkg::return(x)",
        "  pkg:::return(x)",
        "  `return`(x)",
        "  return(x)",
        "}"
    )
    edge_parsed <- parse(text = edge_content, keep.source = TRUE)
    edge_semantic <- semantic_parse_data(
        utils::getParseData(edge_parsed, includeText = TRUE),
        edge_content
    )
    edge_xdoc <- xml2::read_xml(xmlparsedata::xml_parse_data(edge_parsed))
    edge_doc <- Document$new(edge_uri, version = 1L, content = edge_content)
    for (cached in c(TRUE, FALSE)) {
        edge_ws <- list(get_parse_data = function(...) {
            list(
                version = 1L,
                xml_doc = edge_xdoc,
                semantic_data = if (cached) edge_semantic else NULL
            )
        })
        edge_tokens <- extract_semantic_tokens(edge_uri, edge_ws, edge_doc)
        token_type_at <- function(line, col, len) {
            matched <- Filter(function(t) {
                t$line == line && t$col == col && t$length == len
            }, edge_tokens)
            expect_length(matched, 1L)
            matched[[1L]]$tokenType
        }
        expect_equal(token_type_at(0L, 21L, 8L), SemanticTokenTypes$keyword)
        expect_equal(token_type_at(1L, 6L, 1L), SemanticTokenTypes$keyword)
        expect_equal(token_type_at(2L, 6L, 6L), SemanticTokenTypes[["function"]])
        expect_equal(token_type_at(3L, 7L, 6L), SemanticTokenTypes[["function"]])
        expect_equal(token_type_at(4L, 8L, 6L), SemanticTokenTypes[["function"]])
        expect_equal(token_type_at(5L, 2L, 8L), SemanticTokenTypes[["function"]])
        expect_equal(token_type_at(6L, 2L, 6L), SemanticTokenTypes$keyword)
    }
})

test_that("Operators emit expected semantic tokens", {
    uri <- "file:///operators-semantic.R"
    content <- c(
        "summary_fn <- function(x_val = 1L, y_val = 2L) {",
        "  pkg::pub + pkg:::priv - obj@slot * obj$field / x_val ^ y_val ** 2L",
        "  cmp = (x_val > 0L) & (x_val >= 1L) | (y_val < 10L) && (y_val <= 9L) || (x_val == y_val) != (!FALSE)",
        "  dt[, col := x_val %in% 1:10 |> identity(a = x_val)] -> out ->> out_global",
        "  out <<- ?mean",
        paste0("  x_val %", intToUtf8(0x1f389), "% y_val"),
        "  y_val ~ x_val",
        "}"
    )
    parsed <- parse(text = content, keep.source = TRUE, encoding = "UTF-8")
    pd <- utils::getParseData(parsed, includeText = TRUE)
    semantic <- semantic_parse_data(pd, content)
    xdoc <- xml2::read_xml(xmlparsedata::xml_parse_data(parsed))
    document <- Document$new(uri, version = 1L, content = content)

    op_tokens <- c(
        "LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN", "EQ_SUB", "EQ_FORMALS",
        "SPECIAL", "NS_GET", "NS_GET_INT",
        "GT", "GE", "LT", "LE", "EQ", "NE",
        "AND", "OR", "AND2", "OR2", "PIPE",
        "'$'", "'@'", "'~'", "'?'", "':'",
        "'+'", "'-'", "'*'", "'/'", "'^'", "'!'"
    )
    expect_true(all(op_tokens %in% pd$token[pd$terminal]))

    expected <- list(
        # Line 0: <-, =, =
        list(line = 0L, col = 11L, len = 2L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 0L, col = 29L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 0L, col = 41L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        # Line 1: ::, +, :::, -, @, *, $, /, ^, **
        list(line = 1L, col = 5L, len = 2L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 1L, col = 11L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 1L, col = 16L, len = 3L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 1L, col = 24L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 1L, col = 29L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 1L, col = 35L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 1L, col = 40L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 1L, col = 47L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 1L, col = 55L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 1L, col = 63L, len = 2L, type = SemanticTokenTypes$operator, mod = 0L),
        # Line 2: =, >, &, >=, |, <, &&, <=, ||, ==, !=, !
        list(line = 2L, col = 6L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 2L, col = 15L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 2L, col = 21L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 2L, col = 30L, len = 2L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 2L, col = 37L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 2L, col = 46L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 2L, col = 53L, len = 2L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 2L, col = 63L, len = 2L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 2L, col = 70L, len = 2L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 2L, col = 80L, len = 2L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 2L, col = 90L, len = 2L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 2L, col = 94L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        # Line 3: :=, %in%, :, |>, =, ->, ->>
        list(line = 3L, col = 11L, len = 2L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 3L, col = 20L, len = 4L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 3L, col = 26L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 3L, col = 30L, len = 2L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 3L, col = 44L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 3L, col = 54L, len = 2L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 3L, col = 61L, len = 3L, type = SemanticTokenTypes$operator, mod = 0L),
        # Line 4: <<-, ?
        list(line = 4L, col = 6L, len = 3L, type = SemanticTokenTypes$operator, mod = 0L),
        list(line = 4L, col = 10L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L),
        # Line 5: %🎉% (4 UTF-16 code units)
        list(line = 5L, col = 8L, len = 4L, type = SemanticTokenTypes$operator, mod = 0L),
        # Line 6: ~
        list(line = 6L, col = 8L, len = 1L, type = SemanticTokenTypes$operator, mod = 0L)
    )

    for (cached in c(TRUE, FALSE)) {
        workspace <- list(get_parse_data = function(request_uri) {
            expect_identical(request_uri, uri)
            list(
                version = 1L,
                xml_doc = xdoc,
                semantic_data = if (cached) semantic else NULL
            )
        })
        tokens <- extract_semantic_tokens(uri, workspace, document)
        full <- semantic_tokens_full_reply(1L, uri, workspace, document)
        expect_identical(full$result$data, semantic$encoded)
        for (item in expected) {
            matched <- Filter(function(token) {
                token$line == item$line &&
                    token$col == item$col &&
                    token$length == item$len
            }, tokens)
            expect_length(matched, 1L)
            expect_equal(matched[[1L]]$tokenType, item$type)
            expect_equal(matched[[1L]]$tokenModifiers, item$mod)
        }
    }

    withr::local_envvar(c("_R_USE_PIPEBIND_" = "true"))
    pb_uri <- "file:///pipebind-semantic.R"
    pb_content <- "x |> a => f(1, a)"
    pb_parsed <- parse(text = pb_content, keep.source = TRUE)
    pb_semantic <- semantic_parse_data(
        utils::getParseData(pb_parsed, includeText = TRUE),
        pb_content
    )
    pb_xdoc <- xml2::read_xml(xmlparsedata::xml_parse_data(pb_parsed))
    pb_doc <- Document$new(pb_uri, version = 1L, content = pb_content)
    for (cached in c(TRUE, FALSE)) {
        pb_ws <- list(get_parse_data = function(...) {
            list(
                version = 1L,
                xml_doc = pb_xdoc,
                semantic_data = if (cached) pb_semantic else NULL
            )
        })
        pb_tokens <- extract_semantic_tokens(pb_uri, pb_ws, pb_doc)
        matched <- Filter(function(token) {
            token$line == 0L && token$col == 7L && token$length == 2L
        }, pb_tokens)
        expect_length(matched, 1L)
        expect_equal(matched[[1L]]$tokenType, SemanticTokenTypes$operator)
        expect_equal(matched[[1L]]$tokenModifiers, 0L)
    }
})

test_that("Named arguments and scoped parameter references emit parameter tokens", {
    uri <- "file:///parameters-semantic.R"
    content <- c(
        "compute <- function(alpha, beta = alpha + 1L) {",
        "  local_var <- alpha + beta",
        "  inner <- \\(gamma) alpha + gamma + obj$alpha + pkg::beta",
        "  inner(gamma = local_var)",
        "}"
    )
    parsed <- parse(text = content, keep.source = TRUE)
    semantic <- semantic_parse_data(
        utils::getParseData(parsed, includeText = TRUE),
        content
    )
    xdoc <- xml2::read_xml(xmlparsedata::xml_parse_data(parsed))
    document <- Document$new(uri, version = 1L, content = content)
    decl_mod <- bitwShiftL(1L, SemanticTokenModifiers$declaration)

    expected <- list(
        # Formal parameter declarations
        list(line = 0L, col = 20L, name = "alpha", type = SemanticTokenTypes$parameter, mod = decl_mod),
        list(line = 0L, col = 27L, name = "beta", type = SemanticTokenTypes$parameter, mod = decl_mod),
        # Default expression referencing earlier formal parameter
        list(line = 0L, col = 34L, name = "alpha", type = SemanticTokenTypes$parameter, mod = 0L),
        # Body references vs local variable
        list(line = 1L, col = 2L, name = "local_var", type = SemanticTokenTypes$variable, mod = 0L),
        list(line = 1L, col = 15L, name = "alpha", type = SemanticTokenTypes$parameter, mod = 0L),
        list(line = 1L, col = 23L, name = "beta", type = SemanticTokenTypes$parameter, mod = 0L),
        # Nested lambda formal and outer/inner parameter references vs $ and :: members
        list(line = 2L, col = 13L, name = "gamma", type = SemanticTokenTypes$parameter, mod = decl_mod),
        list(line = 2L, col = 20L, name = "alpha", type = SemanticTokenTypes$parameter, mod = 0L),
        list(line = 2L, col = 28L, name = "gamma", type = SemanticTokenTypes$parameter, mod = 0L),
        list(line = 2L, col = 40L, name = "alpha", type = SemanticTokenTypes$variable, mod = 0L),
        list(line = 2L, col = 53L, name = "beta", type = SemanticTokenTypes$variable, mod = 0L),
        # Named call argument (SYMBOL_SUB)
        list(line = 3L, col = 8L, name = "gamma", type = SemanticTokenTypes$parameter, mod = 0L),
        list(line = 3L, col = 16L, name = "local_var", type = SemanticTokenTypes$variable, mod = 0L)
    )

    for (cached in c(TRUE, FALSE)) {
        workspace <- list(get_parse_data = function(request_uri) {
            expect_identical(request_uri, uri)
            list(xml_doc = xdoc, semantic_data = if (cached) semantic else NULL)
        })
        tokens <- extract_semantic_tokens(uri, workspace, document)
        for (item in expected) {
            matched <- Filter(function(token) {
                token$line == item$line &&
                    token$col == item$col &&
                    token$length == nchar(item$name)
            }, tokens)
            expect_length(matched, 1L)
            expect_equal(matched[[1L]]$tokenType, item$type)
            expect_equal(matched[[1L]]$tokenModifiers, item$mod)
        }
    }

    bt_uri <- "file:///backtick-parameters.R"
    bt_content <- c(
        "f <- function(`x` = 1, y = `x`, `...`) x + `y` + `...` + gamma",
        "outside <- function(delta) delta + alpha",
        "noop <- function(unused_arg) 1L",
        "outer <- function(alpha) { inner <- function() { alpha <- 1L; alpha }; inner() }",
        "loop_outer <- function(alpha) { inner <- function() { for (alpha in 1:3) print(alpha) }; inner() }",
        "direct_loop <- function(alpha) { for (alpha in 1:3) print(alpha); alpha }",
        "right_outer <- function(alpha) { inner <- function() { 1L -> alpha; alpha }; inner() }"
    )
    bt_parsed <- parse(text = bt_content, keep.source = TRUE)
    bt_semantic <- semantic_parse_data(
        utils::getParseData(bt_parsed, includeText = TRUE),
        bt_content
    )
    bt_xdoc <- xml2::read_xml(xmlparsedata::xml_parse_data(bt_parsed))
    bt_doc <- Document$new(bt_uri, version = 1L, content = bt_content)
    for (cached in c(TRUE, FALSE)) {
        bt_ws <- list(get_parse_data = function(...) {
            list(xml_doc = bt_xdoc, semantic_data = if (cached) bt_semantic else NULL)
        })
        bt_tokens <- extract_semantic_tokens(bt_uri, bt_ws, bt_doc)
        token_type_at <- function(line, col, len) {
            matched <- Filter(function(t) {
                t$line == line && t$col == col && t$length == len
            }, bt_tokens)
            expect_length(matched, 1L)
            matched[[1L]]$tokenType
        }
        expect_equal(token_type_at(0L, 27L, 3L), SemanticTokenTypes$parameter)
        expect_equal(token_type_at(0L, 39L, 1L), SemanticTokenTypes$parameter)
        expect_equal(token_type_at(0L, 43L, 3L), SemanticTokenTypes$parameter)
        expect_equal(token_type_at(0L, 49L, 5L), SemanticTokenTypes$variable)
        expect_equal(token_type_at(0L, 57L, 5L), SemanticTokenTypes$variable)
        expect_equal(token_type_at(1L, 27L, 5L), SemanticTokenTypes$parameter)
        expect_equal(token_type_at(1L, 35L, 5L), SemanticTokenTypes$variable)
        # Line 3: outer formal alpha vs inner local assignment alpha <- 1L; alpha
        expect_equal(token_type_at(3L, 18L, 5L), SemanticTokenTypes$parameter)
        expect_equal(token_type_at(3L, 49L, 5L), SemanticTokenTypes$variable)
        expect_equal(token_type_at(3L, 62L, 5L), SemanticTokenTypes$variable)
        # Line 4: outer formal alpha vs inner loop variable for (alpha in 1:3) print(alpha)
        expect_equal(token_type_at(4L, 23L, 5L), SemanticTokenTypes$parameter)
        expect_equal(token_type_at(4L, 59L, 5L), SemanticTokenTypes$variable)
        expect_equal(token_type_at(4L, 79L, 5L), SemanticTokenTypes$variable)
        # Line 5: direct loop variable inside direct_loop vs trailing alpha parameter
        expect_equal(token_type_at(5L, 24L, 5L), SemanticTokenTypes$parameter)
        expect_equal(token_type_at(5L, 38L, 5L), SemanticTokenTypes$variable)
        expect_equal(token_type_at(5L, 58L, 5L), SemanticTokenTypes$variable)
        expect_equal(token_type_at(5L, 66L, 5L), SemanticTokenTypes$parameter)
        # Line 6: rightward assignment shadowing
        expect_equal(token_type_at(6L, 61L, 5L), SemanticTokenTypes$variable)
        expect_equal(token_type_at(6L, 68L, 5L), SemanticTokenTypes$variable)
    }

    unused_parsed <- parse(text = "noop <- function(unused_arg) 1L", keep.source = TRUE)
    unused_pd <- utils::getParseData(unused_parsed, includeText = TRUE)
    expect_identical(formal_parameter_symbol_ids(unused_pd), integer())
})

test_that("Builtin constants, ellipsis, and pipe placeholders emit readonly defaultLibrary tokens", {
    uri <- "file:///constants-semantic.R"
    content <- c(
        "flags <- list(NULL, TRUE, FALSE, T, F)",
        "nums <- c(Inf, NaN, NA, NA_integer_, NA_real_, NA_complex_, NA_character_)",
        "forward <- function(...) list(..., ..1, ..2)",
        "shadow <- function(T, F) list(T, F, obj$T, pkg::F, obj$...)",
        "local_shadow <- function() { T <- 10L; F <- 20L; c(T, F) }",
        "loop_shadow <- function() { for (T in 1:3) print(T); F }"
    )
    parsed <- parse(text = content, keep.source = TRUE)
    semantic <- semantic_parse_data(
        utils::getParseData(parsed, includeText = TRUE),
        content
    )
    xdoc <- xml2::read_xml(xmlparsedata::xml_parse_data(parsed))
    document <- Document$new(uri, version = 1L, content = content)

    ro_default_mod <- bitwOr(
        bitwShiftL(1L, SemanticTokenModifiers$readonly),
        bitwShiftL(1L, SemanticTokenModifiers$defaultLibrary)
    )
    decl_mod <- bitwShiftL(1L, SemanticTokenModifiers$declaration)

    expected <- list(
        list(line = 0L, col = 14L, text = "NULL", type = SemanticTokenTypes$keyword, mod = ro_default_mod),
        list(line = 0L, col = 20L, text = "TRUE", type = SemanticTokenTypes$keyword, mod = ro_default_mod),
        list(line = 0L, col = 26L, text = "FALSE", type = SemanticTokenTypes$keyword, mod = ro_default_mod),
        list(line = 0L, col = 33L, text = "T", type = SemanticTokenTypes$keyword, mod = ro_default_mod),
        list(line = 0L, col = 36L, text = "F", type = SemanticTokenTypes$keyword, mod = ro_default_mod),
        list(line = 1L, col = 10L, text = "Inf", type = SemanticTokenTypes$number, mod = ro_default_mod),
        list(line = 1L, col = 15L, text = "NaN", type = SemanticTokenTypes$number, mod = ro_default_mod),
        list(line = 1L, col = 20L, text = "NA", type = SemanticTokenTypes$number, mod = ro_default_mod),
        list(line = 1L, col = 24L, text = "NA_integer_", type = SemanticTokenTypes$number, mod = ro_default_mod),
        list(line = 1L, col = 37L, text = "NA_real_", type = SemanticTokenTypes$number, mod = ro_default_mod),
        list(line = 1L, col = 47L, text = "NA_complex_", type = SemanticTokenTypes$number, mod = ro_default_mod),
        list(line = 1L, col = 60L, text = "NA_character_", type = SemanticTokenTypes$number, mod = ro_default_mod),
        list(line = 2L, col = 20L, text = "...", type = SemanticTokenTypes$parameter, mod = decl_mod),
        list(line = 2L, col = 30L, text = "...", type = SemanticTokenTypes$variable, mod = ro_default_mod),
        list(line = 2L, col = 35L, text = "..1", type = SemanticTokenTypes$variable, mod = ro_default_mod),
        list(line = 2L, col = 40L, text = "..2", type = SemanticTokenTypes$variable, mod = ro_default_mod),
        list(line = 3L, col = 19L, text = "T", type = SemanticTokenTypes$parameter, mod = decl_mod),
        list(line = 3L, col = 22L, text = "F", type = SemanticTokenTypes$parameter, mod = decl_mod),
        list(line = 3L, col = 30L, text = "T", type = SemanticTokenTypes$parameter, mod = 0L),
        list(line = 3L, col = 33L, text = "F", type = SemanticTokenTypes$parameter, mod = 0L),
        list(line = 3L, col = 40L, text = "T", type = SemanticTokenTypes$variable, mod = 0L),
        list(line = 3L, col = 48L, text = "F", type = SemanticTokenTypes$variable, mod = 0L),
        list(line = 3L, col = 55L, text = "...", type = SemanticTokenTypes$variable, mod = 0L),
        # Line 4: local assignment shadowing of T and F
        list(line = 4L, col = 29L, text = "T", type = SemanticTokenTypes$variable, mod = 0L),
        list(line = 4L, col = 39L, text = "F", type = SemanticTokenTypes$variable, mod = 0L),
        list(line = 4L, col = 51L, text = "T", type = SemanticTokenTypes$variable, mod = 0L),
        list(line = 4L, col = 54L, text = "F", type = SemanticTokenTypes$variable, mod = 0L),
        # Line 5: loop-variable shadowing of T while F remains built-in
        list(line = 5L, col = 33L, text = "T", type = SemanticTokenTypes$variable, mod = 0L),
        list(line = 5L, col = 49L, text = "T", type = SemanticTokenTypes$variable, mod = 0L),
        list(line = 5L, col = 53L, text = "F", type = SemanticTokenTypes$keyword, mod = ro_default_mod)
    )

    for (cached in c(TRUE, FALSE)) {
        workspace <- list(get_parse_data = function(request_uri) {
            expect_identical(request_uri, uri)
            list(xml_doc = xdoc, semantic_data = if (cached) semantic else NULL)
        })
        tokens <- extract_semantic_tokens(uri, workspace, document)
        for (item in expected) {
            matched <- Filter(function(token) {
                token$line == item$line &&
                    token$col == item$col &&
                    token$length == nchar(item$text)
            }, tokens)
            expect_length(matched, 1L)
            expect_equal(matched[[1L]]$tokenType, item$type)
            expect_equal(matched[[1L]]$tokenModifiers, item$mod)
        }
    }

    top_uri <- "file:///top-shadow-constants.R"
    top_content <- c(
        "for (T in 1:3) print(c(T, F))",
        "T <- 10L"
    )
    top_parsed <- parse(text = top_content, keep.source = TRUE)
    top_semantic <- semantic_parse_data(
        utils::getParseData(top_parsed, includeText = TRUE),
        top_content
    )
    top_xdoc <- xml2::read_xml(xmlparsedata::xml_parse_data(top_parsed))
    top_doc <- Document$new(top_uri, version = 1L, content = top_content)
    for (cached in c(TRUE, FALSE)) {
        top_ws <- list(get_parse_data = function(...) {
            list(xml_doc = top_xdoc, semantic_data = if (cached) top_semantic else NULL)
        })
        top_tokens <- extract_semantic_tokens(top_uri, top_ws, top_doc)
        token_at_col <- function(col) {
            matched <- Filter(function(t) t$line == 0L && t$col == col && t$length == 1L, top_tokens)
            expect_length(matched, 1L)
            matched[[1L]]
        }
        expect_equal(token_at_col(5L)$tokenType, SemanticTokenTypes$variable)
        expect_equal(token_at_col(5L)$tokenModifiers, 0L)
        expect_equal(token_at_col(23L)$tokenType, SemanticTokenTypes$variable)
        expect_equal(token_at_col(23L)$tokenModifiers, 0L)
        expect_equal(token_at_col(26L)$tokenType, SemanticTokenTypes$keyword)
        expect_equal(token_at_col(26L)$tokenModifiers, ro_default_mod)
    }

    if (getRversion() >= "4.2.0") {
        ph_uri <- "file:///placeholder-semantic.R"
        ph_content <- "x |> subset(a = _)"
        ph_parsed <- parse(text = ph_content, keep.source = TRUE)
        ph_semantic <- semantic_parse_data(
            utils::getParseData(ph_parsed, includeText = TRUE),
            ph_content
        )
        ph_xdoc <- xml2::read_xml(xmlparsedata::xml_parse_data(ph_parsed))
        ph_doc <- Document$new(ph_uri, version = 1L, content = ph_content)
        for (cached in c(TRUE, FALSE)) {
            ph_ws <- list(get_parse_data = function(...) {
                list(xml_doc = ph_xdoc, semantic_data = if (cached) ph_semantic else NULL)
            })
            ph_tokens <- extract_semantic_tokens(ph_uri, ph_ws, ph_doc)
            matched <- Filter(function(t) {
                t$line == 0L && t$col == 16L && t$length == 1L
            }, ph_tokens)
            expect_length(matched, 1L)
            expect_equal(matched[[1L]]$tokenType, SemanticTokenTypes$variable)
            expect_equal(matched[[1L]]$tokenModifiers, ro_default_mod)
        }
    }
})

test_that("Roxygen2 comments and tags emit documentation semantic tokens", {
    uri <- "file:///roxygen-semantic.R"
    content <- c(
        "#' Compute summary statistics",
        "#' @param x_val Input numeric vector.",
        "#' @importFrom stats median",
        "#' @export",
        "summary_fn <- function(x_val) x_val"
    )
    parsed <- parse(text = content, keep.source = TRUE)
    semantic <- semantic_parse_data(
        utils::getParseData(parsed, includeText = TRUE),
        content
    )
    xdoc <- xml2::read_xml(xmlparsedata::xml_parse_data(parsed))
    document <- Document$new(uri, version = 1L, content = content)
    doc_mod <- bitwShiftL(1L, SemanticTokenModifiers$documentation)

    expected <- list(
        # Roxygen2 plain doc line
        list(line = 0L, col = 0L, len = nchar(content[[1L]]), type = SemanticTokenTypes$comment, mod = doc_mod),
        # Roxygen2 @param x_val line
        list(line = 1L, col = 0L, len = 3L, type = SemanticTokenTypes$comment, mod = doc_mod),
        list(line = 1L, col = 3L, len = 6L, type = SemanticTokenTypes$macro, mod = doc_mod),
        list(line = 1L, col = 10L, len = 5L, type = SemanticTokenTypes$parameter, mod = doc_mod),
        list(line = 1L, col = 15L, len = 22L, type = SemanticTokenTypes$comment, mod = doc_mod),
        # Roxygen2 @importFrom line
        list(line = 2L, col = 0L, len = 3L, type = SemanticTokenTypes$comment, mod = doc_mod),
        list(line = 2L, col = 3L, len = 11L, type = SemanticTokenTypes$macro, mod = doc_mod),
        list(line = 2L, col = 14L, len = 13L, type = SemanticTokenTypes$comment, mod = doc_mod),
        # Roxygen2 @export line
        list(line = 3L, col = 0L, len = 3L, type = SemanticTokenTypes$comment, mod = doc_mod),
        list(line = 3L, col = 3L, len = 7L, type = SemanticTokenTypes$macro, mod = doc_mod)
    )

    for (cached in c(TRUE, FALSE)) {
        workspace <- list(get_parse_data = function(request_uri) {
            expect_identical(request_uri, uri)
            list(xml_doc = xdoc, semantic_data = if (cached) semantic else NULL)
        })
        tokens <- extract_semantic_tokens(uri, workspace, document)
        for (item in expected) {
            matched <- Filter(function(token) {
                token$line == item$line &&
                    token$col == item$col &&
                    token$length == item$len
            }, tokens)
            expect_length(matched, 1L)
            expect_equal(matched[[1L]]$tokenType, item$type)
            expect_equal(matched[[1L]]$tokenModifiers, item$mod)
        }
    }

    edge_uri <- "file:///roxygen-edge.R"
    edge_content <- c(
        "#' @param",
        "#' @param x,y Paired numeric vectors.",
        "#' @param ... Additional arguments.",
        "#' @param `x_backtick` Backticked param.",
        paste0("#' @param ", intToUtf8(0x03b1), " A ", intToUtf8(0x1f389), " emoji description."),
        paste0("#' @param caf", intToUtf8(0x00e9), " Accented Latin param."),
        paste0("#' @param ", intToUtf8(c(0x53c2, 0x6570)), " CJK param."),
        "#' @param `x y` Backticked param with space.",
        "# Regular non-roxygen comment",
        "f <- function(x, y, ..., x_backtick) NULL"
    )
    edge_parsed <- parse(text = edge_content, keep.source = TRUE, encoding = "UTF-8")
    edge_semantic <- semantic_parse_data(
        utils::getParseData(edge_parsed, includeText = TRUE),
        edge_content
    )
    edge_xdoc <- xml2::read_xml(xmlparsedata::xml_parse_data(edge_parsed))
    edge_doc <- Document$new(edge_uri, version = 1L, content = edge_content)

    expect_true(all(edge_semantic$cols >= 0L))
    expect_true(all(edge_semantic$lengths > 0L))
    expect_true(all(edge_semantic$encoded >= 0L))

    ws_cached <- list(get_parse_data = function(...) {
        list(xml_doc = edge_xdoc, semantic_data = edge_semantic)
    })
    ws_xml <- list(get_parse_data = function(...) {
        list(xml_doc = edge_xdoc, semantic_data = NULL)
    })
    tok_cached <- extract_semantic_tokens(edge_uri, ws_cached, edge_doc)
    tok_xml <- extract_semantic_tokens(edge_uri, ws_xml, edge_doc)
    expect_identical(tok_cached, tok_xml)

    edge_expected <- list(
        # Line 0: bare #' @param (no parameter token)
        list(line = 0L, col = 0L, len = 3L, type = SemanticTokenTypes$comment, mod = doc_mod),
        list(line = 0L, col = 3L, len = 6L, type = SemanticTokenTypes$macro, mod = doc_mod),
        # Line 1: @param x,y
        list(line = 1L, col = 10L, len = 3L, type = SemanticTokenTypes$parameter, mod = doc_mod),
        # Line 2: @param ...
        list(line = 2L, col = 10L, len = 3L, type = SemanticTokenTypes$parameter, mod = doc_mod),
        # Line 3: @param `x_backtick`
        list(line = 3L, col = 10L, len = 12L, type = SemanticTokenTypes$parameter, mod = doc_mod),
        # Line 4: @param α A 🎉 emoji description. (🎉 is 2 UTF-16 units -> trailing comment len = 24)
        list(line = 4L, col = 10L, len = 1L, type = SemanticTokenTypes$parameter, mod = doc_mod),
        list(line = 4L, col = 11L, len = 24L, type = SemanticTokenTypes$comment, mod = doc_mod),
        # Line 5: @param café Accented Latin param.
        list(line = 5L, col = 10L, len = 4L, type = SemanticTokenTypes$parameter, mod = doc_mod),
        list(line = 5L, col = 14L, len = 22L, type = SemanticTokenTypes$comment, mod = doc_mod),
        # Line 6: @param 参数 CJK param.
        list(line = 6L, col = 10L, len = 2L, type = SemanticTokenTypes$parameter, mod = doc_mod),
        list(line = 6L, col = 12L, len = 11L, type = SemanticTokenTypes$comment, mod = doc_mod),
        # Line 7: @param `x y` Backticked param with space.
        list(line = 7L, col = 10L, len = 5L, type = SemanticTokenTypes$parameter, mod = doc_mod),
        list(line = 7L, col = 15L, len = 29L, type = SemanticTokenTypes$comment, mod = doc_mod),
        # Line 8: regular comment has mod = 0L
        list(line = 8L, col = 0L, len = 29L, type = SemanticTokenTypes$comment, mod = 0L)
    )
    for (item in edge_expected) {
        matched <- Filter(function(token) {
            token$line == item$line &&
                token$col == item$col &&
                token$length == item$len
        }, tok_cached)
        expect_length(matched, 1L)
        expect_equal(matched[[1L]]$tokenType, item$type)
        expect_equal(matched[[1L]]$tokenModifiers, item$mod)
    }

    r6_uri <- "file:///roxygen-r6-provider.R"
    r6_lines <- c(
        "Widget <- R6::R6Class(\"Widget\", public = list(",
        "  run = function() {",
        "    #' @param self The receiver.",
        "    self",
        "  }",
        "))"
    )
    r6_doc <- Document$new(r6_uri, language = "r", version = 1L, content = r6_lines)
    r6_data <- parse_document(r6_uri, r6_lines, is_rmarkdown = FALSE)
    r6_data$version <- 1L
    r6_doc$update_parse_data(r6_data)
    r6_docs <- collections::dict()
    r6_docs$set(r6_uri, r6_doc)
    r6_ws <- list(
        documents = r6_docs,
        member_metadata = collections::dict(),
        get_parse_data = function(...) r6_data,
        get_documentation = function(...) NULL,
        get_signature = function(...) NULL,
        get_help = function(...) NULL
    )
    # Inside roxygen comment line (row 2): hover and scope completion on `self` are excluded
    expect_false(semantic_in_comment_or_string(NULL, 0L, 0L))
    expect_false(semantic_in_comment_or_string(r6_data$semantic_data, 99L, 0L))
    expect_true(semantic_in_comment_or_string(r6_data$semantic_data, 0L, 24L))
    expect_false(semantic_in_comment_or_string(r6_data$semantic_data, 2L, 4L, strict_comment_start = TRUE))
    expect_true(semantic_in_comment_or_string(r6_data$semantic_data, 2L, 4L, strict_comment_start = FALSE))
    expect_true(semantic_in_comment_or_string(r6_data$semantic_data, 2L, 15L, strict_comment_start = TRUE))
    expect_false(semantic_in_comment_or_string(r6_data$semantic_data, 3L, 5L))
    expect_null(member_symbol_location(r6_doc, list(row = 2L, col = 15L)))
    expect_identical(
        context_scope_completion(r6_uri, r6_ws, "se", list(row = 2L, col = 16L), TRUE, 50L, character()),
        list()
    )
    # In code body line (row 3): `self` resolves in R6 method context
    expect_false(is.null(member_symbol_location(r6_doc, list(row = 3L, col = 5L))))
    code_items <- context_scope_completion(
        r6_uri, r6_ws, "se", list(row = 3L, col = 6L), TRUE, 50L, character()
    )
    expect_true("self" %in% vapply(code_items, `[[`, character(1L), "label"))
})
