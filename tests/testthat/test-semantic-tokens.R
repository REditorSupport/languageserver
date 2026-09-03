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
        `OP-DOLLAR` = SemanticTokenTypes$operator,
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
