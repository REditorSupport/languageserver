reference_response_json <- function(payload) {
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", force = TRUE)
}

native_response_json <- function(payload, max_bytes = .Machine$integer.max) {
    .Call("response_json_c", payload, max_bytes, PACKAGE = "languageserver")
}

test_that("Native response JSON matches scalar, array and object conventions", {
    values <- list(
        NULL, list(), structure(list(), names = character()),
        TRUE, FALSE, NA, c(TRUE, FALSE, NA), logical(),
        0L, c(-2147483647L, 0L, 2147483647L), integer(),
        0, -0, c(-2147483648, -2147483647, 0, 2147483647), numeric(),
        "", "value", NA_character_, c("first", NA_character_), character(),
        I(1L), I(FALSE), I("value"), I(character()),
        I(list(1L)), I(list(named = 1L)),
        jsonlite::unbox(1L), jsonlite::unbox(2), jsonlite::unbox(TRUE), jsonlite::unbox("value"),
        list(NULL), list(value = NULL), list(value = list()),
        c(first = 1L, second = 2L), c(first = "value"),
        list(items = list(list(label = "first", kind = 3L)), isIncomplete = FALSE)
    )
    for (value in values) {
        expect_identical(native_response_json(value), reference_response_json(value))
        expect_identical(response_to_json(value), reference_response_json(value))
    }
})

test_that("Native response JSON preserves UTF-8, control characters and names", {
    latin1 <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9)))
    Encoding(latin1) <- "latin1"
    strings <- c(
        intToUtf8(seq_len(31L)), 'quote" slash/ backslash\\',
        "café α 😀", intToUtf8(c(127L, 128L, 8232L, 8233L)), latin1
    )
    payload <- setNames(as.list(strings), strings)
    expected <- reference_response_json(payload)
    expect_identical(native_response_json(payload), expected)
    expect_identical(charToRaw(native_response_json(payload)), charToRaw(expected))
    expect_identical(jsonlite::fromJSON(native_response_json(payload)),
        jsonlite::fromJSON(expected))
})

test_that("Response and error message wire formats retain jsonlite behavior", {
    responses <- list(
        Response$new(1L),
        Response$new("request α", list(items = list(list(label = "😀")))),
        Response$new(NULL, list(data = I(c(0L, 1L, 3L, 2L, 0L)))),
        Response$new(2L, I(3L)),
        ResponseErrorMessage$new(4L, "InvalidParams", "Invalid \"argument\""),
        Response$new(5L, error = list(code = -32603L, message = "error", data = NULL))
    )
    for (response in responses) {
        if (!is.null(response$error)) {
            payload <- list(jsonrpc = response$jsonrpc, id = response$id, error = response$error)
        } else {
            payload <- list(jsonrpc = response$jsonrpc, id = response$id, result = response$result)
        }
        expected <- reference_response_json(payload)
        expect_identical(response$to_json(), expected)
        expect_identical(response$format(), paste0("Content-Length: ",
                nchar(expected, type = "bytes"), "\r\n\r\n", expected))
        parsed <- jsonlite::fromJSON(response$to_json(), simplifyVector = FALSE)
        if (!is.null(response$error)) {
            expect_true("error" %in% names(parsed))
            expect_false("result" %in% names(parsed))
        } else {
            expect_true("result" %in% names(parsed))
            expect_false("error" %in% names(parsed))
        }
    }
})

test_that("Package protocol wrappers use native JSON without changing their representation", {
    point <- position(1L, 2L)
    span <- range(point, position(3L, 4L))
    uri <- document_uri("file:///α.R")
    place <- location(uri, span)
    values <- list(
        point, span, uri, place, text_edit(span, "😀"),
        symbol_information("symbol", 13L, place),
        document_symbol("symbol", 13L, span, span),
        text_document_position_params(uri, point),
        completion_params(uri, point), reference_params(uri, point),
        document_symbol_params(uri), code_action_params(uri, span),
        code_lens_params(uri), document_link_params(uri),
        document_formatting_params(uri, list(tabSize = 4L)),
        document_range_formatting_params(uri, span, list(tabSize = 4L)),
        document_on_type_formatting_params(uri, point, ";", list(tabSize = 4L)),
        rename_params(uri, point, "replacement"),
        did_open_text_document_params(uri), did_change_text_document_params(uri, list()),
        will_save_text_document_params(uri, 1L), did_save_text_document_params(uri, "x"),
        did_close_text_document_params(uri), did_change_configuration_params(list(x = TRUE))
    )
    for (value in values) {
        expect_identical(native_response_json(value), reference_response_json(value))
    }
    expect_null(native_response_json(structure(unclass(point), class = c("position", "custom"))))
    expect_null(native_response_json(structure(unclass(point), class = "unknown_protocol_class")))
})

test_that("Actual symbol, semantic and folding replies take the native JSON path", {
    content <- c("first <- 1", "second <- 2", "fun <- function(x) {", "  x + first", "}")
    uri <- "file:///response-json-provider.R"
    document <- Document$new(uri, language = "r", version = 1L, content = content)
    workspace <- Workspace$new(NULL)
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, content)
    parsed$version <- 1L
    workspace$update_parse_data(uri, parsed)
    replies <- list(
        document_symbol_reply(1L, uri, workspace, document,
            list(hierarchicalDocumentSymbolSupport = FALSE)),
        document_symbol_reply(2L, uri, workspace, document,
            list(hierarchicalDocumentSymbolSupport = TRUE)),
        semantic_tokens_full_reply(3L, uri, workspace, document),
        document_folding_range_reply(4L, uri, workspace, document)
    )
    for (reply in replies) {
        expect_true(length(reply$result) > 0L)
        payload <- list(jsonrpc = reply$jsonrpc, id = reply$id, result = reply$result)
        expect_identical(native_response_json(payload), reference_response_json(payload))
        expect_identical(reply$to_json(), reference_response_json(payload))
    }
})

test_that("Unsupported response values use the original jsonlite encoder", {
    class_environment <- new.env(parent = globalenv())
    numeric_class <- methods::setClass("ResponseJsonNumeric",
        contains = "numeric", where = class_environment)
    withr::defer(methods::removeClass("ResponseJsonNumeric", where = class_environment))
    bytes <- rawToChar(as.raw(0xe9))
    Encoding(bytes) <- "bytes"
    values <- list(
        NA_integer_, NA_real_, NaN, Inf, -Inf,
        1.234567, 2147483648, 1e20,
        as.Date("2026-09-11"), as.POSIXct("2026-09-11", tz = "UTC"),
        factor(c("first", "second")), matrix(c(1L, 2L), nrow = 1L),
        data.frame(value = c(1L, 2L)), 1 + 2i, as.raw(c(1L, 255L)),
        structure(1L, custom = TRUE), structure(1L, class = "custom"),
        structure(1L, class = c("AsIs", "custom")),
        list(first = 1L, 2L), setNames(list(1L, 2L), c("", "")),
        setNames(list(1L, 2L), c("duplicate", "duplicate")),
        setNames(list(1L), NA_character_),
        new.env(parent = emptyenv()), function(x) x, pairlist(x = 1L),
        numeric_class(1), bytes
    )
    capture <- function(fun, value) {
        suppressWarnings(tryCatch(fun(value), error = function(error) conditionMessage(error)))
    }
    for (value in values) {
        expect_null(native_response_json(value))
        expect_identical(capture(response_to_json, value),
            capture(reference_response_json, value))
    }
    # Unsupported nested leaves must reject the entire speculative encoding.
    payload <- list(items = rep(list(list(label = "valid", kind = 3L)), 200L), value = 1.25)
    expect_null(native_response_json(payload))
    expect_identical(response_to_json(payload), reference_response_json(payload))
})

test_that("Larger JSON objects keep unique keys and defer repaired keys", {
    values <- setNames(as.list(seq_len(32L)), sprintf("field_%02d", seq_len(32L)))
    expect_identical(native_response_json(values), reference_response_json(values))
    names(values)[[32L]] <- names(values)[[1L]]
    expect_null(native_response_json(values))
    expect_identical(response_to_json(values), reference_response_json(values))
})

test_that("Native JSON respects measured byte limits and recursion limits", {
    values <- list(NULL, list(), structure(list(), names = character()),
        list(`α"` = c("😀", "\n\t")), list(1L, list(value = I(FALSE))))
    for (value in values) {
        expected <- reference_response_json(value)
        size <- nchar(expected, type = "bytes")
        expect_identical(native_response_json(value, size), expected)
        expect_null(native_response_json(value, size - 1L))
    }
    expect_null(native_response_json(strrep("x", 1000000L), 1024L))
    expect_error(native_response_json(NULL, NA_integer_), "non-negative integer")
    expect_error(native_response_json(NULL, -1L), "non-negative integer")
    deep <- 1L
    for (i in seq_len(130L)) deep <- list(deep)
    expect_null(native_response_json(deep))
    expect_identical(response_to_json(deep), reference_response_json(deep))
    # Compact sequences may use ALTREP accessors on recent R versions.
    expect_identical(response_to_json(list(sequence = seq_len(1000L))),
        reference_response_json(list(sequence = seq_len(1000L))))
})
