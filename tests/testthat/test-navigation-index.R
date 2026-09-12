test_that("indexed token lookup preserves every cursor boundary", {
    content <- c(
        "# preceding comment",
        "value <- function(argument = 1) {",
        "  call(\"text\", argument); value$member",
        "  \"a multiline",
        "string\"",
        "}",
        paste0("\"", intToUtf8(0x1f600), "\"; `", intToUtf8(0x03b1), "` <- 1")
    )
    parsed <- parse(text = content, keep.source = TRUE)
    xml <- xmlparsedata::xml_parse_data(utils::getParseData(parsed))
    fallback <- xml2::read_xml(xml)
    indexed <- xml2::read_xml(xml)
    attr(indexed, "top_level_index") <- xdoc_top_level_index(indexed)
    expect_true(!is.null(attr(indexed, "top_level_index")$tokens))

    describe <- function(node) {
        c(xml2::xml_name(node), xml2::xml_attr(node, "id"))
    }
    for (line in seq_along(content)) {
        for (col in seq_len(nchar(content[[line]]) + 2L)) {
            expect_identical(
                describe(xdoc_find_token(indexed, line, col)),
                describe(xdoc_find_token(fallback, line, col)),
                info = paste("cursor", line, col)
            )
        }
    }
    expect_s3_class(xdoc_find_token(indexed, 100L, 1L), "xml_missing")
})

test_that("empty and unordered XML retain token lookup behavior", {
    empty <- xml2::read_xml("<exprlist/>")
    attr(empty, "top_level_index") <- xdoc_top_level_index(empty)
    expect_s3_class(xdoc_find_token(empty, 1L, 1L), "xml_missing")

    xml <- paste0(
        '<exprlist><SYMBOL line1="2" col1="1" line2="2" col2="1">b</SYMBOL>',
        '<SYMBOL line1="1" col1="1" line2="1" col2="1">a</SYMBOL></exprlist>'
    )
    unordered <- xml2::read_xml(xml)
    attr(unordered, "top_level_index") <- xdoc_top_level_index(unordered)
    expect_null(attr(unordered, "top_level_index")$tokens)
    expect_equal(xml2::xml_text(xdoc_find_token(unordered, 1L, 1L)), "a")
})

test_that("Flat script token indexes preserve terminal nodes and source order", {
    content <- c(
        "# leading comment",
        sprintf("value_%03d <- sum(c(1, 2, 3))", seq_len(300L)),
        "text <- 'a multiline",
        "string' # trailing comment"
    )
    xdoc <- xml2::read_xml(xmlparsedata::xml_parse_data(utils::getParseData(
        parse(text = content, keep.source = TRUE))))
    expected <- xml2::xml_find_all(xdoc, "//*[@line1 and not(*)]")
    index <- xdoc_top_level_index(xdoc)$tokens
    expect_identical(xml2::xml_attr(index$nodes, "id"), xml2::xml_attr(expected, "id"))
    expect_identical(index$line1, as.integer(xml2::xml_attr(expected, "line1")))
    expect_identical(index$col1, as.integer(xml2::xml_attr(expected, "col1")))
    expect_identical(index$is_string, xml2::xml_name(expected) == "STR_CONST")
})

test_that("indexed definition queries preserve XPath scope and result order", {
    content <- c(
        "global <- 1",
        "{",
        "  outer <- function(argument) {",
        "    for (argument in seq_len(argument)) {",
        "      local <- argument",
        "      inner <- function(argument) argument + local",
        "    }",
        "    argument + local",
        "  }",
        "}",
        "left <- left <- 1",
        "1 -> right -> right",
        "global <- global + 1",
        "equal = function(argument) argument",
        "outer(global)"
    )
    xml <- xmlparsedata::xml_parse_data(utils::getParseData(
        parse(text = content, keep.source = TRUE)))
    fallback <- xml2::read_xml(xml)
    indexed <- xml2::read_xml(xml)
    attr(indexed, "top_level_index") <- xdoc_top_level_index(indexed)
    tokens <- xml2::xml_find_all(fallback,
        "//SYMBOL | //SYMBOL_FUNCTION_CALL | //SYMBOL_FORMALS")
    for (token in tokens) {
        line <- as.integer(xml2::xml_attr(token, "line1"))
        col <- as.integer(xml2::xml_attr(token, "col1"))
        name <- xml2::xml_text(token)
        scopes <- xdoc_find_enclosing_scopes(fallback, line, col, top = TRUE)
        for (template in list(definition_xpath, hover_xpath, signature_xpath)) {
            xpath <- glue(template, row = line,
                start = xml2::xml_attr(token, "start"),
                end = xml2::xml_attr(token, "end"),
                token_quote = xml_single_quote(name))
            expect_identical(
                xml2::xml_attr(xdoc_find_definitions(indexed, line, col, name, xpath), "id"),
                xml2::xml_attr(xml2::xml_find_all(scopes, xpath), "id"),
                info = paste(name, "at", line, col)
            )
        }
    }
})

test_that("native reference resolution restores enclosing scopes after siblings", {
    resolve <- function(groups, lines, definition_groups, starts, ends,
        cols = rep.int(1L, length(lines)),
        start_cols = rep.int(1L, length(starts)),
        end_cols = rep.int(9L, length(ends))) {
        .Call(
            "reference_resolve_local_c", PACKAGE = "languageserver",
            as.integer(groups), as.integer(lines), as.integer(cols),
            as.integer(definition_groups), as.integer(starts),
            as.integer(start_cols), as.integer(ends), as.integer(end_cols)
        )
    }
    # Same-name scopes overlap, nest, start together and end before the next
    # reference. Occurrences intentionally arrive out of source order.
    expect_identical(
        resolve(c(1, 1, 1, 1, 1, 1, 2, NA), c(12, 4, 6, 9, 2, 30, 4, 4),
            c(1, 1, 1, 1, 2), c(1, 3, 8, 3, 2), c(20, 5, 10, 4, 9)),
        c(1L, 4L, 1L, 3L, 1L, 0L, 5L, 0L)
    )
    # Columns beyond a million must not alias positions on subsequent lines.
    expect_identical(
        resolve(c(1, 1), c(1, 2), 1, 1, 1,
            cols = c(1500000, 1), start_cols = 1000000, end_cols = 2000000),
        c(1L, 0L)
    )
    expect_identical(resolve(1, 1, integer(), integer(), integer()), 0L)
    expect_identical(resolve(integer(), integer(), 1, 1, 2), integer())
})

test_that("reference name indexes serialize and preserve fallback results", {
    uri <- "file:///reference-name-index.R"
    content <- c(
        "outer <- function(argument) {",
        "  local <- argument",
        "  inner <- function(argument) argument + local",
        "  argument + local",
        "}",
        "outer(1)"
    )
    parsed <- parse_document(uri, content)
    index <- unserialize(serialize(parsed$reference_index, NULL))
    expect_true(is.environment(index$by_name))
    fallback <- index
    fallback$by_name <- NULL
    for (name in unique(index$name)) {
        expect_identical(reference_indices(index, name), which(index$name == name))
        rows <- reference_indices(index, name)
        for (i in rows) {
            point <- list(row = index$line[[i]], col = index$code_point_col[[i]])
            expect_identical(reference_key_at(index, point, name),
                reference_key_at(fallback, point, name))
        }
    }
    expect_identical(reference_indices(index, "missing"), integer())
    expect_null(reference_key_at(NULL, list(row = 0L, col = 0L), "missing"))
})

test_that("document symbol caches distinguish capabilities and changed parses", {
    uri <- "file:///document-symbol-cache.R"
    document <- Document$new(uri, version = 1L, content = "value <- 1")
    workspace <- Workspace$new(NULL)
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, document$content)
    parsed$version <- 1L
    workspace$update_parse_data(uri, parsed)
    flat <- list(hierarchicalDocumentSymbolSupport = FALSE)
    hierarchical <- list(hierarchicalDocumentSymbolSupport = TRUE)
    first <- document_symbol_reply(1L, uri, workspace, document, flat)
    again <- document_symbol_reply(2L, uri, workspace, document, flat)
    expect_equal(first$result, again$result)
    expect_equal(again$id, 2L)
    expect_true(!is.null(again$result[[1L]]$location))
    nested <- document_symbol_reply(3L, uri, workspace, document, hierarchical)
    expect_null(nested$result[[1L]]$location)
    expect_length(parsed$document_symbols_cache, 2L)

    document$set_content(2L, "replacement <- 2")
    expect_null(document_symbol_reply(4L, uri, workspace, document, flat))
    changed <- parse_document(uri, document$content)
    changed$version <- 2L
    workspace$update_parse_data(uri, changed)
    reply <- document_symbol_reply(5L, uri, workspace, document, flat)
    expect_equal(reply$result[[1L]]$name, "replacement")
})
