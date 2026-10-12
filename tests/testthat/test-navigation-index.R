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
    expect_identical(resolve(integer(), integer(), integer(), integer(), integer()), integer())
    expect_identical(resolve(c(0, -1), c(1, 1), c(1, 1), c(1, 1), c(2, 2)), c(0L, 0L))
    expect_identical(resolve(c(1, 1), c(1, 2), c(0, -1), c(1, 1), c(2, 2)), c(0L, 0L))
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

test_that("shared syntax navigation preserves XPath boundaries and declaration order", {
    content <- c(
        "# leading comment",
        "global <- 1",
        "{",
        "  outer <- function(argument = 1) {",
        "    for (argument in seq_len(argument)) {",
        "      local <- argument # trailing comment",
        "      inner <- function(argument) argument + local",
        "    }",
        "    argument + local",
        "  }",
        "}",
        "left <- left <- 1",
        "1 -> right -> right",
        "global <- global + 1",
        "equal = function(argument) argument",
        "outer(global)",
        "lambda <- \\(argument = 1) argument",
        "parenthesized <- (function(argument) argument)",
        "member$field <- global",
        paste0('"', intToUtf8(0x1f600), '"; `', intToUtf8(0x03b1), '` <- global')
    )
    parsed <- parse_document("file:///shared-navigation.R", content)
    syntax <- parsed$syntax_data
    xdoc <- xml2::read_xml(parsed$xml_data)
    nodes <- xml2::xml_find_all(xdoc, "descendant::*")
    node_rows <- function(selected) {
        vapply(as.list(selected), function(node) {
            which(vapply(as.list(nodes), function(candidate) {
                identical(candidate$node, node$node)
            }, logical(1L)))
        }, integer(1L))
    }
    expect_equal(length(syntax$id), length(nodes))
    for (line in seq_along(content)) {
        for (col in seq_len(nchar(content[[line]]) + 2L)) {
            actual <- .Call("syntax_enclosing_c", syntax, as.integer(c(line, col)),
                PACKAGE = "languageserver")
            expect_identical(syntax$order_rank[actual],
                node_rows(xdoc_find_enclosing_scopes(xdoc, line, col)),
                info = paste("scope", line, col))
            row <- navigation_token(parsed, line, col)
            old <- xdoc_find_token(xdoc, line, col)
            if (length(old)) {
                expect_identical(nodes[[syntax$order_rank[[row]]]]$node, old$node,
                    info = paste("token", line, col))
            } else expect_identical(row, 0L)
        }
    }
    for (row in syntax$terminals) {
        if (!syntax$token[[row]] %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS")) next
        line <- syntax$line1[[row]]
        col <- syntax$col1[[row]]
        name <- syntax$text[[row]]
        token <- nodes[[syntax$order_rank[[row]]]]
        for (mode in c("definition", "hover", "signature")) {
            xpath <- glue(get(paste0(mode, "_xpath")), row = line,
                start = xml2::xml_attr(token, "start"), end = xml2::xml_attr(token, "end"),
                token_quote = xml_single_quote(name))
            expected <- xml2::xml_find_all(xdoc_find_enclosing_scopes(xdoc, line, col, top = TRUE), xpath)
            actual <- navigation_definitions(parsed, line, col, name, mode, row)
            expect_identical(syntax$order_rank[actual], node_rows(expected),
                info = paste(mode, name, line, col))
            expect_identical(navigation_definitions(parsed, line, col, name, mode, row, last = TRUE),
                tail(actual, 1L))
        }
    }
    restored <- unserialize(serialize(as.list(parsed), NULL))
    expect_identical(restored$syntax_data, syntax)
    expect_identical(navigation_definitions(restored, 16L, 2L, "global", last = TRUE),
        navigation_definitions(parsed, 16L, 2L, "global", last = TRUE))
})

test_that("indexed navigation retains legacy global sibling histories and formal precedence", {
    parsed <- parse_document("file:///legacy-navigation-order.R", c(
        "first <- function() { value <- 1; value }",
        "second <- function() { value <- 2; value }",
        "value",
        "third <- function(value) { value <- value + 1; value }"
    ))
    syntax <- parsed$syntax_data
    found <- navigation_definitions(parsed, 3L, 1L, "value", last = TRUE)
    # The existing providers include sibling assignments through the XML root.
    expect_equal(syntax$line1[found], 2L)
    formal <- navigation_definitions(parsed, 4L, 50L, "value", last = TRUE)
    expect_equal(syntax$token[formal], "SYMBOL_FORMALS")
    rows <- which(syntax$token == "SYMBOL" & syntax$text == "value" & syntax$line1 == 4L)
    rhs <- rows[[2L]]
    assignments <- navigation_definitions(parsed, 4L, syntax$col1[[rhs]], "value", token = rhs)
    expect_false(any(syntax$line1[assignments] == 4L & syntax$token[assignments] == "expr"))
})

test_that("indexed navigation resolves apostrophes in backtick identifiers", {
    fixture <- provider_fixture(c("`odd'name` <- 1", "`odd'name`"))
    point <- list(row = 1L, col = 4L)
    parsed <- fixture$document$parse_data
    parsed$xml_doc <- parsed$xml_data <- NULL
    definition <- definition_reply(1L, fixture$uri, fixture$workspace,
        fixture$document, point, getwd())$result
    expect_identical(definition$uri, fixture$uri)
    expect_equal(definition$range, range(position(0L, 0L), position(0L, 15L)))
    hover <- hover_reply(2L, fixture$uri, fixture$workspace, fixture$document, point)$result
    expect_identical(hover$contents, "```r\n`odd'name` <- 1\n```")
    expect_null(parsed$xml_doc)
})

test_that("member navigation does not resolve same-name local bindings", {
    withr::local_options(languageserver.member_completion = FALSE)
    previous <- lsp_settings$get("member_completion")
    lsp_settings$set("member_completion", FALSE)
    withr::defer(lsp_settings$set("member_completion", previous))
    content <- c("probe <- function() {", "  value <- 42", "  obj$value",
        "  obj $ value", "  obj$`value`", "  value", "}")
    uri <- "file:///member-navigation.R"
    document <- Document$new(uri, version = 1L, content = content)
    workspace <- Workspace$new(NULL)
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, content)
    parsed$version <- 1L
    workspace$update_parse_data(uri, parsed)
    legacy <- list2env(as.list(parsed), parent = emptyenv())
    legacy$syntax_data <- legacy$navigation_data <- NULL
    legacy$xml_doc <- xml2::read_xml(parsed$xml_data)
    attr(legacy$xml_doc, "top_level_index") <- xdoc_top_level_index(legacy$xml_doc)
    alternate <- list2env(as.list(parsed), parent = emptyenv())
    alternate$syntax_data$token[alternate$syntax_data$token == "'$'"] <- "OP-DOLLAR"
    for (data in list(parsed, alternate, legacy)) {
        document$parse_data <- data
        for (row in 2:4) {
            point <- list(row = row, col = nchar(content[[row + 1L]]) - 2L)
            expect_null(definition_reply(1L, uri, workspace, document, point, getwd())$result)
            expect_null(hover_reply(2L, uri, workspace, document, point)$result)
            if (!is.null(data$syntax_data)) {
                token <- navigation_token(data, row + 1L, point$col + 1L)
                expect_true(navigation_after_dollar(data$syntax_data, token))
            }
        }
        point <- list(row = 5L, col = 4L)
        definition <- definition_reply(1L, uri, workspace, document, point, getwd())$result
        expect_equal(definition$range, range(position(1L, 2L), position(1L, 13L)))
        expect_identical(hover_reply(2L, uri, workspace, document, point)$result$contents,
            "```r\nvalue <- 42\n```")
    }
})

test_that("indexed local providers preserve fallback responses without XML", {
    content <- c(
        "#' Local function documentation.",
        "#' @param argument The function argument.",
        "local <- function(argument = 1) { argument + 1 }",
        "value <- 2",
        "local(argument = value)",
        "value",
        "function(argument) argument -> right_function",
        "right_function(argument = value)"
    )
    uri <- "file:///indexed-local-providers.R"
    document <- Document$new(uri, version = 1L, content = content)
    workspace <- Workspace$new(NULL)
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, content)
    parsed$version <- 1L
    workspace$update_parse_data(uri, parsed)
    legacy <- list2env(as.list(parsed), parent = emptyenv())
    legacy$syntax_data <- legacy$navigation_data <- NULL
    legacy$xml_doc <- xml2::read_xml(parsed$xml_data)
    attr(legacy$xml_doc, "top_level_index") <- xdoc_top_level_index(legacy$xml_doc)
    points <- list(list(row = 5L, col = 2L), list(row = 4L, col = 2L), list(row = 4L, col = 9L),
        list(row = 7L, col = 3L), list(row = 7L, col = 17L))
    replies <- function(parse_data) {
        document$parse_data <- parse_data
        lapply(points, function(point) list(
            definition = definition_reply(1L, uri, workspace, document, point, getwd())$result,
            hover = hover_reply(1L, uri, workspace, document, point)$result,
            signature = signature_reply(1L, uri, workspace, document, point)$result
        ))
    }
    expected <- replies(legacy)
    parsed$xml_doc <- NULL
    expect_equal(replies(parsed), expected)
    expect_null(parsed$xml_doc)
    expect_true(length(ls(parsed$navigation_data$cache)) > 0L)
})

test_that("shared syntax builders preserve native completion semantic and range facts", {
    content <- c(
        "# detached comment",
        "value <- 1",
        "outer <- function(argument = (function(x) x)) {",
        "  local <- function(x) x",
        "  for (i in 1:3) argument(i)",
        "}",
        "(function(x) x) -> right",
        "parenthesized <- (function(x) x)",
        "outer(argument = value) # trailing comment"
    )
    data <- utils::getParseData(parse(text = content, keep.source = TRUE))
    syntax <- syntax_parse_data(data)
    expect_identical(completion_parse_data(data, syntax), completion_parse_data(data))
    expect_identical(semantic_parse_data(data, content, syntax), semantic_parse_data(data, content))
    expect_identical(range_provider_parse_data(data, content, syntax), range_provider_parse_data(data, content))
    malformed <- syntax
    malformed$id_next <- integer()
    expect_error(completion_parse_data(data, malformed), "invalid shared completion column")
    expect_error(range_provider_parse_data(data, content, malformed), "invalid shared provider syntax")
})

test_that("native syntax navigation rejects malformed query shapes and row ids", {
    parsed <- parse_document("file:///navigation-validation.R", "value <- 1; value")
    syntax <- parsed$syntax_data
    index <- parsed$navigation_data
    expect_error(.Call("syntax_enclosing_c", syntax, 1L, PACKAGE = "languageserver"), "invalid syntax point")
    broken <- syntax
    broken$roots[[1L]] <- length(syntax$id) + 1L
    expect_error(.Call("syntax_enclosing_c", broken, c(1L, 1L), PACKAGE = "languageserver"), "invalid navigation row")
    broken <- syntax
    broken$line1 <- integer()
    expect_error(.Call("syntax_enclosing_c", broken, c(1L, 1L), PACKAGE = "languageserver"), "invalid syntax column length")
    query <- function(candidates = 1L, point = c(1L, 13L, 0L), mode = 1L) {
        .Call("navigation_select_c", syntax, index, candidates, point, mode, TRUE, PACKAGE = "languageserver")
    }
    expect_error(query(point = 1L), "invalid navigation query")
    expect_error(query(candidates = 0L), "invalid navigation row")
    expect_error(query(mode = 4L), "invalid navigation mode")
})

test_that("accepted indexed parses install without XML and materialize legacy XML once", {
    uri <- "file:///lazy-navigation-parse.R"
    content <- c("local <- function(argument) argument", "local(1)")
    document <- Document$new(uri, version = 1L, content = content)
    workspace <- Workspace$new(NULL)
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, content)
    parsed$version <- 1L
    workspace$update_parse_data(uri, parsed)
    expect_identical(document$parse_data, parsed)
    expect_null(parsed$xml_doc)
    point <- list(row = 1L, col = 2L)
    expect_true(!is.null(definition_reply(1L, uri, workspace, document, point, getwd())$result))
    expect_true(!is.null(hover_reply(1L, uri, workspace, document, point)$result))
    expect_null(parsed$xml_doc)

    first <- parse_data_xml(parsed)
    expect_s3_class(first, "xml_document")
    expect_identical(parse_data_xml(parsed), first)
    expect_identical(parsed$xml_doc, first)
    expect_identical(attr(first, "top_level_index")$syntax, parsed$syntax_data)
    restored <- list2env(unserialize(serialize(as.list(parsed)[setdiff(names(as.list(parsed)), "xml_doc")], NULL)),
        parent = emptyenv())
    expect_null(restored$xml_doc)
    expect_s3_class(parse_data_xml(restored), "xml_document")
})

test_that("normalized parse cache hits isolate original-content navigation caches", {
    uri <- "file:///normalized-navigation-cache.R"
    spaced <- c(
        "#' @param argument An argument.",
        "local <- function(argument = 1) argument",
        "local(argument = 1)"
    )
    tabbed <- sub("argument = 1", "argument\t= 1", spaced, fixed = TRUE)
    expect_identical(get_content_hash(normalize_parse_content(spaced)),
        get_content_hash(normalize_parse_content(tabbed)))
    document <- Document$new(uri, version = 1L, content = spaced)
    workspace <- Workspace$new(NULL)
    workspace$documents$set(uri, document)
    self <- list(get_workspace = function(...) workspace,
        resolve_task_manager = list(add_task = function(...) NULL),
        pending_replies = collections::dict(), request_handlers = list())
    parsed <- parse_document(uri, spaced)
    parsed$version <- 1L
    workspace$update_parse_data(uri, parsed)
    declaration <- navigation_definitions(parsed, 3L, 2L, "local", "signature", last = TRUE)[[1L]]
    navigation_signature(document, parsed, declaration, "local")
    navigation_documentation(document, parsed, 2L)
    signature_key <- paste0("signature:", declaration, ":local")
    # Existing cached response state must never enter immutable parse snapshots.
    assign(signature_key, "stale cached header", envir = parsed$navigation_data$cache)
    assign("documentation:2", "stale cached documentation", envir = parsed$navigation_data$cache)
    parsed$link_strings_cache <- list(content = spaced, strings = list())
    parsed$linked_editing_cache <- list(content = spaced)
    parse_data_xml(parsed)
    parse_callback(self, uri, 1L, parsed, spaced)
    cached <- workspace$parse_cache$get(parsed$content_hash)
    expect_null(cached$xml_doc)
    expect_null(cached$link_strings_cache)
    expect_null(cached$linked_editing_cache)
    expect_length(ls(cached$navigation_data$cache), 0L)
    expect_equal(get(signature_key, envir = parsed$navigation_data$cache), "stale cached header")
    expect_false(identical(cached$navigation_data$cache, parsed$navigation_data$cache))

    document$set_content(2L, tabbed)
    expect_null(parse_task(self, uri, document))
    accepted <- document$parse_data
    expect_equal(accepted$version, 2L)
    expect_null(accepted$xml_doc)
    expect_length(ls(accepted$navigation_data$cache), 0L)
    expect_false(identical(accepted$navigation_data$cache, cached$navigation_data$cache))
    point <- list(row = 2L, col = 2L)
    actual <- hover_reply(1L, uri, workspace, document, point)$result
    fresh <- parse_document(uri, tabbed)
    fresh$version <- 2L
    document$parse_data <- fresh
    expected <- hover_reply(1L, uri, workspace, document, point)$result
    expect_equal(actual, expected)
    expect_false(any(grepl("stale cached", actual$contents, fixed = TRUE)))
    expect_null(accepted$xml_doc)
    expect_equal(get(signature_key, envir = parsed$navigation_data$cache), "stale cached header")
})

test_that("native syntax builders validate source lengths and adjacency before traversal", {
    data <- utils::getParseData(parse(text = "value <- 1; value", keep.source = TRUE))
    columns <- as.list(data)
    columns$id <- integer()
    expect_error(.Call("syntax_index_c", columns, seq_len(nrow(data)), PACKAGE = "languageserver"),
        "invalid syntax data lengths")
    syntax <- syntax_parse_data(data)
    malformed <- syntax
    malformed$parent_row <- integer()
    expect_error(.Call("navigation_index_c", malformed, PACKAGE = "languageserver"),
        "invalid syntax column length")
    malformed <- syntax
    malformed$first_child[[1L]] <- length(syntax$id) + 1L
    expect_error(.Call("navigation_index_c", malformed, PACKAGE = "languageserver"),
        "invalid navigation adjacency")
    malformed <- syntax
    malformed$next_sibling[[1L]] <- 1L
    expect_error(.Call("navigation_index_c", malformed, PACKAGE = "languageserver"),
        "invalid navigation sibling")
    expect_identical(.Call("syntax_enclosing_c", syntax, c(1L, NA_integer_), PACKAGE = "languageserver"), integer())
    expect_identical(.Call("syntax_enclosing_c", syntax, c(NA_integer_, 1L), PACKAGE = "languageserver"), integer())
})

test_that("native syntax navigation rejects malformed ranges and cyclic child rows", {
    data <- utils::getParseData(parse(text = "value <- 1; value", keep.source = TRUE))
    columns <- as.list(data)
    columns$terminal[[1L]] <- NA
    expect_error(.Call("syntax_index_c", columns, seq_len(nrow(data)), PACKAGE = "languageserver"), "invalid syntax range")
    columns <- as.list(data)
    columns$col2[[1L]] <- 0L
    expect_error(.Call("syntax_index_c", columns, seq_len(nrow(data)), PACKAGE = "languageserver"), "invalid syntax range")
    syntax <- syntax_parse_data(data)
    root <- syntax$roots[[1L]]
    child <- syntax_children(syntax, root)[[1L]]
    syntax$token[[child]] <- "other"
    syntax$children <- c(child, child)
    syntax$child_start[[root]] <- 1L
    syntax$child_end[[root]] <- 1L
    syntax$child_start[[child]] <- 2L
    syntax$child_end[[child]] <- 2L
    expect_error(.Call("syntax_enclosing_c", syntax, c(1L, 1L), PACKAGE = "languageserver"), "cyclic syntax children")
})

test_that("accepted syntax snapshots omit construction-only adjacency", {
    parsed <- parse_document("file:///compact-syntax.R", c("f <- function(x) { x + 1 }", "f(1)"))
    expect_false(any(c("parent", "first_child", "next_sibling", "function_scope", "top", "order") %in%
        names(parsed$syntax_data)))
    restored <- unserialize(serialize(parsed, NULL))
    expect_identical(restored$syntax_data, parsed$syntax_data)
    expect_gt(navigation_token(restored, 2L, 1L), 0L)
    expect_gt(length(.Call("syntax_enclosing_c", restored$syntax_data, c(1L, 23L),
        PACKAGE = "languageserver")), 0L)
    expect_false(is.null(parse_data_xml(restored)))
})
