xml_reference <- function(content, include_text = NA) {
    parsed <- parse(text = content, keep.source = TRUE)
    data <- utils::getParseData(parsed, includeText = include_text)
    xml <- xmlparsedata::xml_parse_data(data)
    if (!is.null(data) && is.null(data$text)) data$text <- rep("", nrow(data))
    list(data = data, xml = xml)
}

test_that("native XML matches parser XML for syntax and detached comments", {
    cases <- list(
        "", "# comment only", c("# before", "x <- 1 # after", "# last"),
        c("f <- function(x = 1, ..., y = quote()) {", "  # nested", "  g(x, , y = )", "}"),
        "if (x) { for (i in 1:3) x[[i]] <- -i } else repeat { break }",
        "while (!x) { x <- x + 1; next }; x -> y; x <<- 2; y ->> z",
        "x <- (a + b) * c / d ^ e; x ~ y; ?help; a:b; x$y; x@y; x[1, ]; x[[2]]",
        "pkg::fun(a = TRUE, NULL, NA, Inf, NaN); pkg:::internal(...); a %op% b",
        "`odd&<name>` <- 'a&b<c>d'; value <- '\"quotes\"'",
        c("α <- '😀中文'", "fun <- function(β = α) β", "# café & < >"),
        c("{", "# before the only expression", "x", "# after the only expression", "}"),
        c("x <- ", "  function()", "    1", "# detached", "y <- 2"),
        'x <- r"--(hello\n& < > " quotes)--"')
    if (getRversion() >= "4.1.0") {
        cases <- c(cases, list(r"(f <- \(x) x |> identity())"))
    }
    for (content in cases) {
        for (include_text in c(FALSE, TRUE)) {
            reference <- xml_reference(content, include_text)
            expect_identical(syntax_xml(syntax_parse_data(reference$data)), reference$xml,
                info = paste(content, collapse = "\n"))
        }
    }
})

test_that("native XML preserves text encoding escaping and numeric formatting", {
    reference <- xml_reference("x <- 'text'")
    data <- reference$data
    data$text[data$terminal] <- paste0("&<>\t\n\r", intToUtf8(c(1, 8, 11, 31)), "é")
    Encoding(data$text) <- "UTF-8"
    expect_identical(syntax_xml(data), xmlparsedata::xml_parse_data(data))
    data$text <- iconv(data$text, "UTF-8", "latin1")
    Encoding(data$text) <- "latin1"
    data$line1 <- data$line1 + 100000L
    data$line2 <- data$line2 + 100000L
    for (penalty in c(-9, 0, 100)) {
        withr::local_options(scipen = penalty)
        expect_identical(syntax_xml(data), xmlparsedata::xml_parse_data(data))
    }
    # Stable ties matter for nested ranges sharing both endpoints.
    data <- reference$data[rev(seq_len(nrow(reference$data))), ]
    expect_identical(syntax_xml(data), xmlparsedata::xml_parse_data(data))
})

test_that("native XML handles generated combinations of nested expressions", {
    leaves <- c("x", "1", "'&<>é'", "f(, x = )", "x$y", "pkg::fun(x)")
    wrappers <- c("(%s)", "{ # before\n%s\n# after\n}", "function(a = %s) a", "if (x) %s else NULL")
    for (leaf in leaves) {
        for (wrapper in wrappers) {
            content <- sprintf(wrapper, leaf)
            reference <- xml_reference(content)
            expect_identical(syntax_xml(reference$data), reference$xml)
        }
    }
})

test_that("native XML validates column shapes before reading storage", {
    data <- xml_reference("x <- 1")$data
    expect_error(.Call("syntax_xml_c", NULL, PACKAGE = "languageserver"), "column")
    for (column in c("text", "token", "terminal", "line1", "col1", "line2", "col2")) {
        broken <- as.list(data)
        broken[[column]] <- broken[[column]][-1L]
        expect_error(.Call("syntax_xml_c", broken, PACKAGE = "languageserver"), "column")
    }
    broken <- data
    broken$line1[[1L]] <- NA_integer_
    expect_error(syntax_xml(broken), "position")
    broken <- data
    broken$terminal[[1L]] <- NA
    expect_error(syntax_xml(broken), "position")
    broken <- data
    broken$token[[1L]] <- "'<future>'"
    expect_null(.Call("syntax_xml_c", broken, PACKAGE = "languageserver"))
})

test_that("worker snapshots defer XML and restore equivalent legacy providers", {
    uri <- "file:///deferred-xml.R"
    content <- c("# a function", "f <- function(x) {", "  value <- x + 1", "  value", "}", "f(2)")
    eager <- parse_document(uri, content)
    deferred <- parse_document(uri, content, include_xml = FALSE)
    expect_lt(length(serialize(deferred, NULL)), length(serialize(eager, NULL)))
    expect_null(deferred$xml_data)
    expect_null(deferred$xml_doc)
    expect_true(deferred$xml_deferred)
    restored <- unserialize(serialize(deferred, NULL))
    expect_identical(as.character(parse_data_xml(restored)), as.character(parse_data_xml(eager)))
    expect_identical(parse_data_xml(restored), restored$xml_doc)
    expect_null(restored$xml_data)
    expect_identical(restored$definitions, eager$definitions)
    expect_identical(restored$completion_data, eager$completion_data)
})

test_that("deferred XML preserves class indexes and incomplete documents", {
    uri <- "file:///deferred-class.R"
    content <- c("Thing <- R6::R6Class('Thing', public = list(value = 1, get = function() self$value))",
        "methods::setClass('S4Thing', slots = c(value = 'numeric'))")
    eager <- parse_document(uri, content)
    deferred <- parse_document(uri, content, include_xml = FALSE)
    expect_null(deferred$xml_data)
    expect_equal(as.list(deferred$class_data$members), as.list(eager$class_data$members))
    expect_equal(as.list(deferred$class_data$supertypes), as.list(eager$class_data$supertypes))
    broken <- parse_document(uri, "f(", include_xml = FALSE)
    expect_true(broken$parse_error)
    expect_s3_class(parse_data_xml(broken), "xml_document")
})

test_that("cursor highlights use syntax with equivalent XML token semantics", {
    uri <- "file:///highlight-syntax.R"
    content <- c("x <- 123", "y <- 123", "α <- '😀 & < >'", "β <- '😀 & < >'",
        "f <- function(arg) arg", "g <- function(arg) arg", "stats::lm(x)", "stats::median(y)",
        "x$property", "x$property", "# 123 and stats", "x + y")
    workspace <- Workspace$new(NULL)
    document <- Document$new(uri, content = content, version = 1L)
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, content, include_xml = FALSE)
    parsed$version <- 1L
    workspace$update_parse_data(uri, parsed)
    legacy <- list2env(as.list(parsed), parent = .GlobalEnv)
    legacy$xml_data <- syntax_xml(parsed$syntax_data)
    legacy$syntax_data <- NULL
    fast <- document_highlight_reply
    mockery::stub(fast, "parse_data_xml", function(...) stop("cursor request must not build XML"))
    for (line in seq_along(content)) {
        for (col in seq.int(0L, nchar(content[[line]]))) {
            point <- list(row = line - 1L, col = col)
            document$parse_data <- legacy
            expected <- document_highlight_reply(1L, uri, workspace, document, point)$result
            document$parse_data <- parsed
            expect_equal(fast(1L, uri, workspace, document, point)$result, expected,
                info = paste("line", line, "column", col))
        }
    }
    expect_length(fast(1L, uri, workspace, document, list(row = 2L, col = 6L))$result, 2L)
    expect_null(parsed$xml_doc)
})
