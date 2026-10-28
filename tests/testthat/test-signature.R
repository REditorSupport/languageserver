# Cross-file signature tests depend on the definition parse, not a fixed delay.
signature_open_files <- function(client, definition, call) {
    did_open(client, definition)
    expect_gt(length(respond_document_symbol(client, definition)), 0L)
    did_open(client, call)
}

test_that("Simple signature works", {
    skip_on_cran()
    client <- language_client(working_dir = withr::local_tempdir())

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "file.path(",
            "fs::path_home('foo', "
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_signature(temp_file, c(0, 10))
    expect_length(result$signatures, 1)
    expect_match(result$signatures[[1]]$label, "file\\.path\\(.*")
    expect_equal(result$signatures[[1]]$documentation$kind, "markdown")
    expect_match(result$signatures[[1]]$documentation$value,
        ".*Construct the path to a file from components in a platform-independent way.*")

    result <- client %>% respond_signature(temp_file, c(1, 21))
    expect_length(result$signatures, 1)
    expect_match(result$signatures[[1]]$label, "path_home\\(.*")
    expect_equal(result$signatures[[1]]$documentation$kind, "markdown")
    expect_match(result$signatures[[1]]$documentation$value,
        ".*`path_expand\\(\\)` performs tilde expansion on a path.*")
})

test_that("Signature of user function works", {
    skip_on_cran()
    client <- language_client(working_dir = withr::local_tempdir())

    defn_file <- withr::local_tempfile(fileext = ".R")
    temp_file <- withr::local_tempfile(fileext = ".R")

    writeLines(c("foo <- function(x, y = 3) { x + y }"), defn_file)
    writeLines(c("foo(3, "), temp_file)

    signature_open_files(client, defn_file, temp_file)

    result <- client %>% respond_signature(
        temp_file, c(0, 7),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_length(result$signatures, 1)
    expect_equal(result$signatures[[1]]$label, "foo(x, y = 3)")
})

test_that("Signature of user function works with semi-colon", {
    skip_on_cran()
    client <- language_client(working_dir = withr::local_tempdir())

    defn_file <- withr::local_tempfile(fileext = ".R")
    temp_file <- withr::local_tempfile(fileext = ".R")

    writeLines(c("foo <- function(x, y = 3) { x + y };"), defn_file)
    writeLines(c("foo(3, "), temp_file)

    signature_open_files(client, defn_file, temp_file)

    result <- client %>% respond_signature(
        temp_file, c(0, 7),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_length(result$signatures, 1)
    expect_equal(result$signatures[[1]]$label, "foo(x, y = 3)")
})

test_that("Signature in Rmarkdown works", {
    skip_on_cran()
    client <- language_client(working_dir = withr::local_tempdir())

    temp_file <- withr::local_tempfile(fileext = ".Rmd")
    writeLines(
        c(
            "Title",
            "",
            "```{r}",
            "file.path(",
            "fs::path_home('foo', ",
            "```",
            "file.path("
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_signature(temp_file, c(3, 10))
    expect_length(result$signatures, 1)
    expect_match(result$signatures[[1]]$label, "file\\.path\\(.*")

    result <- client %>% respond_signature(temp_file, c(4, 21))
    expect_length(result$signatures, 1)
    expect_match(result$signatures[[1]]$label, "path_home\\(.*")

    result <- client %>% respond_signature(temp_file, c(5, 10))
    expect_length(result$signatures, 0)
})

test_that("activeParameter is correctly computed", {
    skip_on_cran()
    client <- language_client(working_dir = withr::local_tempdir())

    defn_file <- withr::local_tempfile(fileext = ".R")
    temp_file <- withr::local_tempfile(fileext = ".R")

    writeLines(c("foo <- function(a, b, c, d) { a + b + c + d }"), defn_file)
    writeLines(c("foo(1, "), temp_file)

    signature_open_files(client, defn_file, temp_file)

    # Test at first parameter (after opening parenthesis)
    result <- client %>% respond_signature(
        temp_file, c(0, 4),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 0)
    expect_length(result$signatures[[1]]$parameters, 4)

    # Test at second parameter (after first comma)
    result <- client %>% respond_signature(
        temp_file, c(0, 7),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 1)
    expect_length(result$signatures[[1]]$parameters, 4)

    # Test with more parameters
    writeLines(c("foo(1, 2, 3, "), temp_file)
    client %>% did_open(temp_file)
    
    result <- client %>% respond_signature(
        temp_file, c(0, 13),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 3)
    expect_length(result$signatures[[1]]$parameters, 4)
})

test_that("activeParameter handles nested calls", {
    skip_on_cran()
    client <- language_client(working_dir = withr::local_tempdir())

    defn_file <- withr::local_tempfile(fileext = ".R")
    temp_file <- withr::local_tempfile(fileext = ".R")

    writeLines(c("foo <- function(a, b, c) { a + b + c }"), defn_file)
    writeLines(c("foo(1, foo(2, 3), "), temp_file)

    signature_open_files(client, defn_file, temp_file)

    # Test at outer function's third parameter
    result <- client %>% respond_signature(
        temp_file, c(0, 18),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 2)
})

test_that("activeParameter correctly handles named arguments", {
    skip_on_cran()
    client <- language_client(working_dir = withr::local_tempdir())

    defn_file <- withr::local_tempfile(fileext = ".R")
    temp_file <- withr::local_tempfile(fileext = ".R")

    writeLines(c("fun <- function(x, y = 1, z = 2) { x + y + z }"), defn_file)
    
    # Test 1: Named argument z= should activate parameter z (index 2), not y
    writeLines(c("fun(1, z = "), temp_file)
    signature_open_files(client, defn_file, temp_file)
    
    result <- client %>% respond_signature(
        temp_file, c(0, 11),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 2)
    expect_length(result$signatures[[1]]$parameters, 3)
    
    # Test 2: Named argument y= should activate parameter y (index 1)
    writeLines(c("fun(x = 1, y = "), temp_file)
    client %>% did_open(temp_file)
    
    result <- client %>% respond_signature(
        temp_file, c(0, 15),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 1)
    
    # Test 3: Positional argument (no name) should use comma count
    writeLines(c("fun(1, 2, "), temp_file)
    client %>% did_open(temp_file)
    
    result <- client %>% respond_signature(
        temp_file, c(0, 10),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 2)
    
    # Test 4: Named argument after skipping parameters
    writeLines(c("fun(z = "), temp_file)
    client %>% did_open(temp_file)
    
    result <- client %>% respond_signature(
        temp_file, c(0, 8),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 2)
})

test_that("activeParameter correctly handles ... (ellipsis)", {
    skip_on_cran()
    client <- language_client(working_dir = withr::local_tempdir())

    defn_file <- withr::local_tempfile(fileext = ".R")
    temp_file <- withr::local_tempfile(fileext = ".R")

    writeLines(c("fun <- function(a, ..., b = 1) { a + b }"), defn_file)
    
    # Test 1: Positional args after 'a' should stick to ... (index 1)
    writeLines(c("fun(1, 2, "), temp_file)
    signature_open_files(client, defn_file, temp_file)
    
    result <- client %>% respond_signature(
        temp_file, c(0, 10),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 1)  # Should be ..., not b
    expect_length(result$signatures[[1]]$parameters, 3)
    
    # Test 2: More positional args should still stick to ...
    writeLines(c("fun(1, 2, 3, 4, "), temp_file)
    client %>% did_open(temp_file)
    
    result <- client %>% respond_signature(
        temp_file, c(0, 16),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 1)  # Still ..., not b
    
    # Test 3: Named argument b= should activate parameter b (index 2)
    writeLines(c("fun(1, 2, 3, b = "), temp_file)
    client %>% did_open(temp_file)
    
    result <- client %>% respond_signature(
        temp_file, c(0, 17),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 2)  # Explicitly named b
    
    # Test 4: First parameter before ...
    writeLines(c("fun("), temp_file)
    client %>% did_open(temp_file)
    
    result <- client %>% respond_signature(
        temp_file, c(0, 4),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 0)  # Should be 'a'
})

test_that("activeParameter handles ... at different positions", {
    skip_on_cran()
    client <- language_client(working_dir = withr::local_tempdir())

    defn_file <- withr::local_tempfile(fileext = ".R")
    temp_file <- withr::local_tempfile(fileext = ".R")

    # Test with ... at the end
    writeLines(c("fun2 <- function(a, b, ...) { a + b }"), defn_file)
    writeLines(c("fun2(1, 2, 3, 4, "), temp_file)
    signature_open_files(client, defn_file, temp_file)
    
    result <- client %>% respond_signature(
        temp_file, c(0, 17),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 2)  # Should stick to ... (index 2)
    
    # Test with ... at the beginning
    writeLines(c("fun3 <- function(..., x, y = 1) { x + y }"), defn_file)
    writeLines(c("fun3(1, 2, "), temp_file)
    signature_open_files(client, defn_file, temp_file)
    
    result <- client %>% respond_signature(
        temp_file, c(0, 11),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 0)  # Should stick to ... (index 0)
})

test_that("Signature parameter parsing respects nested defaults and quotes", {
    signature <- paste0(
        "fun(alpha, beta = list(1, 2), gamma = ",
        "c('a,b', \"c,d\"), `odd name` = { 1, 2 }, ...)"
    )

    expect_identical(
        extract_parameter_names(signature),
        c("alpha", "beta", "gamma", "`odd name`", "...")
    )
    expect_identical(extract_parameter_names("not a signature"), character())
    expect_identical(extract_parameter_names("empty()"), character())

    parameters <- parse_signature_parameters(signature)
    expect_length(parameters, 5L)
    labels <- vapply(parameters, function(parameter) {
        start <- parameter$label[[1L]] + 1L
        end <- parameter$label[[2L]]
        substr(signature, start, end)
    }, character(1L))
    expect_identical(
        labels,
        c(
            "alpha", "beta = list(1, 2)",
            "gamma = c('a,b', \"c,d\")", "`odd name` = { 1, 2 }", "..."
        )
    )
    expect_identical(parse_signature_parameters("missing"), list())
    expect_identical(parse_signature_parameters("empty(   )"), list())
})

test_that("Active parameter detection ignores nested and quoted commas", {
    signature <- "fun(first, second, third, ..., named = NULL)"
    cases <- list(
        list("fun(list(1, 2), ", 1L),
        list("fun('a,b', ", 1L),
        list(r"(fun("a,\"b", )", 1L),
        list("fun(first = 1, named = ", 4L),
        list("fun(1, unknown = ", 1L),
        list("fun(1, 2, 3, 4, ", 3L),
        list("fun(1, # comment, ignored", 1L)
    )

    for (case in cases) {
        content <- case[[1L]]
        actual <- detect_active_parameter(
            content, 0L, 3L, 0L, nchar(content), signature
        )
        expect_equal(actual, case[[2L]])
    }

    expect_equal(
        detect_active_parameter(c("fun(", NA_character_), 0L, 3L, 1L, 0L),
        0L
    )
    expect_equal(
        detect_active_parameter("fun(", 5L, 0L, 6L, 0L),
        0L
    )
    expect_equal(
        detect_active_parameter(
            c("fun(second # comment before =", "  = 1)"),
            0L, 3L, 1L, 5L, signature
        ),
        1L
    )
    commented_sig <- paste(
        "fun # comment (not_a_param)",
        "(",
        "  # comment before first",
        "  first = 1 # comment after default",
        "  ,",
        "  second # comment before =",
        "    = 2,",
        "  third # comment after parameter",
        ")",
        sep = "\n"
    )
    info <- signature_info(commented_sig)
    expect_identical(info$names, c("first", "second", "third"))
    expect_identical(
        vapply(info$parameters, function(parameter) {
            substr(commented_sig, parameter$label[[1L]] + 1L, parameter$label[[2L]])
        }, character(1L)),
        c("first = 1", "second # comment before =\n    = 2", "third")
    )
    expect_identical(signature_info("fun(# comment\n)")$names, character())
})

test_that("Signature help stays visible inside quoted arguments", {
    cases <- list(
        list("fun('<cursor>')", 0L),
        list('fun("<cursor>")', 0L),
        list("fun('unfinished<cursor>", 0L),
        list('fun("a, (nested) \\\"quoted\\\"<cursor>")', 0L),
        list("fun(1, 'a, fake(<cursor>')", 1L),
        list('fun(third = "<cursor>")', 2L),
        list('fun(r"(<cursor>)")', 0L),
        list("fun(R'--[raw, ' fake(<cursor>]--')", 0L),
        list('fun(1, r"--{raw, \" fake(<cursor>}--")', 1L),
        list('fun(third = r"(unfinished, fake(<cursor>', 2L),
        list('fun("first", "<cursor>")', 1L),
        list('fun("first", <cursor>)', 1L)
    )
    for (case in cases) {
        marked <- case[[1L]]
        col <- regexpr("<cursor>", marked, fixed = TRUE)[[1L]] - 1L
        call <- sub("<cursor>", "", marked, fixed = TRUE)
        fixture <- provider_fixture(c("fun <- function(first, second, third) NULL", call))
        fixture$workspace$get_signature <- function(...) "fun(first, second, third)"
        fixture$workspace$get_documentation <- function(...) NULL
        result <- signature_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = 1L, col = col))$result
        expect_equal(length(result$signatures), 1L, info = marked)
        if (length(result$signatures)) {
            expect_identical(result$signatures[[1L]]$label, "fun(first, second, third)", info = marked)
            expect_identical(result$activeParameter, case[[2L]], info = marked)
        }
    }

    # A call-shaped fragment inside a string is not an actual call.
    for (marked in c("'fun(<cursor>)'", 'r"(fun(<cursor>))"')) {
        col <- regexpr("<cursor>", marked, fixed = TRUE)[[1L]] - 1L
        fixture <- provider_fixture(c("fun <- function(first) NULL",
            sub("<cursor>", "", marked, fixed = TRUE)))
        result <- signature_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = 1L, col = col))$result
        expect_equal(length(result$signatures), 0L, info = marked)
    }
})

test_that("Quoted arguments retain pipe and nullable parameter matching", {
    capabilities <- list(textDocument = list(signatureHelp = list(
        signatureInformation = list(noActiveParameterSupport = TRUE))))
    cases <- list(
        list('fun(second = 1, r"(<cursor>)")', 0L),
        list('fun(unknown = "<cursor>")', NULL)
    )
    if (getRversion() >= "4.1.0") {
        cases <- c(cases, list(list('value |> fun("<cursor>")', 1L)))
    }
    for (case in cases) {
        marked <- case[[1L]]
        col <- regexpr("<cursor>", marked, fixed = TRUE)[[1L]] - 1L
        fixture <- provider_fixture(c("fun <- function(first, second, third) NULL",
            sub("<cursor>", "", marked, fixed = TRUE)))
        result <- signature_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = 1L, col = col), capabilities)$result
        expect_length(result$signatures, 1L)
        expect_identical(result$activeParameter, case[[2L]])
    }
})

test_that("Signature help follows multiline strings and ignores calls in their text", {
    cases <- list(
        list(c('fun(1, "first line', 'fake(<cursor>")'), 1L),
        list(c("fun(third = 'first line", "fake(<cursor>')"), 2L),
        list(c('fun(1, r"--[first line', 'fake("<cursor>]--")'), 1L),
        list(c('fun("first', 'second", "third', 'fake(<cursor>")'), 1L),
        list(c('fun(1, r"(first', '<cursor>last)'), 1L)
    )
    for (case in cases) {
        marked <- case[[1L]]
        row <- which(grepl("<cursor>", marked, fixed = TRUE))
        col <- regexpr("<cursor>", marked[[row]], fixed = TRUE)[[1L]] - 1L
        marked[[row]] <- sub("<cursor>", "", marked[[row]], fixed = TRUE)
        fixture <- provider_fixture(c("fun <- function(first, second, third) NULL", marked))
        fixture$workspace$get_signature <- function(...) "fun(first, second, third)"
        fixture$workspace$get_documentation <- function(...) NULL
        result <- signature_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = row, col = col))$result
        expect_equal(length(result$signatures), 1L, info = paste(marked, collapse = "\n"))
        if (length(result$signatures)) {
            expect_identical(result$signatures[[1L]]$label, "fun(first, second, third)")
            expect_identical(result$activeParameter, case[[2L]])
        }
    }

    for (marked in list(c('"first line', 'fun(\'<cursor>\')"'),
            c('r"(first line', 'fun(\'<cursor>\'))"'))) {
        col <- regexpr("<cursor>", marked[[2L]], fixed = TRUE)[[1L]] - 1L
        marked[[2L]] <- sub("<cursor>", "", marked[[2L]], fixed = TRUE)
        fixture <- provider_fixture(c("fun <- function(first) NULL", marked))
        result <- signature_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = 2L, col = col))$result
        expect_length(result$signatures, 0L)
    }
})

test_that("Signature call scanning follows edits and R Markdown regions", {
    document <- Document$new("file:///signature-cache.R", version = 1L,
        content = c('outer("first', 'fake('))
    point <- list(row = 1L, col = 5L)
    expect_identical(document$detect_call(point, signature = TRUE)$token, "outer")
    cache <- document$signature_call_cache
    document$set_content(2L, c('outer("first', 'fake(abc'))
    expect_identical(document$detect_call(list(row = 1L, col = 8L), signature = TRUE)$token, "outer")
    expect_identical(document$signature_call_cache, cache)
    document$set_content(3L, c('outer("first")', 'fake('))
    expect_identical(document$detect_call(point, signature = TRUE)$token, "fake")
    document$set_content(4L, c('outer("first', 'fake('))
    expect_identical(document$detect_call(point, signature = TRUE)$token, "outer")
    document$signature_call_cache <- unserialize(serialize(cache, NULL))
    expect_identical(document$detect_call(point, signature = TRUE)$token, "outer")
    document$set_content(5L, c("outer(", "), fresh("))
    expect_identical(document$detect_call(list(row = 1L, col = 9L), signature = TRUE)$token, "fresh")
    expect_identical(document$detect_call(list(row = 1L, col = 1L), signature = TRUE)$token, "")
    expect_identical(document$detect_call(list(row = 1L, col = 0L), signature = TRUE)$token, "outer")

    content <- c('"unclosed prose', '```{r}', 'get("text")', '```', 'get("text")')
    document <- Document$new("file:///signature.qmd", content = content)
    expect_identical(document$detect_call(list(row = 2L, col = 6L), signature = TRUE)$token, "get")
    expect_identical(document$detect_call(list(row = 4L, col = 6L), signature = TRUE)$token, "")
})

test_that("Installed signature help survives quoted text edits", {
    skip_on_cran()
    root <- withr::local_tempdir()
    path <- file.path(root, "signature-quotes.R")
    uri <- path_to_uri(path)
    client <- language_client(working_dir = root)
    did_open(client, path, text = "get()")
    result <- respond_signature(client, path, c(0L, 4L),
        retry_when = function(result) !length(result$signatures))
    expect_match(result$signatures[[1L]]$label, "^get\\(x,")

    notify(client, "textDocument/didChange", list(
        textDocument = list(uri = uri, version = 2L),
        contentChanges = list(list(range = range(position(0L, 4L), position(0L, 4L)), text = "''"))
    ))
    result <- respond_signature(client, path, c(0L, 5L), retry = FALSE)
    expect_match(result$signatures[[1L]]$label, "^get\\(x,")
    expect_identical(result$activeParameter, 0L)

    inserted <- paste0(intToUtf8(0x1f600), ", fake(")
    notify(client, "textDocument/didChange", list(
        textDocument = list(uri = uri, version = 3L),
        contentChanges = list(list(range = range(position(0L, 5L), position(0L, 5L)), text = inserted))
    ))
    result <- respond_signature(client, path, c(0L, 5L + ncodeunit(inserted)), retry = FALSE)
    expect_match(result$signatures[[1L]]$label, "^get\\(x,")
    expect_identical(result$activeParameter, 0L)

    raw_lines <- c('get(x = r"--[first line', 'fake(" text]--")')
    notify(client, "textDocument/didChange", list(
        textDocument = list(uri = uri, version = 4L),
        contentChanges = list(list(text = paste(raw_lines, collapse = "\n")))
    ))
    result <- respond_signature(client, path, c(1L, 6L), retry = FALSE)
    expect_match(result$signatures[[1L]]$label, "^get\\(x,")
    expect_identical(result$activeParameter, 0L)
})

test_that("Signature reply resolves local documented functions", {
    content <- c(
        "#' Add values",
        "#' @param first First value.",
        "#' @param second Second value.",
        "add_values <- function(first, second = list(1, 2)) first + second",
        "add_values(first = 1, second = 2)"
    )
    fixture <- provider_fixture(content)
    reply <- signature_reply(
        1L, fixture$uri, fixture$workspace, fixture$document,
        list(row = 4L, col = nchar(content[[5L]]) - 2L)
    )

    expect_length(reply$result$signatures, 1L)
    expect_match(reply$result$signatures[[1L]]$label, "add_values\\(first")
    expect_match(
        reply$result$signatures[[1L]]$documentation$value,
        "Add values"
    )
    expect_equal(reply$result$activeSignature, 0L)
    expect_equal(reply$result$activeParameter, 1L)

    commented_call <- c(
        content[1:4],
        "(",
        "  add_values # trailing comment before (",
        "    # full-line comment before (",
        "    (first = 1, second = 2)",
        ")"
    )
    commented_fixture <- provider_fixture(commented_call)
    commented_reply <- signature_reply(
        1L, commented_fixture$uri, commented_fixture$workspace,
        commented_fixture$document,
        list(row = 7L, col = nchar(commented_call[[8L]]) - 2L)
    )
    expect_length(commented_reply$result$signatures, 1L)
    expect_match(commented_reply$result$signatures[[1L]]$label, "add_values\\(first")
    expect_equal(commented_reply$result$activeParameter, 1L)
})

test_that("Signature reply falls back to workspace metadata", {
    uri <- "file:///external-signature.R"
    document <- Document$new(uri, content = "external(value = ")
    workspace <- list(
        get_parse_data = function(...) list(xml_doc = NULL),
        get_signature = function(symbol, package, exported_only) {
            expect_equal(symbol, "external")
            expect_true(exported_only)
            "external(value, ..., option = TRUE)"
        },
        get_documentation = function(...) list(
            description = "An external function."
        )
    )
    reply <- signature_reply(
        1L, uri, workspace, document,
        list(row = 0L, col = nchar(document$content[[1L]]))
    )

    expect_length(reply$result$signatures, 1L)
    expect_equal(
        reply$result$signatures[[1L]]$documentation$value,
        "An external function."
    )
    expect_equal(reply$result$activeParameter, 0L)
})
