test_that("Simple signature works", {
    skip_on_cran()
    client <- language_client()

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
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    temp_file <- withr::local_tempfile(fileext = ".R")

    writeLines(c("foo <- function(x, y = 3) { x + y }"), defn_file)
    writeLines(c("foo(3, "), temp_file)

    client %>% did_open(defn_file) %>% did_open(temp_file)

    result <- client %>% respond_signature(
        temp_file, c(0, 7),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_length(result$signatures, 1)
    expect_equal(result$signatures[[1]]$label, "foo(x, y = 3)")
})

test_that("Signature of user function works with semi-colon", {
    skip_on_cran()
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    temp_file <- withr::local_tempfile(fileext = ".R")

    writeLines(c("foo <- function(x, y = 3) { x + y };"), defn_file)
    writeLines(c("foo(3, "), temp_file)

    client %>% did_open(defn_file) %>% did_open(temp_file)

    result <- client %>% respond_signature(
        temp_file, c(0, 7),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_length(result$signatures, 1)
    expect_equal(result$signatures[[1]]$label, "foo(x, y = 3)")
})

test_that("Signature in Rmarkdown works", {
    skip_on_cran()
    client <- language_client()

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
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    temp_file <- withr::local_tempfile(fileext = ".R")

    writeLines(c("foo <- function(a, b, c, d) { a + b + c + d }"), defn_file)
    writeLines(c("foo(1, "), temp_file)

    client %>% did_open(defn_file) %>% did_open(temp_file)

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
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    temp_file <- withr::local_tempfile(fileext = ".R")

    writeLines(c("foo <- function(a, b, c) { a + b + c }"), defn_file)
    writeLines(c("foo(1, foo(2, 3), "), temp_file)

    client %>% did_open(defn_file) %>% did_open(temp_file)

    # Test at outer function's third parameter
    result <- client %>% respond_signature(
        temp_file, c(0, 18),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 2)
})

test_that("activeParameter correctly handles named arguments", {
    skip_on_cran()
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    temp_file <- withr::local_tempfile(fileext = ".R")

    writeLines(c("fun <- function(x, y = 1, z = 2) { x + y + z }"), defn_file)
    
    # Test 1: Named argument z= should activate parameter z (index 2), not y
    writeLines(c("fun(1, z = "), temp_file)
    client %>% did_open(defn_file) %>% did_open(temp_file)
    
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
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    temp_file <- withr::local_tempfile(fileext = ".R")

    writeLines(c("fun <- function(a, ..., b = 1) { a + b }"), defn_file)
    
    # Test 1: Positional args after 'a' should stick to ... (index 1)
    writeLines(c("fun(1, 2, "), temp_file)
    client %>% did_open(defn_file) %>% did_open(temp_file)
    
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
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    temp_file <- withr::local_tempfile(fileext = ".R")

    # Test with ... at the end
    writeLines(c("fun2 <- function(a, b, ...) { a + b }"), defn_file)
    writeLines(c("fun2(1, 2, 3, 4, "), temp_file)
    client %>% did_open(defn_file) %>% did_open(temp_file)
    
    result <- client %>% respond_signature(
        temp_file, c(0, 17),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_equal(result$activeParameter, 2)  # Should stick to ... (index 2)
    
    # Test with ... at the beginning
    writeLines(c("fun3 <- function(..., x, y = 1) { x + y }"), defn_file)
    writeLines(c("fun3(1, 2, "), temp_file)
    client %>% did_open(defn_file) %>% did_open(temp_file)
    
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
        list('fun("a,\\\"b", ', 1L),
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
