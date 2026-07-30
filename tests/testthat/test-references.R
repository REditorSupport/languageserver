test_that("reference index distinguishes lexical and package scopes", {
    fixture <- provider_fixture(c(
        "value <- 1",
        "fun <- function(value) value + 1",
        "value + pkg::value()"
    ))
    index <- fixture$document$parse_data$reference_index

    value_keys <- index$definition_key[index$name == "value"]
    expect_true("global:value" %in% value_keys)
    expect_true(any(startsWith(value_keys, "local:")))
    expect_true("package:pkg:value" %in% value_keys)
})

test_that("Find References works for functions in files", {
    skip_on_cran()
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    defn2_file <- withr::local_tempfile(fileext = ".R")
    query_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c("my_fn <- function(x) {", "  x + 1", "}"), defn_file)
    writeLines(c("my_fn"), query_file)

    client %>% did_open(defn_file)
    client %>% did_open(query_file)

    # query at the beginning of token
    result <- client %>% respond_references(
        query_file, c(0, 0), retry_when = function(result) length(result) < 2)
    expect_length(result, 2)

    result1 <- result %>% keep(~ .$uri == path_to_uri(defn_file))
    expect_length(result1, 1)
    expect_equal(result1[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result1[[1]]$range$end, list(line = 0, character = 5))

    result2 <- result %>% keep(~ .$uri == path_to_uri(query_file))
    expect_length(result2, 1)
    expect_equal(result2[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result2[[1]]$range$end, list(line = 0, character = 5))

    # query in the middle of token
    result <- client %>% respond_references(query_file, c(0, 3))
    expect_length(result, 2)

    result1 <- result %>% keep(~ .$uri == path_to_uri(defn_file))
    expect_length(result1, 1)
    expect_equal(result1[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result1[[1]]$range$end, list(line = 0, character = 5))

    result2 <- result %>% keep(~ .$uri == path_to_uri(query_file))
    expect_length(result2, 1)
    expect_equal(result2[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result2[[1]]$range$end, list(line = 0, character = 5))

    # query at the end of token
    result <- client %>% respond_references(query_file, c(0, 5))
    expect_length(result, 2)

    result1 <- result %>% keep(~ .$uri == path_to_uri(defn_file))
    expect_length(result1, 1)
    expect_equal(result1[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result1[[1]]$range$end, list(line = 0, character = 5))

    result2 <- result %>% keep(~ .$uri == path_to_uri(query_file))
    expect_length(result2, 1)
    expect_equal(result2[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result2[[1]]$range$end, list(line = 0, character = 5))

    # remove definition
    writeLines("", defn_file)
    client %>% did_open(defn_file)

    result <- client %>% respond_references(query_file, c(0, 0),
        retry_when = function(result) {
            length(result) > 0
        })

    expect_length(result, 0)

    # move function into different file
    writeLines(c("my_fn <- function(x) {", "  x + 1", "}"), defn2_file)
    client %>% did_open(defn2_file)

    result <- client %>% respond_references(query_file, c(0, 0))
    expect_length(result, 2)

    result1 <- result %>% keep(~ .$uri == path_to_uri(defn2_file))
    expect_length(result1, 1)
    expect_equal(result1[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result1[[1]]$range$end, list(line = 0, character = 5))

    result2 <- result %>% keep(~ .$uri == path_to_uri(query_file))
    expect_length(result2, 1)
    expect_equal(result2[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result2[[1]]$range$end, list(line = 0, character = 5))
})

test_that("Find References works in single file", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c("my_fn <- function(x) {x + 1}", "my_fn", ".nonexistent"),
        single_file)

    client %>% did_open(single_file)

    # first query a known function to make sure the file is processed
    result <- client %>% respond_references(
        single_file, c(1, 0), , retry_when = function(result) length(result) < 2)
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result[[1]]$range$end, list(line = 0, character = 5))

    expect_equal(result[[2]]$range$start, list(line = 1, character = 0))
    expect_equal(result[[2]]$range$end, list(line = 1, character = 5))

    # then query the missing function. The file is processed, don't need to retry
    result <- client %>% respond_references(single_file, c(2, 0), retry = FALSE)

    expect_length(result, 0)
})

test_that("Find References works in scope with different assignment operators", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "my_fn <- function(var1) {",
        "  var2 <- 1",
        "  var3 = 2",
        "  3 -> var4",
        "  for (var5 in 1:10) {",
        "    var1 + var2 + var3 + var4 + var5",
        "  }",
        "}",
        "my_fn(1)"
    ), single_file)

    client %>% did_open(single_file)

    # first query a known function to make sure the file is processed
    result <- client %>% respond_references(
        single_file, c(8, 0), retry_when = function(result) length(result) < 2)
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result[[1]]$range$end, list(line = 0, character = 5))

    expect_equal(result[[2]]$range$start, list(line = 8, character = 0))
    expect_equal(result[[2]]$range$end, list(line = 8, character = 5))

    result <- client %>% respond_references(single_file, c(5, 5))
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 0, character = 18))
    expect_equal(result[[1]]$range$end, list(line = 0, character = 22))

    expect_equal(result[[2]]$range$start, list(line = 5, character = 4))
    expect_equal(result[[2]]$range$end, list(line = 5, character = 8))

    result <- client %>% respond_references(single_file, c(5, 12))
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 1, character = 2))
    expect_equal(result[[1]]$range$end, list(line = 1, character = 6))

    expect_equal(result[[2]]$range$start, list(line = 5, character = 11))
    expect_equal(result[[2]]$range$end, list(line = 5, character = 15))

    result <- client %>% respond_references(single_file, c(5, 20))
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 2, character = 2))
    expect_equal(result[[1]]$range$end, list(line = 2, character = 6))

    expect_equal(result[[2]]$range$start, list(line = 5, character = 18))
    expect_equal(result[[2]]$range$end, list(line = 5, character = 22))


    result <- client %>% respond_references(single_file, c(5, 26))
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 3, character = 7))
    expect_equal(result[[1]]$range$end, list(line = 3, character = 11))

    expect_equal(result[[2]]$range$start, list(line = 5, character = 25))
    expect_equal(result[[2]]$range$end, list(line = 5, character = 29))


    result <- client %>% respond_references(single_file, c(5, 34))
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 4, character = 7))
    expect_equal(result[[1]]$range$end, list(line = 4, character = 11))

    expect_equal(result[[2]]$range$start, list(line = 5, character = 32))
    expect_equal(result[[2]]$range$end, list(line = 5, character = 36))
})

test_that("Find References in Rmarkdown works", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".Rmd")
    writeLines(
        c(
            "```{r}",
            "my_fn <- function(x) {x + 1}",
            "```",
            "",
            "```{r}",
            "my_fn",
            ".nonexistent",
            "```"
        ),
        single_file
    )

    client %>% did_open(single_file)

    # first query a known function to make sure the file is processed
    result <- client %>% respond_references(single_file, c(5, 0))
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 1, character = 0))
    expect_equal(result[[1]]$range$end, list(line = 1, character = 5))

    expect_equal(result[[2]]$range$start, list(line = 5, character = 0))
    expect_equal(result[[2]]$range$end, list(line = 5, character = 5))

    # then query the missing function. The file is processed, don't need to retry
    result <- client %>% respond_references(single_file, c(6, 0), retry = FALSE)
    expect_length(result, 0)
})

test_that("Reference index excludes members and resolves qualified calls", {
    uri <- "file:///reference-index.R"
    content <- c(
        "outer <- function(argument) {",
        "  local <- argument",
        "  object$member",
        "  base::mean(local)",
        "}"
    )
    parsed <- parse_document(uri, content)
    index <- parsed$reference_index

    expect_false("member" %in% index$name)
    mean_index <- which(index$name == "mean")
    expect_length(mean_index, 1L)
    expect_true(index$qualified_call[[mean_index]])
    expect_equal(index$call_package[[mean_index]], "base")
    expect_equal(index$definition_key[[mean_index]], "package:base:mean")

    local_index <- which(index$name == "local")
    expect_true(length(local_index) >= 2L)
    expect_true(all(startsWith(index$definition_key[local_index], "local:")))

    expect_null(reference_key_at(NULL, list(row = 0L, col = 0L), "x"))
    expect_null(reference_key_at(index, list(row = 99L, col = 0L), "x"))
})

test_that("References fall back to XML when no occurrence index exists", {
    content <- c(
        "target <- function() 1",
        "caller <- function() { target(); target() }"
    )
    uri <- "file:///legacy-references.R"
    document <- Document$new(uri, version = 1L, content = content)
    parse_data <- parse_document(uri, content)
    parse_data$xml_doc <- xml2::read_xml(parse_data$xml_data)
    parse_data$reference_index <- NULL
    document$update_parse_data(parse_data)

    documents <- collections::dict()
    documents$set(uri, document)
    workspace <- new.env(parent = baseenv())
    workspace$documents <- documents
    workspace$get_parse_data <- function(...) parse_data
    workspace$get_definition <- function(...) NULL

    reply <- references_reply(
        1L, uri, workspace, document, list(row = 0L, col = 1L)
    )

    expect_length(reply$result, 3L)
    expect_true(all(vapply(reply$result, function(item) item$uri == uri,
        logical(1L))))
    expect_equal(
        map_int(reply$result, c("range", "start", "line")),
        c(0L, 1L, 1L)
    )
    expect_equal(
        map_int(reply$result, c("range", "start", "character")),
        c(0L, 23L, 33L)
    )
})
