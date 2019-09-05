context("Test Definition")

test_that("DefinitionCache works", {
    dc <- DefinitionCache$new()
    range1 <- range(position(1, 2), position(3, 4))
    range2 <- range(position(5, 6), position(7, 8))
    ranges <- list(defn1 = range1, defn2 = range2)
    dc$update("uri1", ranges)
    expect_equal(dc$get("defn2")$uri, "uri1")
    # defn2 disappears from uri1
    ranges_updated <- list(defn1 = range1)
    dc$update("uri1", ranges_updated)
    expect_null(dc$get("defn2"))
    # defn1 now appears in uri2
    dc$update("uri2", ranges_updated)
    expect_equal(dc$get("defn1")$uri, "uri2")
})

test_that("DefinitionCache works when multiple functions are removed", {
    skip_on_cran()
    dc <- DefinitionCache$new()
    range1 <- range(position(1, 2), position(3, 4))
    range2 <- range(position(5, 6), position(7, 8))
    ranges <- list(defn1 = range1, defn2 = range2)
    dc$update("uri1", ranges)
    expect_equal(dc$get("defn1")$uri, "uri1")
    expect_equal(dc$get("defn2")$uri, "uri1")
    # defn1, defn2 disappear from uri1
    ranges_updated <- list()
    dc$update("uri1", ranges_updated)
    expect_null(dc$get("defn1"))
    expect_null(dc$get("defn2"))
})

test_that("DefinitionCache gets all definitions in document", {
    dc <- DefinitionCache$new()
    range1 <- range(position(1, 2), position(3, 4))
    range2 <- range(position(5, 6), position(7, 8))
    ranges <- list(defn1 = range1, defn2 = range2)
    dc$update("uri1", ranges)
    result <- dc$get_functs_for_uri("uri1")
    expect_equal(names(result), c("defn1", "defn2"))
})

test_that("DefinitionCache filter works", {
    dc <- DefinitionCache$new()
    range1 <- range(position(1, 2), position(3, 4))
    range2 <- range(position(5, 6), position(7, 8))
    range3 <- range(position(9, 10), position(11, 12))
    range4 <- range(position(13, 14), position(15, 16))
    ranges1 <- list(abc = range1, acb = range2)
    ranges2 <- list(adbc = range1, ac = range2)
    dc$update("uri1", ranges1)
    dc$update("uri2", ranges2)
    result <- dc$filter("abc")
    expect_true("abc" %in% names(result))
    expect_true("adbc" %in% names(result))
    expect_true(!("acb" %in% names(result)))
    expect_true(!("ac" %in% names(result)))
})

test_that("Go to Definition works for functions in files", {
    skip_on_cran()
    defn_file <- tempfile()
    defn2_file <- tempfile()
    query_file <- tempfile()
    writeLines(c("my_fn <- function(x) {", "  x + 1", "}"), defn_file)
    writeLines(c("my_fn"), query_file)
    exec <- if (.Platform$OS.type == "windows") "Rterm" else "R"
    bin <- file.path(R.home("bin"), exec)
    client <- languageserver:::LanguageClient$new(
        bin, c("--slave", "-e", "languageserver::run()"))
    client$start()
    client$fetch(blocking = TRUE)
    client$deliver(Notification$new("textDocument/didSave", list(textDocument = list(uri = path_to_uri(defn_file)))))
    client$deliver(Notification$new("textDocument/didSave", list(textDocument = list(uri = path_to_uri(query_file)))))
    data <- request_with_timeout(function() client$deliver(client$request("textDocument/definition",
        list(textDocument = list(uri = path_to_uri(query_file)), position = list(line = 0, character = 0)))),
        client)
    expect_equal(data$result$range$start, list(line = 0, character = 0))
    expect_equal(data$result$range$end, list(line = 2, character = 0))
    # move function into different file
    writeLines("", defn_file)
    writeLines(c("my_fn <- function(x) {", "  x + 1", "}"), defn2_file)
    client$deliver(Notification$new("textDocument/didSave", list(textDocument = list(uri = path_to_uri(defn_file)))))
    client$deliver(Notification$new("textDocument/didSave", list(textDocument = list(uri = path_to_uri(defn2_file)))))
    data <- request_with_timeout(function() client$deliver(client$request("textDocument/definition",
        list(textDocument = list(uri = path_to_uri(query_file)), position = list(line = 0, character = 0)))),
        client,
        condition = function(x) x$result$uri == path_to_uri(defn_file))
    expect_equal(data$result$uri, path_to_uri(defn2_file))
    # clean up
    file.remove(defn_file)
    file.remove(defn2_file)
    file.remove(query_file)
    client$stop()
})

test_that("Go to Definition works for functions in packages", {
    skip_on_cran()
    query_file <- tempfile()
    writeLines(c("print"), query_file)
    exec <- if (.Platform$OS.type == "windows") "Rterm" else "R"
    bin <- file.path(R.home("bin"), exec)
    client <- languageserver:::LanguageClient$new(
        bin, c("--slave", "-e", "languageserver::run()"))
    client$start()
    client$fetch(blocking = TRUE)
    client$deliver(Notification$new("textDocument/didSave", list(textDocument = list(uri = path_to_uri(query_file)))))
    data <- request_with_timeout(function() client$deliver(client$request("textDocument/definition",
        list(textDocument = list(uri = path_to_uri(query_file)), position = list(line = 0, character = 0)))),
        client)
    line <- readLines(path_from_uri(data$result$uri), n = 1)
    expect_true(startsWith(line, c("print <- function")))
    # clean up
    file.remove(query_file)
    client$stop()
})

test_that("Go to Definition works for missing functions", {
    skip_on_cran()
    query_file <- tempfile()
    writeLines(c("_.nonexistent._.function"), query_file)
    exec <- if (.Platform$OS.type == "windows") "Rterm" else "R"
    bin <- file.path(R.home("bin"), exec)
    client <- languageserver:::LanguageClient$new(
        bin, c("--slave", "-e", "languageserver::run()"))
    client$start()
    client$fetch(blocking = TRUE)
    client$deliver(Notification$new("textDocument/didSave", list(textDocument = list(uri = path_to_uri(query_file)))))
    data <- request_with_timeout(function() client$deliver(client$request("textDocument/definition",
        list(textDocument = list(uri = path_to_uri(query_file)), position = list(line = 0, character = 0)))),
        client, timeout_seconds = 2) # will run full duration of timeout_seconds
    expect_equal(length(data$result), 0)
    # clean up
    file.remove(query_file)
    client$stop()
})

test_that("Go to Definition works when package is specified", {
    skip_on_cran()
    # When there is a user-defined function with the same name
    # as a package function, but the package is specified,
    # test that the package version is used.
    defn_file <- tempfile()
    query_file <- tempfile()
    writeLines(c("print <- function(x) {", "# Not base::print", "}"), defn_file)
    writeLines(c("base::print"), query_file)
    exec <- if (.Platform$OS.type == "windows") "Rterm" else "R"
    bin <- file.path(R.home("bin"), exec)
    client <- languageserver:::LanguageClient$new(
        bin, c("--slave", "-e", "languageserver::run()"))
    client$start()
    client$fetch(blocking = TRUE)
    client$deliver(Notification$new("textDocument/didSave", list(textDocument = list(uri = path_to_uri(defn_file)))))
    client$deliver(Notification$new("textDocument/didSave", list(textDocument = list(uri = path_to_uri(query_file)))))
    Sys.sleep(1) # delay to let languageserver process notifications
    data <- request_with_timeout(function() client$deliver(client$request("textDocument/definition",
        list(textDocument = list(uri = path_to_uri(query_file)), position = list(line = 0, character = 7)))),
        client)
    # should find base::print definition, not the one in defn_file.d
    expect_true(data$result$uri != path_to_uri(defn_file))
    # clean up
    file.remove(defn_file)
    file.remove(query_file)
    client$stop()
})
