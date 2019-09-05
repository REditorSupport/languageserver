context("Test Symbol")

test_that("Document Symbol works", {
    skip_on_cran()
    defn_file <- tempfile()
    writeLines(c("f <- function(x) {", "  x + 1", "}",
                 "g <- function(x) { x - 1 }"), defn_file)
    expected <- list(
        f = list(range = list(start = list(line = 0, character = 0),
                              end   = list(line = 2, character = 0))),
        g = list(range = list(start = list(line = 3, character = 0),
                              end   = list(line = 3, character = 25))))
    exec <- if (.Platform$OS.type == "windows") "Rterm" else "R"
    bin <- file.path(R.home("bin"), exec)
    client <- languageserver:::LanguageClient$new(
        bin, c("--slave", "-e", "languageserver::run()"))
    client$start()
    client$fetch(blocking = TRUE)
    client$deliver(Notification$new("textDocument/didSave", list(textDocument = list(uri = path_to_uri(defn_file)))))
    data <- request_with_timeout(function() client$deliver(client$request("textDocument/documentSymbol",
        list(textDocument = list(uri = path_to_uri(defn_file))))), client)
    # functions may not be returned in order they appear in file
    for(r in data$result) {
        funct <- r$name
        expect_equal(r$location$range, expected[[funct]]$range)
    }
    # clean up
    file.remove(defn_file)
    client$stop()
})

test_that("Workspace Symbol works", {
    skip_on_cran()
    defn_file <- tempfile()
    defn2_file <- tempfile()
    writeLines(c("f1 <- function(x) {", "  x + 1", "}",
                 "g <- function(x) { x - 1 }"), defn_file)
    writeLines(c("f2 <- function(x) {", "  x + 1", "}"), defn2_file)
    expected_names <- c("f1", "f2")
    exec <- if (.Platform$OS.type == "windows") "Rterm" else "R"
    bin <- file.path(R.home("bin"), exec)
    client <- languageserver:::LanguageClient$new(
        bin, c("--slave", "-e", "languageserver::run()"))
    client$start()
    client$fetch(blocking = TRUE)
    client$deliver(Notification$new("textDocument/didSave", list(textDocument = list(uri = path_to_uri(defn_file)))))
    client$deliver(Notification$new("textDocument/didSave", list(textDocument = list(uri = path_to_uri(defn2_file)))))
    Sys.sleep(1) # time to process both files
    data <- request_with_timeout(function() client$deliver(client$request("workspace/symbol",
        list(query = "f"))), client)
    result_names <- sapply(data$result, function(x) x$name)
    expect_equal(length(result_names), length(expected_names))
    expect_true(all(result_names %in% expected_names))
    # clean up
    file.remove(defn_file)
    file.remove(defn2_file)
    client$stop()
})
