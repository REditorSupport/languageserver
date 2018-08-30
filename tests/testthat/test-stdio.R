exec <- if (.Platform$OS.type == "windows") "Rterm" else "R"
bin <- file.path(R.home("bin"), exec)

wd <- system.file("projects", "mypackage", package = "languageserver")

# languageserver:::logger$debug_mode(TRUE)

client <- languageserver:::LanguageClient$new(
    bin, c("--slave", "-e", "languageserver::run()"))

client$start(working_dir = wd)
data <- client$fetch(blocking = TRUE)
client$handle_raw(data)

context("Test STDIO connection")

test_that("initialize", {
    expect_false(is.null(client$ServerCapabilities))
})
