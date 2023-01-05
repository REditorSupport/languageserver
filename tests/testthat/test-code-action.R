test_that("Code action works", {
  skip_on_cran()

  dir <- tempdir()
  client <- language_client(working_dir = dir, diagnostics = TRUE)

  temp_file <- withr::local_tempfile(tmpdir = dir, fileext = ".R")
  writeLines(c(
    "1+1",
    "my_fun <- function(x) {",
    "  x+1",
    "}"
  ), temp_file)

  client %>% did_open(temp_file)
  data <- client %>% wait_for("textDocument/publishDiagnostics")

  expect_equal(client$diagnostics$size(), 1)
  expect_equal(client$diagnostics$get(data$uri), data$diagnostics)
  expect_equal(data$diagnostics[[1]]$source, "infix_spaces_linter")
  expect_equal(data$diagnostics[[1]]$message, "Put spaces around all infix operators.")
  expect_equal(data$diagnostics[[2]]$source, "infix_spaces_linter")
  expect_equal(data$diagnostics[[2]]$message, "Put spaces around all infix operators.")

  result <- client %>% respond_code_action(temp_file, c(0, 0), c(0, 1))
  expect_length(result, 2)
  expect_length(result %>% keep(~ .$title == "Disable all linters for this line"), 1)
  expect_length(result %>% keep(~ .$title == "Disable infix_spaces_linter for this line"), 1)

  result <- client %>% respond_code_action(temp_file, c(1, 0), c(1, 5), retry = FALSE)
  expect_length(result, 0)

  result <- client %>% respond_code_action(temp_file, c(2, 3), c(2, 4))
  expect_length(result, 2)
  expect_length(result %>% keep(~ .$title == "Disable all linters for this line"), 1)
  expect_length(result %>% keep(~ .$title == "Disable infix_spaces_linter for this line"), 1)

  result <- client %>% respond_code_action(temp_file, c(0, 0), c(3, 0))
  expect_length(result, 2)
  expect_length(result %>% keep(~ .$title == "Disable all linters for this line"), 1)
  expect_length(result %>% keep(~ .$title == "Disable infix_spaces_linter for this line"), 1)
})
