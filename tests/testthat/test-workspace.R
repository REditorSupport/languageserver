test_that("Null workspace folder works", {
  skip_on_cran()
  client <- language_client(NULL)

  temp_file <- withr::local_tempfile(fileext = ".R")
  writeLines(
    c(
      "file.path("
    ),
    temp_file)

  client %>% did_open(temp_file)

  result <- client %>% respond_signature(temp_file, c(0, 10))
  expect_length(result$signatures, 1)
  expect_match(result$signatures[[1]]$label, "file\\.path\\(.*")
})

test_that("Changes in non-open files in package workspace are handled", {
  skip_on_cran()

  dir <- tempfile()
  dir.create(dir)

  desc_file <- file.path(dir, "DESCRIPTION")
  file.create(desc_file)

  source_dir <- file.path(dir, "R")
  dir.create(source_dir, "R")

  file1 <- file.path(source_dir, "test1.R")
  writeLines(c(
    "fun1 <- function(x) {",
    "  fun3(x)",
    "}",
    "fun1()"
  ), file1)

  client <- language_client(dir)

  result <- client %>% respond_signature(file1, c(3, 5),
    retry_when = function(result) {
      length(result$signatures) == 0
    })

  expect_length(result$signatures, 1)
  expect_match(result$signatures[[1]]$label, "fun1\\(x\\)")

  result <- client %>% respond_signature(file1, c(1, 7))
  expect_length(result$signatures, 0)

  file2 <- file.path(source_dir, "test2.R")
  writeLines(c(
    "fun3 <- function(x) {",
    "  x",
    "}"
  ), file2)

  client %>% notify(
    "workspace/didChangeWatchedFiles", list(
      changes = list(
        list(
          uri = path_to_uri(file2),
          type = FileChangeType$Created
        )
      )
    ))

  result <- client %>% respond_signature(file1, c(1, 7),
    retry_when = function(result) {
      length(result$signatures) == 0
    })
  expect_length(result$signatures, 1)
  expect_match(result$signatures[[1]]$label, "fun3\\(x\\)")

  writeLines(c(
    "fun3 <- function(z) {",
    "  z",
    "}"
  ), file2)

  client %>% notify(
    "workspace/didChangeWatchedFiles", list(
      changes = list(
        list(
          uri = path_to_uri(file2),
          type = FileChangeType$Changed
        )
      )
  ))

  result <- client %>% respond_signature(file1, c(1, 7),
    retry_when = function(result) {
      length(result$signatures) == 0
    })
  expect_length(result$signatures, 1)
  expect_match(result$signatures[[1]]$label, "fun3\\(z\\)")

  file.remove(file2)
  client %>% notify(
    "workspace/didChangeWatchedFiles", list(
      changes = list(
        list(
          uri = path_to_uri(file2),
          type = FileChangeType$Deleted
        )
      )
  ))

  result <- client %>% respond_signature(file1, c(1, 7),
    retry_when = function(result) {
      length(result$signatures) > 0
    })
  expect_length(result$signatures, 0)
})

test_that("Multiple workspace folders are loaded and handled", {
  skip_on_cran()

  dir1 <- tempfile()
  dir.create(dir1)
  file.create(file.path(dir1, "DESCRIPTION"))
  dir.create(file.path(dir1, "R"))
  writeLines(c("fun_wk1 <- function(x) { x }"), file.path(dir1, "R", "test1.R"))

  dir2 <- tempfile()
  dir.create(dir2)
  file.create(file.path(dir2, "DESCRIPTION"))
  dir.create(file.path(dir2, "R"))
  writeLines(c("fun_wk2 <- function(y) { y }"), file.path(dir2, "R", "test2.R"))

  client <- language_client(NULL, workspace_folders = c(dir1, dir2))

  # Give language server a moment to parse initially loaded workspace files
  Sys.sleep(0.5)

  # Check we can find both symbols using workspace/symbol
  result <- client %>% respond_workspace_symbol("fun_wk",
    retry_when = function(result) {
      length(result) < 2
    })
  expect_equal(length(result), 2)

  file1 <- file.path(dir1, "R", "test1.R")
  writeLines(c(
    "fun_wk1 <- function(x) { x }",
    "fun_wk2(1)",
    "fun_wk1(2)"
  ), file1)
  client %>% did_open(file1)

  # Check we can find fun_wk1 in its respective document
  result <- client %>% respond_signature(file1, c(2, 9),
    retry_when = function(result) {
      length(result$signatures) == 0
    })
  expect_length(result$signatures, 1)
  expect_match(result$signatures[[1]]$label, "fun_wk1\\(x\\)")

  # Additionally check dynamically adding a workspace folder
  dir3 <- tempfile()
  dir.create(dir3)
  file.create(file.path(dir3, "DESCRIPTION"))
  dir.create(file.path(dir3, "R"))
  writeLines(c("fun_wk3 <- function(z) { z }"), file.path(dir3, "R", "test3.R"))

  client %>% notify(
    "workspace/didChangeWorkspaceFolders", list(
      event = list(
        added = list(list(uri = path_to_uri(dir3), name = basename(dir3))),
        removed = list()
      )
    ))

  # Re-evaluate workspace symbols to see if the new one is available
  result <- client %>% respond_workspace_symbol("fun_wk",
    retry_when = function(result) {
      length(result) < 3
    })
  expect_equal(length(result), 3)
})

test_that("Fallback workspace cleans up documents when directory is added", {
  skip_on_cran()

  client <- language_client(NULL, workspace_folders = NULL)

  dir1 <- tempfile()
  dir.create(dir1)
  dir.create(file.path(dir1, "R"))
  file.create(file.path(dir1, "DESCRIPTION"))
  file1 <- file.path(dir1, "R", "test1.R")
  writeLines(c("fun_wk1 <- function(x) { x }"), file1)

  # Open the file, it goes to fallback workspace
  client %>% did_open(file1)

  # Wait for file to be parsed
  Sys.sleep(0.5)

  # Add a workspace that contains the file
  client %>% notify(
    "workspace/didChangeWorkspaceFolders", list(
      event = list(
        added = list(list(uri = path_to_uri(dir1), name = basename(dir1))),
        removed = list()
      )
    ))

  # Wait for workspace update to process
  Sys.sleep(0.5)

  # Server should have loaded the file as part of the new workspace folder, 
  # but the old reference in the fallback workspace should be deleted.
  # If we search for fun_wk1, it should only be found once (in the new workspace).
  result <- client %>% respond_workspace_symbol("fun_wk1",
    retry_when = function(result) {
      length(result) == 0
    })
  
  expect_equal(length(result), 1)
})

