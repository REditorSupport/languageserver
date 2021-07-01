test_that("Null-root workspace works", {
  skip_on_cran()
  client <- language_client(NULL)

  temp_file <- withr::local_tempfile(fileext = ".R")
  writeLines(
    c(
      "file.path("
    ),
    temp_file)

  client %>% did_save(temp_file)

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
  Sys.sleep(0.5)

  result <- client %>% respond_signature(file1, c(3, 5))
  expect_length(result$signatures, 1)
  expect_match(result$signatures[[1]]$label, "fun1\\(x\\)")

  result <- client %>% respond_signature(file1, c(1, 7), retry = FALSE)
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

  Sys.sleep(0.5)
  result <- client %>% respond_signature(file1, c(1, 7))
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

  Sys.sleep(0.5)
  result <- client %>% respond_signature(file1, c(1, 7))
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

  Sys.sleep(0.5)
  result <- client %>% respond_signature(file1, c(1, 7))
  expect_length(result$signatures, 0)
})
