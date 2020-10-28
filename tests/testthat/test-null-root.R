context("Test Null-root Workspace")

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
