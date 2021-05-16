test_that("Selection range works with single position", {
  skip_on_cran()
  client <- language_client()

  defn_file <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "f <- function(var) {",
    "  var + 1",
    "}"
  ), defn_file)

  client %>% did_save(defn_file)
  result <- client %>% respond_selection_range(defn_file, list(
    list(
      line = 1,
      character = 4
    )
  ))

  expect_equal(length(result), 1)
  expect_equal(result, list(list(range = list(
    start = list(line = 1L, character = 2L),
    end = list(line = 1L, character = 5L)
  ), parent = list(range = list(
    start = list(line = 1L, character = 2L), end = list(
      line = 1L,
      character = 5L
    )
  ), parent = list(
    range = list(start = list(
      line = 1L, character = 2L
    ), end = list(line = 1L, character = 9L)),
    parent = list(range = list(
      start = list(line = 0L, character = 19L),
      end = list(line = 2L, character = 1L)
    ), parent = list(
      range = list(
        start = list(line = 0L, character = 5L),
        end = list(line = 2L, character = 1L)
      ), parent = list(
        range = list(
          start = list(line = 0L, character = 0L),
          end = list(line = 2L, character = 1L)
        ), parent = NULL
      )
    ))
  )))))
})


test_that("Selection range works with multiple positions", {
  skip_on_cran()
  client <- language_client()

  defn_file <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "f <- function(var) {",
    "  var + 1",
    "}"
  ), defn_file)

  client %>% did_save(defn_file)
  result <- client %>% respond_selection_range(defn_file, list(
    list(
      line = 0,
      character = 16
    ),
    list(
      line = 1,
      character = 4
    )
  ))

  expect_equal(length(result), 2)
  expect_equal(result, list(list(range = list(
    start = list(line = 0L, character = 14L),
    end = list(line = 0L, character = 17L)
  ), parent = list(range = list(
    start = list(line = 0L, character = 5L), end = list(
      line = 2L,
      character = 1L
    )
  ), parent = list(
    range = list(start = list(
      line = 0L, character = 0L
    ), end = list(line = 2L, character = 1L)),
    parent = NULL
  ))), list(
    range = list(start = list(
      line = 1L,
      character = 2L
    ), end = list(line = 1L, character = 5L)),
    parent = list(range = list(
      start = list(line = 1L, character = 2L),
      end = list(line = 1L, character = 5L)
    ), parent = list(
      range = list(
        start = list(line = 1L, character = 2L),
        end = list(line = 1L, character = 9L)
      ), parent = list(
        range = list(
          start = list(line = 0L, character = 19L),
          end = list(line = 2L, character = 1L)
        ), parent = list(
          range = list(
            start = list(line = 0L, character = 5L),
            end = list(line = 2L, character = 1L)
          ), parent = list(
            range = list(
              start = list(line = 0L, character = 0L),
              end = list(line = 2L, character = 1L)
            ), parent = NULL
          )
        )
      )
    ))
  )))
})

test_that("Selection range works in Rmarkdown", {
  skip_on_cran()
  client <- language_client()

  defn_file <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c(
    "## section1",
    "Some text here",
    "### subsection1",
    "```{r}",
    "f <- function(var) {",
    "  var + 1",
    "}",
    "# title",
    "# description",
    "g <- function(x) {",
    "  x - 1",
    "}",
    "```"
  ), defn_file)

  client %>% did_save(defn_file)

  result <- client %>% respond_selection_range(defn_file, list(
    list(
      line = 5,
      character = 4
    )
  ))

  expect_equal(length(result), 1)
  expect_equal(result, list(list(range = list(
    start = list(line = 5L, character = 2L),
    end = list(line = 5L, character = 5L)
  ), parent = list(range = list(
    start = list(line = 5L, character = 2L), end = list(
      line = 5L,
      character = 5L
    )
  ), parent = list(
    range = list(start = list(
      line = 5L, character = 2L
    ), end = list(line = 5L, character = 9L)),
    parent = list(range = list(
      start = list(line = 4L, character = 19L),
      end = list(line = 6L, character = 1L)
    ), parent = list(
      range = list(
        start = list(line = 4L, character = 5L),
        end = list(line = 6L, character = 1L)
      ), parent = list(
        range = list(
          start = list(line = 4L, character = 0L),
          end = list(line = 6L, character = 1L)
        ), parent = NULL
      )
    ))
  )))))
})
