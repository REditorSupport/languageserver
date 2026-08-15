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
  expect_equal(data$diagnostics[[1]]$code, "infix_spaces_linter")
  expect_equal(data$diagnostics[[1]]$message, "Put spaces around all infix operators.")
  expect_equal(data$diagnostics[[2]]$code, "infix_spaces_linter")
  expect_equal(data$diagnostics[[2]]$message, "Put spaces around all infix operators.")

  result <- client %>% respond_code_action(temp_file, c(0, 0), c(0, 1))
  expect_length(result, 3)
  expect_length(result %>% keep(~ .$title == "Add spaces around `+`"), 1)
  expect_length(result %>% keep(~ .$title == "Disable all linters for this line"), 1)
  expect_length(result %>% keep(~ .$title == "Disable infix_spaces_linter for this line"), 1)

  result <- client %>% respond_code_action(temp_file, c(1, 0), c(1, 5), retry = FALSE)
  expect_length(result, 0)

  result <- client %>% respond_code_action(temp_file, c(2, 3), c(2, 4))
  expect_length(result, 3)
  expect_length(result %>% keep(~ .$title == "Add spaces around `+`"), 1)
  expect_length(result %>% keep(~ .$title == "Disable all linters for this line"), 1)
  expect_length(result %>% keep(~ .$title == "Disable infix_spaces_linter for this line"), 1)

  result <- client %>% respond_code_action(temp_file, c(0, 0), c(3, 0))
  expect_length(result, 4)
  expect_length(result %>% keep(~ .$title == "Add spaces around `+`"), 2)
  expect_length(result %>% keep(~ .$title == "Disable all linters for these lines"), 1)
  expect_length(result %>% keep(~ .$title == "Disable infix_spaces_linter for these lines"), 1)
})

test_that("Code actions provide preferred direct fixes", {
  uri <- "file:///code-actions.R"
  document <- Document$new(uri, content = c(
    "x=1",
    "f(x,y)",
    "1 != NA",
    "T"
  ))
  diagnostic <- function(line, start, end, code, message) {
    list(
      range = range(position(line, start), position(line, end)),
      severity = DiagnosticSeverity$Information,
      source = "lintr",
      code = code,
      message = message
    )
  }
  diagnostics <- list(
    diagnostic(0, 1, 2, "assignment_linter",
      "Use one of <-, <<- for assignment, not =."),
    diagnostic(0, 1, 2, "infix_spaces_linter",
      "Put spaces around all infix operators."),
    diagnostic(1, 3, 4, "commas_linter", "Put a space after a comma."),
    diagnostic(2, 0, 7, "equals_na_linter", "Use is.na() instead of x != NA"),
    diagnostic(3, 0, 1, "T_and_F_symbol_linter", "Use TRUE instead of the symbol T.")
  )

  reply <- document_code_action_reply(
    1L, uri, NULL, document, list(),
    list(diagnostics = diagnostics, only = list("quickfix"))
  )
  titles <- vapply(reply$result, function(action) action$title, character(1L))
  expect_true(all(c(
    "Replace `=` with `<-`",
    "Add spaces around `=`",
    "Normalize spacing around `,`",
    "Replace comparison with `is.na()`",
    "Replace `T` with `TRUE`"
  ) %in% titles))

  direct <- Filter(function(action) isTRUE(action$isPreferred), reply$result)
  expect_length(direct, 5L)
  expect_true(all(vapply(direct, function(action) {
    length(action$diagnostics) >= 1L && length(action$edit$changes[[uri]]) == 1L
  }, logical(1L))))
  na_action <- reply$result[[match("Replace comparison with `is.na()`", titles)]]
  expect_equal(na_action$edit$changes[[uri]][[1L]]$newText, "!is.na(1)")
})

test_that("Fix-all resolves overlapping diagnostic edits", {
  uri <- "file:///fix-all.R"
  document <- Document$new(uri, content = "x=1")
  diagnostics <- list(
    list(
      range = range(position(0, 1), position(0, 2)),
      source = "lintr",
      code = "assignment_linter",
      message = "Use one of <-, <<- for assignment, not =."
    ),
    list(
      range = range(position(0, 1), position(0, 2)),
      source = "lintr",
      code = "infix_spaces_linter",
      message = "Put spaces around all infix operators."
    )
  )

  reply <- document_code_action_reply(
    1L, uri, NULL, document, list(),
    list(diagnostics = diagnostics, only = list("source.fixAll"))
  )

  expect_length(reply$result, 1L)
  expect_equal(reply$result[[1L]]$kind, "source.fixAll")
  edits <- reply$result[[1L]]$edit$changes[[uri]]
  expect_length(edits, 1L)
  expect_equal(edits[[1L]]$newText, " <- ")
})

test_that("Direct fixes cover common layout diagnostics", {
  uri <- "file:///layout-fixes.R"
  document <- Document$new(uri, content = c(
    "if(TRUE){",
    "x; y  ",
    "x %>%",
    " f()"
  ))
  diagnostics <- list(
    list(
      range = range(position(0, 2), position(0, 3)),
      code = "spaces_left_parentheses_linter",
      message = "Place a space before left parenthesis."
    ),
    list(
      range = range(position(0, 8), position(0, 9)),
      code = "brace_linter",
      message = "There should be a space before an opening curly brace."
    ),
    list(
      range = range(position(1, 1), position(1, 2)),
      code = "semicolon_linter",
      message = "Replace compound semicolons by a newline."
    ),
    list(
      range = range(position(1, 4), position(1, 6)),
      code = "trailing_whitespace_linter",
      message = "Remove trailing whitespace."
    ),
    list(
      range = range(position(2, 2), position(2, 5)),
      code = "pipe_consistency_linter",
      message = "Use the |> pipe operator instead of the %>% pipe operator."
    ),
    list(
      range = range(position(3, 0), position(3, 1)),
      code = "indentation_linter",
      message = "Indentation should be 2 spaces but is 1 spaces."
    )
  )

  fixes <- code_action_direct_fixes(document, diagnostics)
  actual <- setNames(
    lapply(fixes, function(fix) fix$edit$newText),
    vapply(fixes, function(fix) fix$title, character(1L))
  )
  expect_equal(actual[["Add space before `(`"]], " (")
  expect_equal(actual[["Add space before `{`"]], " {")
  expect_equal(actual[["Replace `;` with a newline"]], "\n")
  expect_equal(actual[["Remove trailing whitespace"]], "")
  expect_equal(actual[["Replace pipe with `|>`"]], " |>")
  expect_equal(actual[["Fix indentation"]], "  ")
})

test_that("Nolint actions cover all affected lines and extend directives", {
  uri <- "file:///nolint.R"
  document <- Document$new(uri, content = c(
    "x+1 # nolint: commas_linter.",
    "y+2"
  ))
  diagnostics <- lapply(0:1, function(line) {
    list(
      range = range(position(line, 1), position(line, 2)),
      source = "lintr",
      code = "infix_spaces_linter",
      message = "Put spaces around all infix operators."
    )
  })

  actions <- code_action_suppression_actions(uri, document, diagnostics)
  titles <- vapply(actions, function(action) action$title, character(1L))
  all_action <- actions[[match("Disable all linters for these lines", titles)]]
  specific_action <- actions[[match(
    "Disable infix_spaces_linter for these lines", titles)]]

  expect_length(all_action$edit$changes[[uri]], 2L)
  expect_length(specific_action$edit$changes[[uri]], 2L)
  expect_equal(specific_action$edit$changes[[uri]][[1L]]$newText,
    ", infix_spaces_linter")
})

test_that("Code action capabilities and request interface are precise", {
  expect_equal(
    unlist(ServerCapabilities$codeActionProvider$codeActionKinds),
    c("quickfix", "refactor.extract", "refactor.inline", "source.fixAll")
  )
  request_range <- range(position(0, 0), position(0, 1))
  params <- code_action_params(document_uri("file:///test.R"), request_range)
  expect_identical(params$range, request_range)
  expect_null(params$position)
})

test_that("Direct fixes reject diagnostics that cannot be applied safely", {
  diagnostic <- function(code, start = 0L, end = 1L, message = "", line = 0L,
                         source = "lintr") {
    list(
      range = range(position(line, start), position(line, end)),
      source = source,
      code = code,
      message = message
    )
  }

  cases <- list(
    list("x <- 1", diagnostic("assignment_linter", 2L, 4L)),
    list("x + 1", diagnostic("infix_spaces_linter", 2L, 2L)),
    list("f(x)", diagnostic("commas_linter")),
    list(" x", diagnostic("indentation_linter", message = "Bad indentation")),
    list("x %>% f()", diagnostic("pipe_consistency_linter", 2L, 5L,
      "Use one consistent pipe")),
    list("X", diagnostic("T_and_F_symbol_linter")),
    list("x", diagnostic("trailing_whitespace_linter")),
    list(c("", "x"), diagnostic("trailing_blank_lines_linter")),
    list("x", diagnostic("semicolon_linter")),
    list("x", diagnostic("spaces_left_parentheses_linter")),
    list("if (x)", diagnostic("brace_linter", message =
      "There should be a space before an opening curly brace.")),
    list("x == 1", diagnostic("equals_na_linter", 0L, 6L)),
    list("x", diagnostic("unknown_linter"))
  )

  for (case in cases) {
    document <- Document$new("file:///invalid-fix.R", content = case[[1L]])
    expect_null(code_action_direct_fix(document, case[[2L]]))
  }

  already_formatted <- Document$new("file:///no-op.R", content = "x + y")
  expect_null(code_action_direct_fix(
    already_formatted,
    diagnostic("infix_spaces_linter", 1L, 4L)
  ))
  expect_equal(code_action_character("abc", -1L), "")
  expect_equal(code_action_character("abc", 3L), "")
  expect_null(code_action_nearest_character("abc", ",", 0L, 1L))
})

test_that("Code action helpers preserve multiline text and merge duplicates", {
  document <- Document$new("file:///edits.R", content = c("abc", "def"))
  edit <- text_edit(
    range(position(0L, 1L), position(1L, 1L)),
    "replacement"
  )
  expect_equal(code_action_edit_text(document, edit), "bc\nd")

  diagnostic <- list(
    range = range(position(0L, 1L), position(0L, 2L)),
    source = "lintr",
    code = "assignment_linter",
    message = "Use <- for assignment."
  )
  assignment <- Document$new("file:///duplicate.R", content = "x=1")
  fixes <- code_action_direct_fixes(
    assignment,
    list(diagnostic, diagnostic, within(diagnostic, source <- "another-tool"))
  )
  expect_length(fixes, 1L)
  expect_length(fixes[[1L]]$diagnostics, 2L)
  expect_identical(code_action_direct_fixes(assignment, list()), list())
  expect_identical(code_action_non_overlapping_fixes(list()), list())
})

test_that("Nolint edits handle existing, blank, and trailing-space lines", {
  document <- Document$new("file:///nolint-edges.R", content = c(
    "x # nolint",
    "y # nolint: first_linter.",
    "   ",
    "z   "
  ))

  expect_null(code_action_nolint_edit(document, 0L))
  expect_null(code_action_nolint_edit(document, 0L, "new_linter"))
  expect_equal(
    code_action_nolint_edit(document, 1L)$newText,
    "# nolint"
  )
  expect_null(code_action_nolint_edit(document, 1L, "first_linter"))
  expect_equal(
    code_action_nolint_edit(document, 1L, "second_linter")$newText,
    ", second_linter"
  )
  expect_equal(
    code_action_nolint_edit(document, 2L, "blank_linter")$newText,
    "# nolint: blank_linter."
  )
  expect_equal(
    code_action_nolint_edit(document, 3L)$newText,
    " # nolint"
  )
})

test_that("Code action filtering ignores unrelated or invalid diagnostics", {
  uri <- "file:///filtered-actions.R"
  document <- Document$new(uri, content = "x")
  unrelated <- list(
    range = range(position(0L, 0L), position(0L, 1L)),
    source = "another-tool",
    code = "some_rule",
    message = "Not from lintr"
  )
  missing_code <- within(unrelated, {
    source <- "lintr"
    code <- NULL
  })
  invalid_row <- within(unrelated, {
    source <- "lintr"
    range <- range(position(10L, 0L), position(10L, 1L))
  })

  expect_identical(
    code_action_suppression_actions(
      uri, document, list(unrelated, missing_code)
    ),
    list()
  )
  expect_identical(
    code_action_suppression_actions(uri, document, list(invalid_row)),
    list()
  )

  reply <- document_code_action_reply(
    1L, uri, NULL, document, list(),
    list(diagnostics = NULL, only = list("quickfix"))
  )
  expect_identical(reply$result, list())
})
