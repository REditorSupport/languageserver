inlay_pipe_hints <- function(fixture, request = range(position(0L, 0L), position(100L, 0L))) {
    inlay_hint_reply(1L, fixture$uri, fixture$workspace, fixture$document, request)$result
}

inlay_pipe_labels <- function(hints) {
    indices <- order(vapply(hints, function(hint) hint$position$line, integer(1L)),
        vapply(hints, function(hint) hint$position$character, integer(1L)))
    vapply(hints[indices], `[[`, character(1L), "label")
}

test_that("Native pipe inlay hints match the inserted argument and placeholders", {
    skip_if(getRversion() < "4.2.0", "Native pipe placeholders require R 4.2")
    cases <- list(
        list("x |> fun1(1, 2)", c("arg2 =", "arg3 =")),
        list("x |> fun1(1)", "arg2 ="),
        list("x |> fun1()", character()),
        list("x |> fun1(1, arg3 = 2)", "arg2 ="),
        list("x |> fun1(arg1 = 1, 2)", "arg3 ="),
        list("x |> fun1(arg2 = _, 1, 2)", c("arg1 =", "arg3 =")),
        list("x |> fun1(1, arg2 = _, 2)", c("arg1 =", "arg3 =")),
        list("x |> fun1(1, 2, arg3 = _)", c("arg1 =", "arg2 =")),
        list("x |> fun1(arg2 = 1, arg3 = 2)", character()),
        list("x |> fun1('_', 2)", c("arg2 =", "arg3 =")),
        list("x |> fun1(`_`, 2)", c("arg2 =", "arg3 =")),
        list("x |> pkg::fun1(1, 2)", c("arg2 =", "arg3 =")),
        list("x |> fun1(1, 2) |> fun1(3, 4)", rep(c("arg2 =", "arg3 ="), 2L)),
        list("x |> fun1(fun1(1, 2), 3)", c("arg2 =", "arg1 =", "arg2 =", "arg3 =")),
        list("fun1(x |> fun1(1, 2), 3)", c("arg1 =", "arg2 =", "arg3 =", "arg2 =")),
        list(c("x |> # placeholder _", "  fun1(1, 2)"), c("arg2 =", "arg3 =")),
        list(c("x |> fun1(", "  1, arg2 = _ # pipe argument", "  , 2)"), c("arg1 =", "arg3 =")),
        list('"\U0001f600" |> fun1(1, 2)', c("arg2 =", "arg3 ="))
    )
    for (case in cases) {
        fixture <- provider_fixture(case[[1L]], function(...) formals(function(arg1, arg2, arg3) NULL))
        actual <- inlay_pipe_hints(fixture)
        expect_equal(inlay_pipe_labels(actual), case[[2L]], info = paste(case[[1L]], collapse = "\n"))
        fixture$document$parse_data$range_data <- NULL
        expect_equal(inlay_pipe_hints(fixture), actual)
    }
})

test_that("Magrittr inlay hints respect direct dot placeholders", {
    cases <- list(
        list("x %>% fun1(1, 2)", c("arg2 =", "arg3 =")),
        list("x %<>% fun1(1, 2)", c("arg2 =", "arg3 =")),
        list("x %T>% fun1(1, 2)", c("arg2 =", "arg3 =")),
        list("x %T>% fun1(1)", "arg2 ="),
        list("x %T>% fun1(., 2)", c("arg1 =", "arg2 =")),
        list("x %T>% fun1(1, arg2 = ., 2)", c("arg1 =", "arg3 =")),
        list("x %T>% fun1(fun1(.), 2)", c("arg2 =", "arg3 =")),
        list("x %>% fun1(., 2)", c("arg1 =", "arg2 =")),
        list("x %>% fun1(1, arg2 = ., 2)", c("arg1 =", "arg3 =")),
        list("x %>% fun1(fun1(.), 2)", c("arg2 =", "arg3 =")),
        list("x %>% fun1('.data', 2)", c("arg2 =", "arg3 =")),
        list("x %>% fun1('.', 2)", c("arg2 =", "arg3 =")),
        list("x %>% { fun1(1, 2) }", c("arg1 =", "arg2 =")),
        list("x %custom>% fun1(1, 2)", c("arg1 =", "arg2 ="))
    )
    for (case in cases) {
        fixture <- provider_fixture(case[[1L]], function(...) formals(function(arg1, arg2, arg3) NULL))
        actual <- inlay_pipe_hints(fixture)
        expect_equal(inlay_pipe_labels(actual), case[[2L]], info = case[[1L]])
        fixture$document$parse_data$range_data <- NULL
        expect_equal(inlay_pipe_hints(fixture), actual)
    }
})

test_that("Piped inlay hints retain viewport positions and stop at ellipsis", {
    fixture <- provider_fixture("x %>% fun1(1, 2)", function(...) formals(function(arg1, arg2, arg3) NULL))
    hints <- inlay_pipe_hints(fixture)
    expect_equal(vapply(hints, function(hint) hint$position$character, integer(1L)), c(11L, 14L))
    expect_equal(inlay_pipe_hints(fixture, range(position(0L, 14L), position(1L, 0L))), hints[2L])
    for (args in list(formals(function(arg1, ..., named) NULL), formals(function(..., named) NULL))) {
        fixture <- provider_fixture("x %>% fun1(1, 2)", function(...) args)
        expect_length(inlay_pipe_hints(fixture), 0L)
        fixture$document$parse_data$range_data <- NULL
        expect_length(inlay_pipe_hints(fixture), 0L)
    }
})

test_that("Cached piped hints survive unrelated edits and invalidate changed pipes", {
    fixture <- provider_fixture(c("x %>%", "  fun1(1, 2)", "", "fun1(3, 4)"),
        function(...) formals(function(arg1, arg2, arg3) NULL))
    original <- inlay_pipe_hints(fixture)
    expect_equal(inlay_pipe_labels(original), c("arg2 =", "arg3 =", "arg1 =", "arg2 ="))
    parsed <- inlay_hint_edit(fixture, 2L, list(list(
        range = range(position(2L, 0L), position(2L, 0L)), text = "other("
    )))
    expect_true(parsed$parse_error)
    expect_identical(inlay_pipe_hints(fixture), original)
    parsed <- inlay_hint_edit(fixture, 3L, list(list(
        range = range(position(0L, 2L), position(0L, 5L)), text = "+"
    )))
    expect_true(parsed$parse_error)
    expect_equal(inlay_pipe_hints(fixture), original[3:4])
})

test_that("Cached hints invalidate when a multiline call becomes piped", {
    for (pipe in c("%>%", "%T>%")) {
        fixture <- provider_fixture(c("x +", "  fun1(1, 2)", "", "fun1(3, 4)"),
            function(...) formals(function(arg1, arg2, arg3) NULL))
        original <- inlay_pipe_hints(fixture)
        expect_equal(inlay_pipe_labels(original), rep(c("arg1 =", "arg2 ="), 2L))
        parsed <- inlay_hint_edit(fixture, 2L, list(
            list(range = range(position(0L, 2L), position(0L, 3L)), text = pipe),
            list(range = range(position(2L, 0L), position(2L, 0L)), text = "other(")
        ))
        expect_true(parsed$parse_error)
        expect_equal(inlay_pipe_hints(fixture), original[3:4])
        inlay_hint_edit(fixture, 3L, list(list(
            range = range(position(2L, 0L), position(2L, 6L)), text = ""
        )))
        expect_equal(inlay_pipe_labels(inlay_pipe_hints(fixture)),
            c("arg2 =", "arg3 =", "arg1 =", "arg2 ="))
    }
})

test_that("Installed signature help and inlay hints account for tee pipes", {
    skip_on_cran()
    client <- language_client(working_dir = withr::local_tempdir())
    path <- withr::local_tempfile(fileext = ".R")
    did_open(client, path, text = c("fun1 <- function(arg1, arg2, arg3) NULL", "x %T>% fun1(1, 2)"))
    signature <- respond_signature(client, path, c(1L, nchar("x %T>% fun1(")))
    expect_equal(signature$activeParameter, 1L)
    hints <- respond(client, "textDocument/inlayHint", list(
        textDocument = list(uri = path_to_uri(path)),
        range = range(position(1L, 0L), position(2L, 0L))
    ))
    expect_equal(inlay_pipe_labels(hints), c("arg2 =", "arg3 ="))
})

test_that("Installed inlay hints account for the native pipe", {
    skip_on_cran()
    skip_if(getRversion() < "4.2.0", "Native pipe placeholders require R 4.2")
    client <- language_client(working_dir = withr::local_tempdir())
    path <- withr::local_tempfile(fileext = ".R")
    writeLines(c("fun1 <- function(arg1, arg2, arg3) { }", "x |> fun1(1, 2)",
            "x |> fun1(1, arg2 = _, 2)"), path)
    did_open(client, path)
    hints <- respond(client, "textDocument/inlayHint", list(
        textDocument = list(uri = path_to_uri(path)),
        range = range(position(1L, 0L), position(3L, 0L))
    ))
    expect_equal(inlay_pipe_labels(hints), c("arg2 =", "arg3 =", "arg1 =", "arg3 ="))
    expect_equal(vapply(hints, function(hint) hint$data$parameter, character(1L)),
        c("arg2", "arg3", "arg1", "arg3"))
})
