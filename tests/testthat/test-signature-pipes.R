signature_pipe_cursor <- function(content) {
    row <- which(grepl("<cursor>", content, fixed = TRUE))
    col <- regexpr("<cursor>", content[[row]], fixed = TRUE)[[1L]] - 1L
    content[[row]] <- sub("<cursor>", "", content[[row]], fixed = TRUE)
    list(content = content, point = list(row = row - 1L, col = col))
}

test_that("Piped signature parameters account for implicit and explicit arguments", {
    signature <- "f(x, vec, option = FALSE, ..., named = NULL)"
    cases <- list(
        list("x |> f(<cursor>)", 1L),
        list("x |> f(1, <cursor>)", 2L),
        list("x |> f(<cursor>", 1L),
        list("x |> f(1, <cursor>", 2L),
        list("x |> f(option = <cursor>)", 2L),
        list("x |> f(vec = 1, <cursor>)", 2L),
        list("x |> f(<cursor>, vec = 1)", 2L),
        list("x |> f(1, 2, 3, <cursor>)", 3L),
        list("x |> f(named = <cursor>)", 4L),
        list("x |> f(<cursor>, vec = _)", 0L),
        list("x |> f(1<cursor>, vec = _)", 0L),
        list("x |> f(1, vec = <cursor>_)", 1L),
        list("x |> f(vec = _, <cursor>)", 0L),
        list("x |> f(x = _, <cursor>)", 1L),
        list("x |> f(1, vec = _, <cursor>)", 2L),
        list("x |> f(<cursor>, x = _)", 1L),
        list("x |> f(<cursor>, vec = _", 0L),
        list("x |> base::f(<cursor>)", 1L),
        list("x |> base:::f(<cursor>)", 1L),
        list("x |> object$method(<cursor>)", 1L),
        list("x |> `odd function`(<cursor>)", 1L),
        list("x |> f() |> f(<cursor>)", 1L),
        list("f(x |> f(), <cursor>)", 1L),
        list("x |> f(f(<cursor>))", 0L),
        list("x |> f(f(<cursor>), vec = _)", 0L),
        list("x |> f(y |> f(vec = _), <cursor>)", 2L),
        list("x |> f('_, vec = _', <cursor>)", 2L),
        list('x |> f(r"--(_, vec = _)--", <cursor>)', 2L),
        list("x |> f(`_`, <cursor>)", 2L),
        list("x |> f(.name_with_underscore, <cursor>)", 2L),
        list("x |> f(<cursor>); f(vec = _)", 1L),
        list("f(<cursor>) # x |> f", 0L),
        list("'x |> f'; f(<cursor>)", 0L),
        list("x %custom>% f(<cursor>)", 0L),
        list(c("x |> # f(vec = _)", "  f(<cursor>)"), 1L),
        list(c("x |> f(", "  <cursor>", ", vec = _)", "f()"), 0L),
        list(c("x |> f(1, # vec = _", "  <cursor>)"), 2L),
        list(c("x |> f(vec = _ # a comment", ", <cursor>)"), 0L),
        list("x |> f(1 %#% 2, <cursor>)", 2L),
        list("x |> f(1 %\"% 2<cursor>, vec = _)", 0L),
        list("数据 |> f('😀', <cursor>)", 2L)
    )
    for (case in cases) {
        cursor <- signature_pipe_cursor(case[[1L]])
        document <- Document$new("file:///signature-pipes.R", content = cursor$content)
        opening <- document$detect_call(cursor$point)$opening
        expect_identical(detect_active_parameter(cursor$content,
                opening$row, opening$col, cursor$point$row, cursor$point$col, signature),
            case[[2L]], info = paste(case[[1L]], collapse = "\n"))
    }
})

test_that("Magrittr signature parameters respect top-level dot placeholders", {
    cases <- list(
        list("x %>% f(<cursor>)", 1L),
        list("x %<>% f(<cursor>)", 1L),
        list("x %T>% f(<cursor>)", 1L),
        list("x %T>% f(1, <cursor>)", 2L),
        list("x %T>% f(<cursor>, vec = .)", 0L),
        list("x %T>% f(vec = ., <cursor>)", 0L),
        list("x %T>% f(f(.), <cursor>)", 2L),
        list("x %>% f(1, <cursor>)", 2L),
        list("x %>% f(<cursor>, vec = .)", 0L),
        list("x %>% f(vec = ., <cursor>)", 0L),
        list("x %>% f(., <cursor>)", 1L),
        list("x %>% f(1, ., <cursor>)", 2L),
        list("x %>% f(f(.), <cursor>)", 2L),
        list("x %>% f(.data, <cursor>)", 2L),
        list("x %>% f('.', <cursor>)", 2L),
        list("x %>% { f(<cursor>) }", 0L),
        list("x %>% f(<cursor>) %>% f(.)", 1L)
    )
    for (case in cases) {
        cursor <- signature_pipe_cursor(case[[1L]])
        document <- Document$new("file:///signature-pipes.R", content = cursor$content)
        opening <- document$detect_call(cursor$point)$opening
        expect_identical(detect_active_parameter(cursor$content,
                opening$row, opening$col, cursor$point$row, cursor$point$col,
                "f(x, vec, option = FALSE)"),
            case[[2L]], info = case[[1L]])
    }
})

test_that("Signature replies use pipe context for local functions", {
    skip_if(getRversion() < "4.2.0", "Native pipe placeholders require R 4.2")
    cases <- list(
        list("x |> f(<cursor>)", 1L),
        list("x |> f(<cursor>, vec = _)", 0L),
        list("x |> f(vec = _, <cursor>)", 0L)
    )
    for (case in cases) {
        cursor <- signature_pipe_cursor(c(
            "f <- function(x, vec, option = FALSE) NULL", case[[1L]]))
        fixture <- provider_fixture(cursor$content)
        reply <- signature_reply(1L, fixture$uri, fixture$workspace,
            fixture$document, cursor$point)
        expect_length(reply$result$signatures, 1L)
        expect_equal(reply$result$activeParameter, case[[2L]])
    }
})

test_that("Installed signature help handles native pipes and placeholders", {
    skip_on_cran()
    skip_if(getRversion() < "4.2.0", "Native pipe placeholders require R 4.2")
    client <- language_client(working_dir = withr::local_tempdir())
    temp_file <- withr::local_tempfile(fileext = ".R")
    cases <- list(
        list("x |> findInterval(<cursor>)", 1L),
        list("x |> findInterval(1:10<cursor>, vec = _)", 0L),
        list("x |> findInterval(1:10, vec = <cursor>_)", 1L),
        list("x |> findInterval(vec = _, <cursor>)", 0L)
    )
    for (case in cases) {
        cursor <- signature_pipe_cursor(case[[1L]])
        writeLines(cursor$content, temp_file)
        did_open(client, temp_file)
        result <- respond_signature(client, temp_file,
            c(cursor$point$row, cursor$point$col))
        expect_length(result$signatures, 1L)
        expect_match(result$signatures[[1L]]$label, "^findInterval\\(x, vec,")
        expect_equal(result$activeParameter, case[[2L]])
    }
})
