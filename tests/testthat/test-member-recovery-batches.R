recovery_line_items <- function(content) {
    items <- list()
    start <- 1L
    for (end in seq_along(content)) {
        if (end - start > 128L) start <- end
        text <- content[seq.int(start, end)]
        if (sum(nchar(text, type = "bytes")) > 65536L) {
            start <- end
            next
        }
        message <- NULL
        parsed <- tryCatch(parse(text = text, keep.source = TRUE), error = function(e) {
            message <<- conditionMessage(e)
            NULL
        })
        if (is.null(parsed)) {
            if (!grepl("unexpected end of input|INCOMPLETE_STRING", message)) start <- end + 1L
            next
        }
        refs <- attr(parsed, "srcref")
        for (i in seq_along(parsed)) {
            ref <- refs[[i]]
            items[[length(items) + 1L]] <- list(expr = utils::removeSource(parsed[[i]]),
                start = c(start + ref[[1L]] - 2L, ref[[5L]] - 1L),
                end = c(start + ref[[3L]] - 2L, ref[[6L]]))
        }
        start <- end + 1L
    }
    items
}

test_that("batched incomplete recovery preserves statements ranges and limits", {
    prefix <- sprintf("item_%d <- %d", 1:300, 1:300)
    cases <- list(
        c(prefix, "value <- list(alpha = 1)", "value$"),
        c("# comment", "x <- list(", "α = 1,", "β = 2)", "x$"),
        c(prefix, "bad <- )", "after <- list(a = 1)", "after$"),
        c(rep("# padding", 127), "f <- function(x) {", "  x + 1", "}", "f("),
        c("f <- function(x) {", rep("x <- x + 1", 130), "}", "after <- 1", "after$"),
        c("a <- 1; b <- 2", "if (TRUE) a <- 2", "else b <- 3", "b$"),
        c("x <- 1 +", "  2", "f <- function(x = list(a = 1)) x", "f()$"),
        c('#line 1000 "virtual.R"', "value <- list(alpha = 1)", rep("", 130), "value$"),
        c(paste0("#", paste(rep("a", 65537), collapse = "")), "x <- 1", "y <- 2", "y$"),
        character())
    for (content in cases) {
        actual <- member_document_index(content)$items
        actual <- lapply(actual, function(item) {
            list(expr = utils::removeSource(item$expr), start = item$start, end = item$end)
        })
        expect_identical(actual, recovery_line_items(content))
    }
})

test_that("line directives do not shift recovered binding positions into virtual files", {
    content <- c('#line 1000 "virtual.R"', "value <- list(alpha = 1)", rep("", 130), "value$")
    index <- member_document_index(content)
    expect_identical(index$items[[1L]]$start, c(1L, 0L))
    fixture <- provider_fixture(content)
    result <- member_completion(fixture$uri, fixture$workspace, fixture$document,
        list(row = 132L, col = 6L), TRUE, 200L)
    expect_true("alpha" %in% vapply(result, `[[`, character(1L), "label"))
})

test_that("complete recovery sections do not parse once per line", {
    content <- c(sprintf("item_%d <- %d", 1:500, 1:500), "value <- list(alpha = 1)", "value$")
    recover <- member_document_index
    calls <- 0L
    mockery::stub(recover, "parse", function(...) {
        calls <<- calls + 1L
        base::parse(...)
    })
    actual <- recover(content)
    expect_length(actual$items, 501L)
    expect_lte(calls, 133L)
    expect_identical(actual$bindings$value[[1L]]$expr, quote(list(alpha = 1)))
})

test_that("incomplete recovery retains preceding functions aliases and effects", {
    prefix <- sprintf("item_%d <- %d", 1:150, 1:150)
    fixture <- provider_fixture(c(prefix, "make <- function(x = 1) list(alpha = x)",
            "value <- make()", "alias <- value", "alias$"))
    result <- member_completion(fixture$uri, fixture$workspace, fixture$document,
        list(row = 153L, col = 6L), TRUE, 200L)
    expect_true("alpha" %in% vapply(result, `[[`, character(1L), "label"))
})
