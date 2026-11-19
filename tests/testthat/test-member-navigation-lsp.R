test_that("LSP member providers agree on list and environment functions after edits", {
    skip_on_cran()
    root <- withr::local_tempdir()
    client <- language_client(working_dir = root)
    path <- file.path(root, "members.R")
    uri <- path_to_uri(path)
    did_open(client, path, text = "fun <- function(unrelated) NULL")
    notify(client, "workspace/didChangeConfiguration", list(settings = list(parse_delay = 0.5)))

    declarations <- list(
        lst0 = "lst0 <- list(fun = function(x, y) { x + y })",
        env = c("env <- new.env()", "env$fun <- function(x, y) {", "  x + y", "}"),
        lst = c("lst <- list()", "lst$fun <- function(x, y) {", "  x + y", "}")
    )
    for (i in seq_along(declarations)) {
        receiver <- names(declarations)[[i]]
        declaration <- declarations[[i]]
        call <- paste0(receiver, "$fun(")
        lines <- c(declaration, paste0(call, "1, 2)"), call)
        row <- length(lines) - 1L
        start <- nchar(receiver) + 1L
        notify(client, "textDocument/didChange", list(textDocument = list(uri = uri, version = i + 1L),
                contentChanges = list(list(text = paste(lines, collapse = "\n")))))

        # The first request after each edit targets the member of an unfinished call.
        definition <- respond_definition(client, path, c(row, start + 1L), retry = FALSE)
        target <- which(grepl("fun", declaration, fixed = TRUE))
        col <- regexpr("fun", declaration[[target]], fixed = TRUE)[[1L]] - 1L
        expect_identical(definition$uri, uri)
        expect_equal(definition$range$start, list(line = target - 1L, character = col))
        expect_equal(definition$range$end, list(line = target - 1L, character = col + 3L))

        members <- respond_completion(client, path, c(row, start), retry = FALSE)$items
        expect_identical(vapply(members, `[[`, character(1L), "label"), "fun")
        expect_identical(members[[1L]]$detail, "fun(x, y)")
        expect_identical(members[[1L]]$data$type, "member")
        args <- respond_completion(client, path, c(row, nchar(call)), retry = FALSE)$items
        args <- Filter(function(item) !is.null(item$data$parameter), args)
        expect_identical(vapply(args, `[[`, character(1L), "insertText"), c("x = ", "y = "))
        hover <- respond_hover(client, path, c(row, start + 1L), retry = FALSE)
        expect_identical(hover$contents[[1L]], "```r\nfun(x, y)\n```")
        signature <- respond_signature(client, path, c(row - 1L, nchar(call) + 3L), retry = FALSE)
        expect_identical(signature$signatures[[1L]]$label, "fun(x, y)")
        expect_equal(signature$activeParameter, 1L)
    }
})
