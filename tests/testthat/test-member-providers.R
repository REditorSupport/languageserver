member_provider_fixture <- function(lines, snapshots = list(), language = "r") {
    uri <- if (language == "r") "file:///member-provider.R" else "file:///member-provider.qmd"
    document <- Document$new(uri, language = language, version = 1L, content = lines)
    data <- parse_document(uri, lines, is_rmarkdown = document$is_rmarkdown)
    data$version <- 1L
    document$update_parse_data(data)
    metadata <- collections::dict()
    for (package in names(snapshots)) metadata$set(package, member_index_thaw(snapshots[[package]]))
    workspace <- list(
        member_metadata = metadata,
        get_documentation = function(...) NULL,
        get_signature = function(...) stop("unrelated bare function lookup"),
        get_help = function(...) stop("unrelated bare help lookup")
    )
    list(uri = uri, document = document, workspace = workspace)
}

member_provider_signature <- function(
  fixture, row = fixture$document$nline - 1L,
  col = nchar(fixture$document$line0(row))
) {
    signature_reply(
        1L, fixture$uri, fixture$workspace, fixture$document,
        list(row = row, col = col)
    )$result
}

member_provider_hover <- function(fixture, row, col) {
    hover_reply(
        1L, fixture$uri, fixture$workspace, fixture$document,
        list(row = row, col = col)
    )$result
}

test_that("Member signatures follow fluent source closures without calling arguments", {
    marker <- withr::local_tempfile()
    fixture <- member_provider_fixture(c(
        "factory <- function(x) list(step=function() list(finish=function(value, flag=TRUE) x))",
        sprintf(
            "factory({writeLines(\"ran\", %s); stop(\"input\")})$step()$finish(flag = ",
            encodeString(marker, quote = "\"")
        )
    ))
    result <- member_provider_signature(fixture)
    expect_identical(result$signatures[[1L]]$label, "finish(value, flag = TRUE)")
    expect_identical(result$activeParameter, 1L)
    expect_length(result$signatures[[1L]]$parameters, 2L)
    expect_false(file.exists(marker))

    fixture <- member_provider_fixture(c(
        "x <- list(run=function(first, ..., option=TRUE) NULL)", "x$run(",
        " first=1,", " option = "
    ))
    expect_identical(member_provider_signature(fixture)$activeParameter, 2L)
    fixture <- member_provider_fixture(c(
        "f <- function() {x <- list(run=function(local=1) NULL); x$run(", "}"
    ))
    expect_identical(member_provider_signature(fixture, 0L)$signatures[[1L]]$label, "run(local = 1)")
})

test_that("Member hover preserves quoted names, fields and UTF-16 ranges", {
    fixture <- member_provider_fixture(c(
        "x <- list(`a b`=function(value=1) NULL, answer=42)",
        "\"\U0001f680\"; x$`a b`(value=1)", "x$answer"
    ))
    for (col in 7:12) {
        result <- member_provider_hover(fixture, 1L, col)
        expect_identical(result$contents, "```r\n`a b`(value = 1)\n```")
        expect_identical(result$range$start$character, 8L)
        expect_identical(result$range$end$character, 13L)
    }
    expect_identical(
        member_provider_signature(fixture, 1L, 20L)$signatures[[1L]]$label,
        "`a b`(value = 1)"
    )
    expect_identical(member_provider_hover(fixture, 2L, 4L)$contents, "```r\n42\n```")
})

test_that("R6 members use public inherited declarations without initialization or getters", {
    fixture <- member_provider_fixture(c(
        "Parent <- R6::R6Class(\"Parent\", public=list(run=function(value, option=FALSE) self))",
        "Child <- R6::R6Class(\"Child\", inherit=Parent, public=list(initialize=function() stop(\"initialize\")),",
        "active=list(danger=function() stop(\"getter\")))", "Child$new()$run(option = "
    ))
    expect_identical(
        member_provider_signature(fixture)$signatures[[1L]]$label,
        "run(value, option = FALSE)"
    )
    expect_identical(
        member_provider_hover(fixture, 3L, 13L)$contents,
        "```r\nrun(value, option = FALSE)\n```"
    )
    fixture$document$set_content(2L, c(fixture$document$content[1:3], "Child$new()$danger"))
    data <- parse_document(fixture$uri, fixture$document$content)
    data$version <- 2L
    fixture$document$update_parse_data(data)
    expect_null(member_provider_hover(fixture, 3L, 14L))
})

test_that("Unknown and shadowed member receivers never resolve unrelated bare functions", {
    for (lines in list(
        c("unknown$sum("),
        c("x <- list(sum=function(value) NULL)", "f <- function(x) {x$sum(", "}"),
        c("x <- list(sum=function(value) NULL)", "x <- opaque()", "x$sum(")
    )) {
        fixture <- member_provider_fixture(lines)
        row <- if (endsWith(tail(lines, 1L), "}")) length(lines) - 2L else length(lines) - 1L
        expect_length(member_provider_signature(fixture, row)$signatures, 0L)
        col <- regexpr("sum", lines[[row + 1L]])[[1L]]
        expect_null(member_provider_hover(fixture, row, col))
    }
    fixture <- member_provider_fixture(c("x <- list(sum=function(value) NULL)", "# x$sum("))
    expect_length(member_provider_signature(fixture)$signatures, 0L)
    expect_null(member_provider_hover(fixture, 1L, 6L))
})

test_that("Member providers stay inside R cells", {
    fixture <- member_provider_fixture(c(
        "```{r}", "x <- list(run=function(value=1) NULL)",
        "x$run(", "```", "```{python}", "x$run(", "```"
    ), language = "quarto")
    expect_identical(member_provider_signature(fixture, 2L)$signatures[[1L]]$label, "run(value = 1)")
    expect_identical(member_provider_hover(fixture, 2L, 3L)$contents, "```r\nrun(value = 1)\n```")
    expect_null(member_provider_hover(fixture, 5L, 3L))
    expect_null(member_provider_signature(fixture, 5L)$signatures)
})

test_that("Installed package members retain method signatures and documentation identity", {
    skip_if_not_installed("polars")
    snapshot <- member_prepare_package("polars")
    fixture <- member_provider_fixture(c(
        "library(polars)", "q <- pl$scan_csv(csv_file)",
        "q$group_by(\"Species\", .maintain_order = "
    ), list(polars = snapshot))
    queried <- character()
    fixture$workspace$get_documentation <- function(key, package, isf, uri) {
        expect_identical(package, "polars")
        expect_identical(uri, fixture$uri)
        queried <<- c(queried, key)
        list(
            description = paste("Documentation for", key),
            arguments = list(.maintain_order = "Preserve group order.")
        )
    }
    signature <- member_provider_signature(fixture)
    expect_identical(signature$signatures[[1L]]$label, "group_by(..., .maintain_order = FALSE)")
    expect_identical(signature$activeParameter, 1L)
    hover <- member_provider_hover(fixture, 2L, 5L)
    expect_match(hover$contents[[2L]], "lazyframe__group_by", fixed = TRUE)
    argument <- member_provider_hover(fixture, 2L, 25L)
    expect_identical(argument$contents[[1L]], "```r\ngroup_by(..., .maintain_order = FALSE)\n```")
    expect_identical(argument$contents[[2L]], "`.maintain_order` - Preserve group order.")
    expect_true(all(queried == "lazyframe__group_by"))

    for (case in list(
        c("pl$col(\"x\")$sum(", "expr__sum"),
        # Series delegates this operation to Expr and copies its formals.
        c("pl$Series(\"x\", 1:3)$sum(", "expr__sum"),
        c("pl$col(\"x\")$str$to_uppercase(", "expr_str_to_uppercase")
    )) {
        fixture$document$set_content(2L, c("library(polars)", case[[1L]]))
        data <- parse_document(fixture$uri, fixture$document$content)
        data$version <- 2L
        fixture$document$update_parse_data(data)
        result <- member_provider_signature(fixture)
        expect_length(result$signatures, 1L)
        expect_identical(tail(queried, 1L), case[[2L]])
        col <- nchar(case[[1L]]) - 2L
        hover <- member_provider_hover(fixture, 1L, col)
        expect_match(hover$contents[[2L]], case[[2L]], fixed = TRUE)
    }
    fixture$document$set_content(3L, c("library(polars)", "pl$scan_csv(csv_file)$`_ldf`$collect("))
    data <- parse_document(fixture$uri, fixture$document$content)
    data$version <- 3L
    fixture$document$update_parse_data(data)
    expect_identical(member_provider_signature(fixture)$signatures[[1L]]$label, "collect(engine)")
    expect_identical(tail(queried, 1L), "PlRLazyFrame_collect")
})

test_that("Member signature and hover work over LSP immediately after an edit", {
    skip_on_cran()
    skip_if_not_installed("polars")
    client <- language_client()
    path <- withr::local_tempfile(fileext = ".R")
    uri <- path_to_uri(path)
    lines <- c("library(polars)", "q <- pl$scan_csv(csv_file)", "q$group_by(\"Species\")")
    did_open(client, path, text = paste(lines, collapse = "\n"))
    deadline <- Sys.time() + 15
    repeat {
        ready <- respond_completion(client, path, c(2L, 2L), retry = FALSE)
        if (any(vapply(ready$items, function(item) identical(item$data$type, "member"), logical(1L)))) break
        if (Sys.time() > deadline) break
        Sys.sleep(0.1)
    }
    expect_true(any(vapply(ready$items, function(item) identical(item$data$type, "member"), logical(1L))))
    notify(client, "workspace/didChangeConfiguration", list(settings = list(parse_delay = 0.5)))
    lines[[3L]] <- "q$group_by(\"Species\", .maintain_order = "
    notify(client, "textDocument/didChange", list(
        textDocument = list(uri = uri, version = 2L),
        contentChanges = list(list(text = paste(lines, collapse = "\n")))
    ))
    # No request retry or delay after didChange: the handler must wait for the
    # current incomplete parse, then infer the method on the first request.
    result <- respond_signature(client, path, c(2L, nchar(lines[[3L]])), retry = FALSE)
    expect_identical(result$signatures[[1L]]$label, "group_by(..., .maintain_order = FALSE)")
    expect_equal(result$activeParameter, 1L)
    expect_match(result$signatures[[1L]]$documentation$value, "group", ignore.case = TRUE)

    lines[[3L]] <- "q$group_by(\"Species\", .maintain_order = TRUE)"
    notify(client, "textDocument/didChange", list(
        textDocument = list(uri = uri, version = 3L),
        contentChanges = list(list(text = paste(lines, collapse = "\n")))
    ))
    hover <- respond_hover(client, path, c(2L, 5L), retry = FALSE)
    expect_identical(hover$contents[[1L]], "```r\ngroup_by(..., .maintain_order = FALSE)\n```")
    expect_match(hover$contents[[2L]], "group", ignore.case = TRUE)
    expect_equal(hover$range$start$character, 2L)
    expect_equal(hover$range$end$character, 10L)
    argument <- respond_hover(client, path, c(2L, 25L), retry = FALSE)
    expect_match(argument$contents[[2L]], "`.maintain_order`", fixed = TRUE)
})
