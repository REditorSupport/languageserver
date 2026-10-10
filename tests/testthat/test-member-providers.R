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

test_that("Multiline dollar chains share completion, signatures and hover", {
    code <- c(
        "make_pipeline <- function(source) {",
        "self <- new.env(parent = emptyenv())",
        "self$source <- source",
        "self$filter <- function(predicate) self",
        "self$limit <- function(n = 10L) self",
        "self$collect <- function(format = \"list\") {",
        "list(data = source, format = format, metadata = list(cached = FALSE))",
        "}", "self", "}", "pipeline <- make_pipeline(iris)",
        "pipeline$", "  filter(\"Sepal.Length > 5\")$", "  limit(n = 5)$",
        "  filter(\"Petal.Length < 2\")$", "  collect()"
    )
    fixture <- member_provider_fixture(code)
    for (row in 11:15) {
        col <- if (row == 11L) 9L else 4L
        items <- member_completion(fixture$uri, fixture$workspace, fixture$document, list(row = row, col = col), TRUE, 200L)
        labels <- vapply(items, `[[`, character(1L), "label")
        expected <- if (row == 11L) c("collect", "filter", "limit", "source") else if (row == 13L) "limit" else if (row == 15L) "collect" else "filter"
        expect_identical(labels, expected)
        if (row > 11L) {
            expect_identical(items[[1L]]$textEdit$range$start$line, row)
            expect_identical(items[[1L]]$textEdit$range$start$character, 2L)
            expect_false(grepl("[(]", items[[1L]]$textEdit$newText))
        }
    }
    for (row in 12:15) {
        col <- regexpr("[(]", code[[row + 1L]])[[1L]]
        expected <- if (row == 13L) "limit(n = 10L)" else if (row == 15L) "collect(format = \"list\")" else "filter(predicate)"
        expect_identical(member_provider_signature(fixture, row, col)$signatures[[1L]]$label, expected)
        expect_identical(member_provider_hover(fixture, row, 4L)$contents, sprintf("```r\n%s\n```", expected))
    }
    fixture <- member_provider_fixture(c(code[1:11], "pipeline$ # continue", " # comment", "", "  collect(format = "))
    expect_identical(member_provider_signature(fixture)$signatures[[1L]]$label, "collect(format = \"list\")")
    expect_identical(member_provider_hover(fixture, 14L, 4L)$range$start$character, 2L)
    fixture <- member_provider_fixture(c("x <- list(`a b`=function(value=1) NULL)", "x$", "  `a b`("))
    expect_identical(member_provider_signature(fixture)$signatures[[1L]]$label, "`a b`(value = 1)")
    expect_identical(member_provider_hover(fixture, 2L, 4L)$contents, "```r\n`a b`(value = 1)\n```")
    for (lines in list(c("x <- list(run=function() NULL)", "\"x$\"", "run("),
            c("x <- list(run=function() NULL)", "# x$", "run("))) {
        fixture <- member_provider_fixture(lines)
        expect_null(member_hover_location(fixture$document, list(row = 2L, col = 2L)))
        expect_null(member_symbol(fixture$uri, fixture$workspace, fixture$document,
                member_call_location(fixture$document, list(row = 2L, col = 4L)))$signature)
    }
})

test_that("R6 self, super and private refer to their declared method contexts", {
    parent <- paste0("Parent <- R6::R6Class(\"Parent\", public=list(run=function(value=1) self, base=42),",
        "private=list(inherited=function(key=2) self), active=list(parent_active=function() stop(\"getter\")))")
    code <- c(parent, "Child <- R6::R6Class(\"Child\", inherit=Parent,", "public=list(",
        "test=function() {", "  self$child(option = TRUE)", "  super$run(value = 2)", "},",
        "child=function(option=FALSE) self),", "private=list(secret=function(key=1) NULL),",
        "active=list(current=function() self$child()))")
    fixture <- member_provider_fixture(code)
    for (case in list(list(row = 4L, col = 7L, expected = c("base", "child", "current", "parent_active", "run", "test")),
            list(row = 5L, col = 8L, expected = c("inherited", "parent_active", "run")))) {
        items <- member_completion(fixture$uri, fixture$workspace, fixture$document, case[c("row", "col")], TRUE, 200L)
        expect_identical(vapply(items, `[[`, character(1L), "label"), case$expected)
    }
    expect_identical(member_provider_hover(fixture, 4L, 10L)$contents, "```r\nchild(option = FALSE)\n```")
    expect_identical(member_provider_hover(fixture, 5L, 10L)$contents, "```r\nrun(value = 1)\n```")
    expect_identical(member_provider_signature(fixture, 4L, 14L)$signatures[[1L]]$label, "child(option = FALSE)")
    expect_identical(member_provider_hover(fixture, 9L, 37L)$contents, "```r\nchild(option = FALSE)\n```")

    fixture <- member_provider_fixture(c(parent, "Child <- R6::R6Class(\"Child\", inherit=Parent,",
            "private=list(test=function() private$secret(), secret=function(key=1) NULL))"))
    expect_identical(member_provider_hover(fixture, 2L, 42L)$contents, "```r\nsecret(key = 1)\n```")
    fixture <- member_provider_fixture(c(parent, "Child <- R6::R6Class(\"Child\", inherit=Parent,",
            "public=list(test=function(self) self$run()))"))
    expect_null(member_provider_hover(fixture, 2L, 38L))
    fixture <- member_provider_fixture(c("f <- function() self$run()"))
    expect_null(member_provider_hover(fixture, 0L, 23L))
    # A trailing parse error cannot remove the class context before it.
    fixture <- member_provider_fixture(c(code, "broken <- )"))
    expect_identical(member_provider_hover(fixture, 4L, 10L)$contents, "```r\nchild(option = FALSE)\n```")
    fixture <- member_provider_fixture(c(parent, "Child <- R6::R6Class(\"Child\", inherit=Parent,",
            "public=list(test=function() super$run()$child(), child=function(option=FALSE) self))"))
    expect_identical(member_provider_hover(fixture, 2L, 42L)$contents, "```r\nchild(option = FALSE)\n```")
    fixture <- member_provider_fixture(c(parent, "Child <- R6::R6Class(\"Child\", inherit=Parent,",
            "public=list(test=function() {self <- list(local=1); self$local}))"))
    expect_identical(member_provider_hover(fixture, 2L, 59L)$contents, "```r\n1\n```")
    fixture <- member_provider_fixture(c("```{r}", code, "```", "```{python}", "self$child()", "```"), language = "quarto")
    expect_identical(member_provider_hover(fixture, 5L, 10L)$contents, "```r\nchild(option = FALSE)\n```")
    expect_null(member_call_location(fixture$document, list(row = 13L, col = 11L)))
})

test_that("Multiline members and R6 method context work on the first LSP request after edits", {
    skip_on_cran()
    client <- language_client()
    path <- withr::local_tempfile(fileext = ".R")
    uri <- path_to_uri(path)
    declarations <- c("x <- list(run=function(value=1) NULL)")
    did_open(client, path, text = declarations)
    notify(client, "workspace/didChangeConfiguration", list(settings = list(parse_delay = 0.5)))
    edit <- function(lines, version) {
        notify(client, "textDocument/didChange", list(textDocument = list(uri = uri, version = version),
                contentChanges = list(list(text = paste(lines, collapse = "\n")))))
    }
    edit(c(declarations, "x$", "  ru"), 2L)
    result <- respond_completion(client, path, c(2L, 4L), retry = FALSE)
    expect_identical(vapply(result$items, `[[`, character(1L), "label"), "run")
    edit(c(declarations, "x$", "  run(value = "), 3L)
    expect_identical(respond_signature(client, path, c(2L, 14L), retry = FALSE)$signatures[[1L]]$label, "run(value = 1)")
    edit(c(declarations, "x$", "  run()"), 4L)
    expect_identical(respond_hover(client, path, c(2L, 4L), retry = FALSE)$contents[[1L]], "```r\nrun(value = 1)\n```")
    code <- c("Parent <- R6::R6Class(\"Parent\", public=list(run=function(value=1) self))",
        "Child <- R6::R6Class(\"Child\", inherit=Parent, public=list(",
        "test=function() {self$},", "child=function(flag=TRUE) self))")
    edit(code, 5L)
    result <- respond_completion(client, path, c(2L, 22L), retry = FALSE)
    expect_setequal(vapply(result$items, `[[`, character(1L), "label"), c("run", "test", "child"))
    code[[3L]] <- "test=function() super$run(),"
    edit(code, 6L)
    expect_identical(respond_hover(client, path, c(2L, 24L), retry = FALSE)$contents[[1L]], "```r\nrun(value = 1)\n```")
})

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

test_that("Copied R6 command methods share completion, signatures and hover", {
    lines <- c(
        "commands <- function(send) list(GET=function(key) send(key), SET=function(key, value, option=NULL) send(key, value))",
        "Client <- R6::R6Class(\"Client\", lock_objects=FALSE, public=list(initialize=function(send) {",
        "methods <- commands(send); for (name in names(methods)) {self[[name]] <- methods[[name]]}",
        "}))", "client <- Client$new(unknown)", "client$SET(option = "
    )
    fixture <- member_provider_fixture(lines)
    result <- member_provider_signature(fixture)
    expect_identical(result$signatures[[1L]]$label, "SET(key, value, option = NULL)")
    expect_identical(result$activeParameter, 2L)
    expect_identical(member_provider_hover(fixture, 5L, 8L)$contents,
        "```r\nSET(key, value, option = NULL)\n```")
})

test_that("R6 initializer early returns preserve public method providers", {
    for (result in c("private", "invisible(NULL)", "list(secret=1)")) {
        fixture <- member_provider_fixture(c(
            sprintf('Client <- R6::R6Class("Client", public=list(run=NULL, initialize=function() {self$run <- function(value, flag=TRUE) NULL; return(%s)}), private=list(secret=1))', result),
            "client <- Client$new()", "client$run(flag = "
        ))
        signature <- member_provider_signature(fixture)
        expect_identical(signature$signatures[[1L]]$label, "run(value, flag = TRUE)")
        expect_identical(signature$activeParameter, 1L)
        expect_identical(member_provider_hover(fixture, 2L, 8L)$contents,
            "```r\nrun(value, flag = TRUE)\n```")
    }
})

test_that("R6 assignment attempts retain original method signatures and opaque active bindings", {
    for (body in c("self$run <- function(replacement=2) NULL; self$x <- function(fake=1) NULL",
            "methods <- list(run=function(replacement=2) NULL, x=function(fake=1) NULL); for (name in names(methods)) self[[name]] <- methods[[name]]")) {
        declaration <- sprintf('Client <- R6::R6Class("Client", lock_objects=FALSE, public=list(run=function(original=1) NULL, initialize=function() {%s}), active=list(x=function(value) NULL))', body)
        fixture <- member_provider_fixture(c(declaration, "client <- Client$new()", "client$run("))
        expect_identical(member_provider_signature(fixture)$signatures[[1L]]$label, "run(original = 1)")
        expect_identical(member_provider_hover(fixture, 2L, 8L)$contents, "```r\nrun(original = 1)\n```")
        fixture <- member_provider_fixture(c(declaration, "client <- Client$new()", "client$x("))
        expect_length(member_provider_signature(fixture)$signatures, 0L)
        expect_null(member_provider_hover(fixture, 2L, 8L))
    }
})

test_that("Installed callr factory members share completion, signatures and hover", {
    snapshot <- member_prepare_package("callr")
    fixture <- member_provider_fixture(c(
        'job <- callr::r_bg(function() stop("must not run"))', "job$get_result("
    ), list(callr = snapshot))
    items <- member_completion(fixture$uri, fixture$workspace, fixture$document,
        list(row = 1L, col = 4L), TRUE, 200L)
    labels <- vapply(items, `[[`, character(1L), "label")
    expect_true(all(c("cleanup", "get_result") %in% labels))
    item <- items[[match("get_result", labels)]]
    expect_identical(item$detail, "get_result()")
    expect_identical(item$kind, CompletionItemKind$Method)
    expect_identical(member_provider_signature(fixture)$signatures[[1L]]$label, "get_result()")
    expect_identical(member_provider_hover(fixture, 1L, 5L)$contents, "```r\nget_result()\n```")
})

test_that("Installed processx members preserve method parameters across providers", {
    skip_if_not_installed("processx")
    snapshot <- member_prepare_package("processx")
    for (case in list(c("is_alive(", "is_alive()"), c("wait(timeout = ", "wait(timeout = -1)"))) {
        fixture <- member_provider_fixture(c(
            'proc <- processx::process$new("must-not-launch")', paste0("proc$", case[[1L]])
        ), list(processx = snapshot))
        items <- member_completion(fixture$uri, fixture$workspace, fixture$document,
            list(row = 1L, col = 5L), TRUE, 200L)
        labels <- vapply(items, `[[`, character(1L), "label")
        expect_true(all(c("is_alive", "wait", "get_exit_status") %in% labels))
        method <- sub("[(].*", "", case[[1L]])
        item <- items[[match(method, labels)]]
        expect_identical(item$detail, case[[2L]])
        expect_identical(item$kind, CompletionItemKind$Method)
        result <- member_provider_signature(fixture)
        expect_identical(result$signatures[[1L]]$label, case[[2L]])
        if (method == "wait") {
            expect_identical(result$activeParameter, 0L)
            expect_identical(result$signatures[[1L]]$parameters[[1L]]$label, c(5L, 17L))
        }
        expect_identical(member_provider_hover(fixture, 1L, 6L)$contents,
            sprintf("```r\n%s\n```", case[[2L]]))
    }
})

test_that("callr and processx members work through LSP without library calls or execution", {
    skip_on_cran()
    skip_if_not_installed("processx")
    root <- withr::local_tempdir()
    marker <- file.path(root, "executed")
    client <- language_client(working_dir = root)
    # Coverage loads the instrumented namespace in both parse and metadata
    # workers, alongside the other parallel tests in this shard.
    timeout <- if (identical(Sys.getenv("R_COVR"), "true")) 60 else 15
    cases <- list(
        list(package = "callr", receiver = "job", method = "get_result", signature = "get_result()",
            code = sprintf('job <- callr::r_bg(function() {writeLines("ran", %s); stop("input")})',
                encodeString(marker, quote = "\""))),
        list(package = "processx", receiver = "proc", method = "is_alive", signature = "is_alive()",
            code = sprintf('proc <- processx::process$new({writeLines("ran", %s); stop("command")})',
                encodeString(marker, quote = "\"")))
    )
    for (case in cases) {
        path <- file.path(root, paste0(case$package, ".R"))
        call <- paste0(case$receiver, "$", case$method, "()")
        did_open(client, path, text = paste(case$code, call, sep = "\n"))
        deadline <- Sys.time() + timeout
        repeat {
            result <- respond_completion(client, path, c(1L, nchar(case$receiver) + 1L), retry = FALSE)
            labels <- vapply(result$items, `[[`, character(1L), "label")
            if (case$method %in% labels || Sys.time() > deadline) break
            Sys.sleep(0.1)
        }
        expect_true(case$method %in% labels, info = case$package)
        result <- respond_signature(client, path, c(1L, nchar(call) - 1L), retry = FALSE)
        expect_length(result$signatures, 1L)
        if (length(result$signatures) != 1L) next
        expect_identical(result$signatures[[1L]]$label, case$signature)
        result <- respond_hover(client, path, c(1L, nchar(case$receiver) + 2L), retry = FALSE)
        expect_identical(result$contents[[1L]], sprintf("```r\n%s\n```", case$signature))
        expect_false(file.exists(marker))
    }
})

test_that("Installed Redux methods provide signatures and hover without connecting", {
    skip_if_not_installed("redux")
    snapshot <- member_prepare_package("redux")
    for (case in list(c("GET(key = ", "GET(key)", "0"),
            c("SET(value = ", "SET(key, value, EX = NULL, PX = NULL, condition = NULL)", "1"),
            c("PING(message = ", "PING(message = NULL)", "0"))) {
        fixture <- member_provider_fixture(c('redis <- redux::hiredis(host = "127.0.0.1")',
                paste0("redis$", case[[1L]])), list(redux = snapshot))
        result <- member_provider_signature(fixture)
        expect_identical(result$signatures[[1L]]$label, case[[2L]])
        expect_identical(result$activeParameter, as.integer(case[[3L]]))
        expect_identical(member_provider_hover(fixture, 1L, 7L)$contents,
            sprintf("```r\n%s\n```", case[[2L]]))
    }
})

test_that("Redux completion, signature and hover work through LSP without a server", {
    skip_on_cran()
    skip_if_not_installed("redux")
    root <- withr::local_tempdir()
    path <- file.path(root, "redis.R")
    client <- language_client(working_dir = root)
    lines <- c('redis <- redux::hiredis(host = "127.0.0.1")', "redis$GET(key = \"key\")")
    did_open(client, path, text = paste(lines, collapse = "\n"))
    deadline <- Sys.time() + 15
    repeat {
        result <- respond_completion(client, path, c(1L, 6L), retry = FALSE)
        labels <- vapply(result$items, `[[`, character(1L), "label")
        if ("GET" %in% labels || Sys.time() > deadline) break
        Sys.sleep(0.1)
    }
    expect_true(all(c("GET", "SET", "PING") %in% labels))
    result <- respond_signature(client, path, c(1L, 10L), retry = FALSE)
    expect_identical(result$signatures[[1L]]$label, "GET(key)")
    result <- respond_hover(client, path, c(1L, 7L), retry = FALSE)
    expect_identical(result$contents[[1L]], "```r\nGET(key)\n```")
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

test_that("Polars query providers prefer receiver metadata over competing cached exports", {
    skip_if_not_installed("polars")
    snapshot <- member_prepare_package("polars")
    unrelated <- member_generic_index("")
    unrelated$package <- "unrelated"
    unrelated$exports <- c("scan_csv", "csv_file", "filter", "col", "group_by", "agg", "all", "median", "collect", "sum")
    lines <- c(
        "library(polars)", "",
        "csv_file <- tempfile(fileext = \".csv\")",
        "write.csv(iris, csv_file, row.names = FALSE)", "",
        "q <- pl$scan_csv(csv_file, infer_schema_files = 10)", "q # not working", "",
        "q1 <- q$filter(pl$col(\"Sepal.Length\") > 5)", "q1 # not working", "",
        "q1 <- q$filter(pl$col(\"Sepal.Length\") > 5)$group_by(\"Species\")$agg(pl$all()$median())$collect()",
        "q1 # not working", "",
        "q2 <- q1$group_by(\"Species\")$agg(pl$all()$sum())", "q2 # not working"
    )
    metadata <- MemberMetadataCache$new(32 * 1024^2)
    metadata$set("unrelated", as.list(member_index_thaw(member_index_freeze(unrelated))))
    metadata$set("polars", as.list(member_index_thaw(snapshot)))
    # Prepare in a clean worker, as production does, so S4 fixtures declared by
    # other tests do not inflate the methods namespace snapshot.
    methods_snapshot <- callr::r(function() languageserver:::member_prepare_package("methods"))
    metadata$set("methods", as.list(member_index_thaw(methods_snapshot)))
    expect_true(metadata$has("polars"))
    restored <- character()
    cache <- metadata
    metadata <- list(keys = cache$keys, catalog = cache$catalog, get = function(package) {
        if (!cache$.__enclos_env__$private$indexes$has(package)) restored <<- c(restored, package)
        cache$get(package)
    })
    for (row in c(6L, 9L, 12L, 15L)) {
        edited <- lines
        name <- sub(" .*", "", edited[[row + 1L]])
        edited[[row + 1L]] <- paste0(name, "$group_by(\"Species\", .maintain_order = ")
        fixture <- member_provider_fixture(edited)
        fixture$workspace$member_metadata <- metadata
        queried <- character()
        fixture$workspace$get_documentation <- function(key, package, isf, uri) {
            expect_identical(package, "polars")
            queried <<- c(queried, key)
            list(description = paste("Documentation for", key))
        }
        point <- list(row = row, col = nchar(name) + 1L)
        resolved <- member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document, point,
            member_cursor(fixture$document, point))
        expect_identical(resolved$value$type, if (row < 12L) "polars_lazy_frame" else "polars_data_frame")
        expect_false(resolved$budget$exhausted)
        items <- member_completion(fixture$uri, fixture$workspace, fixture$document, point, TRUE, 200L)
        labels <- vapply(items, `[[`, character(1L), "label")
        expect_true(all(c("filter", "group_by") %in% labels))
        expect_identical("collect" %in% labels, row < 12L)
        expect_identical("lazy" %in% labels, row >= 12L)
        item <- items[[match("group_by", labels)]]
        signature <- "group_by(..., .maintain_order = FALSE)"
        expect_identical(item$data$type, "member")
        expect_identical(item$data$package, "polars")
        expect_identical(item$detail, signature)
        expect_identical(member_provider_signature(fixture, row)$signatures[[1L]]$label, signature)
        hover <- member_provider_hover(fixture, row, nchar(name) + 3L)
        expect_identical(hover$contents[[1L]], sprintf("```r\n%s\n```", signature))
        expect_true(all(queried == if (row < 12L) "lazyframe__group_by" else "dataframe__group_by"))
    }
    expect_identical(restored, "polars")
})

test_that("Polars providers survive large metadata for several open documents over LSP", {
    skip_on_cran()
    skip_if_not_installed("polars")
    root <- withr::local_tempdir()
    # A test-only request reads the actual server cache. Namespace completion
    # can succeed before member metadata arrives, so it is not a readiness test.
    script <- paste(
        "languageserver:::lsp_settings$update_from_options()",
        "server <- languageserver:::LanguageServer$new('localhost', NULL)",
        "server$request_handlers[['test/memberMetadata']] <- function(self, id, params) {",
        "cache <- self$get_workspace(params$uri)$member_metadata",
        "ready <- all(c('polars', 'methods', 'R6') %in% cache$keys())",
        "if (ready && isTRUE(params$evict)) cache$get('methods')",
        "decoded <- cache$.__enclos_env__$private$indexes$keys()",
        "self$deliver(languageserver:::Response$new(id, result = list(ready = ready, decoded = decoded)))",
        "}", "server$run()", sep = "; "
    )
    original_new <- LanguageClient$new
    mockery::stub(language_client, "LanguageClient$new", function(command, args) {
        original_new(command, c("--no-echo", "-e", script))
    })
    client <- language_client(working_dir = root)
    path <- file.path(root, "polars.R")
    lines <- c(
        "library(polars)",
        "q <- pl$scan_csv(csv_file, infer_schema_files = 10)",
        "q1 <- q$filter(pl$col(\"Sepal.Length\") > 5)", "q1",
        "q1 <- q$filter(pl$col(\"Sepal.Length\") > 5)$group_by(\"Species\")$agg(pl$all()$median())$collect()",
        "q1", "q2 <- q1$group_by(\"Species\")$agg(pl$all()$sum())", "q2",
        "Leaf <- methods::setClass(\"Leaf\", slots = c(value = \"numeric\"))",
        "Dataset <- R6::R6Class(\"Dataset\", public = list(value = 1))"
    )
    did_open(client, path, text = lines)
    other <- file.path(root, "other.R")
    did_open(client, other, text = "methods::setClass(\"Other\", slots = c(value = \"numeric\"))")
    ready <- respond(client, "test/memberMetadata", list(uri = path_to_uri(path), evict = TRUE),
        timeout = if (identical(Sys.getenv("R_COVR"), "true")) 60 else 20,
        retry_when = function(result) !isTRUE(result$ready))
    expect_true(ready$ready)
    expect_true("methods" %in% ready$decoded)
    expect_false("polars" %in% ready$decoded)
    for (row in c(3L, 5L, 7L)) {
        edited <- lines
        name <- edited[[row + 1L]]
        edited[[row + 1L]] <- paste0(name, "$group_by(\"Species\", .maintain_order = ")
        notify(client, "textDocument/didChange", list(
            textDocument = list(uri = path_to_uri(path), version = row),
            contentChanges = list(list(text = paste(edited, collapse = "\n")))
        ))
        completion <- respond_completion(client, path, c(row, nchar(name) + 1L), retry = FALSE)
        expect_true(all(vapply(completion$items, function(item) identical(item$data$type, "member"), logical(1L))))
        labels <- vapply(completion$items, `[[`, character(1L), "label")
        expect_true(all(c("filter", "group_by") %in% labels))
        expect_identical("collect" %in% labels, row == 3L)
        expect_identical("lazy" %in% labels, row != 3L)
        signature <- respond_signature(client, path, c(row, nchar(edited[[row + 1L]])), retry = FALSE)
        expect_identical(signature$signatures[[1L]]$label, "group_by(..., .maintain_order = FALSE)")
        hover <- respond_hover(client, path, c(row, nchar(name) + 3L), retry = FALSE)
        expect_identical(hover$contents[[1L]], "```r\ngroup_by(..., .maintain_order = FALSE)\n```")
    }
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
