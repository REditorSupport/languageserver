test_that("Completion providers use precomputed document indexes", {
    fixture <- provider_fixture(c(
        "xvar0 <- rnorm(10)",
        "my_fun <- function(xvar1) {",
        "    xvar2 = 1",
        "    2 -> xvar3",
        "    for (xvar4 in 1:10) {",
        "        xvar",
        "    }",
        "}"
    ))
    parse_data <- fixture$workspace$get_parse_data(fixture$uri)

    # Prove these providers do not need to traverse the XML document.
    parse_data$xml_doc <- NULL
    scope_items <- scope_completion(
        fixture$uri,
        fixture$workspace,
        "xvar",
        list(row = 5L, col = 12L)
    )
    scope_labels <- vapply(scope_items, `[[`, character(1L), "label")
    expect_setequal(scope_labels, c("xvar1", "xvar2", "xvar3", "xvar4"))

    token_items <- token_completion(
        fixture$uri, fixture$workspace, "xvar")
    token_labels <- vapply(token_items, `[[`, character(1L), "label")
    expect_setequal(token_labels,
        c("xvar0", "xvar1", "xvar2", "xvar3", "xvar4", "xvar"))
})

test_that("Completion providers bound broad result sets early", {
    variables <- sprintf("    value_%04d <- %d", 1:500, 1:500)
    fixture <- provider_fixture(c(
        "my_fun <- function() {",
        variables,
        "    value_",
        "}"
    ))

    items <- scope_completion(
        fixture$uri,
        fixture$workspace,
        "value_",
        list(row = 501L, col = 10L),
        limit = 20L
    )

    expect_length(items, 20L)
    expect_true(isTRUE(attr(items, "truncated")))
    expect_equal(
        sort(vapply(items, `[[`, character(1L), "label")),
        sprintf("value_%04d", 1:20)
    )

    namespace <- new.env(parent = baseenv())
    namespace$get_symbols <- function(want_functs, ...) {
        if (want_functs) {
            sprintf("value_function_%04d", 500:1)
        } else {
            sprintf("value_field_%04d", 500:1)
        }
    }
    namespace$get_lazydata <- function() sprintf("value_data_%04d", 500:1)
    workspace <- new.env(parent = baseenv())
    workspace$get_namespace <- function(...) namespace

    workspace_items <- workspace_completion(
        workspace,
        "value_",
        package = "example",
        exported_only = TRUE,
        limit = 20L
    )
    all_labels <- c(
        namespace$get_symbols(TRUE),
        namespace$get_symbols(FALSE),
        namespace$get_lazydata()
    )
    expected <- all_labels[
        order(paste0(sort_prefixes$global, all_labels), method = "radix")
    ][1:20]

    expect_length(workspace_items, 20L)
    expect_true(isTRUE(attr(workspace_items, "truncated")))
    expect_equal(
        vapply(workspace_items, `[[`, character(1L), "label"),
        expected
    )
})

test_that("Completion candidate selection uses stable UTF-8 radix ordering", {
    labels <- c("zeta", "äther", "Alpha", ".hidden", "_private", "alpha")
    sort_text <- paste0(sort_prefixes$global, labels)
    token <- "a"
    expected <- order(
        !startsWith(labels, token), sort_text, method = "radix")[1:4]

    expect_identical(
        completion_select_indices(labels, sort_text, token, 4L),
        expected
    )
})

test_that("Argument value completion resolves formals once", {
    calls <- 0L
    workspace <- new.env(parent = baseenv())
    workspace$guess_namespace <- function(...) "example"
    workspace$get_formals <- function(...) {
        calls <<- calls + 1L
        alist(
            method = c("auto", "manual"),
            style = c("plain", "fancy")
        )
    }

    items <- arg_value_completion(
        NULL, workspace, NULL, NULL, "a", "my_fun")

    expect_equal(calls, 1L)
    expect_setequal(
        vapply(items, `[[`, character(1L), "label"),
        c("auto", "manual", "plain", "fancy")
    )
})

test_that("Completion parse index handles supported symbol forms", {
    parse_data <- parse_document("file:///completion-index.R", c(
        "# assignments",
        "left_value <- 1",
        "2 -> right_value",
        "equal_value = 3",
        "left_fun <- function(argument) argument",
        "lambda_fun <- \\(lambda_argument) lambda_argument",
        "for (loop_value in 1:3) print(loop_value)",
        "object$member",
        "target(named = 1)"
    ))$completion_data

    expect_setequal(parse_data$symbols$name,
        c("left_value", "right_value", "equal_value", "loop_value"))
    expect_setequal(parse_data$functions$name, c("left_fun", "lambda_fun"))
    expect_setequal(parse_data$formals$name,
        c("argument", "lambda_argument"))
    expect_setequal(parse_data$empty_tokens, c("member", "named"))
})

completion_test_namespace <- function(name, functions = character(),
    values = character(), lazydata = character()) {
    namespace <- new.env(parent = baseenv())
    namespace$package_name <- name
    namespace$get_symbols <- function(want_functs, exported_only = TRUE) {
        if (want_functs) functions else values
    }
    namespace$get_lazydata <- function() lazydata
    namespace$exists_funct <- function(object) object %in% functions
    namespace
}

test_that("Object completions quote non-syntactic names", {
    namespace <- completion_test_namespace(
        "example", functions = "run task", values = "Mazda RX4"
    )
    workspace <- new.env(parent = baseenv())
    workspace$loaded_packages <- "example"
    workspace$imported_objects <- collections::dict()
    workspace$imported_objects$set("run task", "example")
    workspace$get_namespace <- function(name) {
        if (identical(name, "example")) namespace else NULL
    }

    for (snippets in c(FALSE, TRUE)) {
        expected <- if (snippets) "`run task`($0)" else "`run task`"
        item <- ns_function_completion(namespace, "run", TRUE, snippets)[[1L]]
        expect_identical(item$label, "run task")
        expect_identical(item$insertText, expected)
        item <- imported_object_completion(workspace, "run", snippets)[[1L]]
        expect_identical(item$insertText, expected)
        items <- workspace_completion(workspace, "", snippet_support = snippets)
        functions <- Filter(function(item) identical(item$label, "run task"), items)
        expect_length(functions, 2L)
        expect_true(all(vapply(functions, function(item) identical(item$insertText, expected), logical(1L))))
        value <- Filter(function(item) identical(item$label, "Mazda RX4"), items)[[1L]]
        expect_identical(value$insertText, "`Mazda RX4`")
        expect_identical(as.character(parse(text = value$insertText)[[1L]]), value$label)
    }
})

test_that("Argument completions quote names from namespace formals", {
    fixture <- provider_fixture("target(", formals_resolver = function(...) {
        alist(`a b` = , `if` = , normal = )
    })
    items <- arg_completion(fixture$uri, fixture$workspace, list(row = 0L, col = 7L),
        "", "target", package = "example")
    expect_identical(vapply(items, `[[`, character(1L), "insertText"),
        c("`a b` = ", "`if` = ", "normal = "))
    for (item in items) {
        call <- parse(text = paste0("target(", item$insertText, "1)"))[[1L]]
        expect_identical(names(call)[[2L]], item$label)
    }

    fixture <- provider_fixture(c("local({", "target <- function(`a b`) NULL", "target(", ")", "})"))
    items <- arg_completion(fixture$uri, fixture$workspace, list(row = 2L, col = 7L),
        "", "target")
    expect_identical(items[[1L]]$insertText, "`a b` = ")
})

test_that("LSP completion quotes names without duplicating existing calls", {
    fixture <- provider_fixture(c("example::run", "example::run()"))
    namespace <- completion_test_namespace("example", functions = "run task")
    fixture$workspace$get_namespace <- function(name) namespace
    for (snippets in c(FALSE, TRUE)) {
        for (row in 0:1) {
            items <- completion_reply(1L, fixture$uri, fixture$workspace, fixture$document,
                list(row = row, col = 12L),
                list(completionItem = list(snippetSupport = snippets)))$result$items
            expect_length(items, 1L)
            expect_identical(items[[1L]]$label, "run task")
            expect_identical(items[[1L]]$insertText,
                if (snippets && row == 0L) "`run task`($0)" else "`run task`")
        }
    }
})

test_that("Function completions preserve existing call parentheses", {
    fixture <- provider_fixture(c("mea(x)", "mea"))
    namespace <- completion_test_namespace("base", functions = "mean")
    fixture$workspace$loaded_packages <- "base"
    fixture$workspace$imported_objects <- collections::dict()
    fixture$workspace$get_namespace <- function(name) {
        if (identical(name, "base")) namespace else NULL
    }
    capabilities <- list(completionItem = list(snippetSupport = TRUE))

    complete_at <- function(row, col) {
        completion_reply(
            1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = row, col = col), capabilities
        )$result$items
    }

    existing_call_items <- complete_at(0L, 3L)
    existing_call_mean <- keep(existing_call_items, ~ .$label == "mean")
    expect_length(existing_call_mean, 1L)
    expect_null(existing_call_mean[[1L]]$insertText)

    bare_items <- complete_at(1L, 3L)
    bare_mean <- keep(bare_items, ~ .$label == "mean")
    expect_length(bare_mean, 1L)
    expect_equal(bare_mean[[1L]]$insertText, "mean($0)")
    expect_equal(bare_mean[[1L]]$insertTextFormat, InsertTextFormat$Snippet)
})

test_that("Quoted namespace completion replaces the complete source name", {
    namespace <- completion_test_namespace("example", functions = "run task")
    cases <- list(
        list(text = "example::`run", col = 13L, end = 13L, call = FALSE),
        list(text = "example::`run`", col = 13L, end = 14L, call = FALSE),
        list(text = "example::`run`", col = 14L, end = 14L, call = FALSE),
        list(text = "example::`run old`(1)", col = 13L, end = 18L, call = TRUE),
        list(text = "example::`", col = 10L, end = 10L, call = FALSE),
        list(text = "example:::`run", col = 14L, end = 14L, call = FALSE)
    )
    for (case in cases) {
        fixture <- provider_fixture(case$text)
        fixture$workspace$get_namespace <- function(name) {
            if (identical(name, "example")) namespace else NULL
        }
        start <- if (startsWith(case$text, "example:::")) 10L else 9L
        for (snippets in c(FALSE, TRUE)) {
            items <- completion_reply(1L, fixture$uri, fixture$workspace,
                fixture$document, list(row = 0L, col = case$col),
                list(completionItem = list(snippetSupport = snippets)))$result$items
            expect_length(items, 1L)
            item <- items[[1L]]
            expect_identical(item$label, "run task")
            expect_identical(item$filterText, "`run task`")
            expect_identical(item$textEdit$range, range(
                position(0L, start), position(0L, case$end)))
            snippet <- snippets && !case$call
            expect_identical(item$textEdit$newText,
                if (snippet) "`run task`($0)" else "`run task`")
            inserted <- sub("($0)", "()", item$textEdit$newText, fixed = TRUE)
            code <- paste0(substr(case$text, 1L, start), inserted,
                substring(case$text, case$end + 1L))
            expr <- parse(text = code)[[1L]]
            if (snippet || case$call) expr <- expr[[1L]]
            expect_identical(as.character(expr[[3L]]), "run task")
        }
    }
})

test_that("Namespace indexes retain names that need quoting", {
    namespace <- PackageNamespace$new("base")
    expect_true("[<-" %in% namespace$get_symbols(want_functs = TRUE))
    expect_true(namespace$exists_funct("[<-"))
    items <- ns_function_completion(namespace, "[<-", TRUE, FALSE)
    item <- Filter(function(item) identical(item$label, "[<-"), items)[[1L]]
    expect_identical(item$insertText, "`[<-`")
})

test_that("Quoted workspace and imported names complete with decoded prefixes", {
    names <- c("run task", "a`b", "`lead", "a\\b", "a\nb", paste0(intToUtf8(0xe9), " task"))
    for (name in names) {
        quoted <- encodeString(name, quote = "`")
        prefix <- substr(quoted, 1L, nchar(quoted) - 1L)
        fixture <- provider_fixture(prefix)
        namespace <- completion_test_namespace("example", values = name)
        fixture$workspace$loaded_packages <- "example"
        fixture$workspace$imported_objects <- collections::dict()
        fixture$workspace$get_namespace <- function(package) {
            if (identical(package, "example")) namespace else NULL
        }
        items <- completion_reply(1L, fixture$uri, fixture$workspace,
            fixture$document, list(row = 0L, col = nchar(prefix)), list())$result$items
        items <- Filter(function(item) identical(item$label, name), items)
        expect_length(items, 1L)
        expect_identical(items[[1L]]$label, name)
        expect_identical(items[[1L]]$filterText, quoted)
        expect_identical(items[[1L]]$textEdit$newText, quoted)
        expect_identical(as.character(parse(text = items[[1L]]$textEdit$newText)[[1L]]), name)
    }

    fixture <- provider_fixture("`run")
    namespace <- completion_test_namespace("example", functions = "run task")
    fixture$workspace$loaded_packages <- character()
    fixture$workspace$imported_objects <- collections::dict()
    fixture$workspace$imported_objects$set("run task", "example")
    fixture$workspace$get_namespace <- function(package) {
        if (identical(package, "example")) namespace else NULL
    }
    items <- completion_reply(1L, fixture$uri, fixture$workspace,
        fixture$document, list(row = 0L, col = 4L), list())$result$items
    expect_length(items, 1L)
    expect_identical(items[[1L]]$textEdit$newText, "`run task`")
})

test_that("Quoted completion edit ranges use UTF-16 positions", {
    text <- paste0('"', intToUtf8(0x1f680), '"; example::`run old`(1)')
    fixture <- provider_fixture(text)
    namespace <- completion_test_namespace("example", functions = "run task")
    fixture$workspace$get_namespace <- function(name) namespace
    items <- completion_reply(1L, fixture$uri, fixture$workspace, fixture$document,
        list(row = 0L, col = 18L), list())$result$items
    expect_length(items, 1L)
    expect_identical(items[[1L]]$textEdit$range,
        range(position(0L, 15L), position(0L, 24L)))
    expect_identical(items[[1L]]$textEdit$newText, "`run task`")
})

test_that("Quoted local names and argument prefixes retain their context", {
    fixture <- provider_fixture(c("local({", "target <- function(`a b`) NULL",
            "`run task` <- function() NULL", "target(`a`)", "`run`", "})"))
    fixture$workspace$loaded_packages <- character()
    fixture$workspace$imported_objects <- collections::dict()
    fixture$workspace$get_namespace <- function(...) NULL
    fixture$workspace$guess_namespace <- function(...) NULL
    capabilities <- list(completionItem = list(snippetSupport = TRUE))

    items <- completion_reply(1L, fixture$uri, fixture$workspace, fixture$document,
        list(row = 3L, col = 9L), capabilities)$result$items
    item <- Filter(function(item) identical(item$detail, "parameter"), items)[[1L]]
    expect_identical(item$textEdit$newText, "`a b` = ")
    expect_identical(item$textEdit$range, range(position(3L, 7L), position(3L, 10L)))

    items <- completion_reply(1L, fixture$uri, fixture$workspace, fixture$document,
        list(row = 4L, col = 4L), capabilities)$result$items
    item <- Filter(function(item) identical(item$detail, "[scope]"), items)[[1L]]
    expect_identical(item$textEdit$newText, "`run task`($0)")
    expect_identical(item$filterText, "`run task`")
})

test_that("Quoted namespace completion works through the language client", {
    skip_on_cran()
    working_dir <- withr::local_tempdir()
    client <- language_client(working_dir = working_dir)
    path <- withr::local_tempfile(fileext = ".R")
    writeLines("base::`row", path)
    client %>% did_open(path)

    result <- client %>% respond_completion(path, c(0L, 10L))
    item <- keep(result$items, ~ .$label == "row.names")
    expect_length(item, 1L)
    expect_equal(item[[1L]]$textEdit$range,
        range(position(0L, 6L), position(0L, 10L)), ignore_attr = TRUE)

    call_path <- withr::local_tempfile(fileext = ".R")
    writeLines("base::`[<`(x, 1, value = 2)", call_path)
    client %>% did_open(call_path)
    result <- client %>% respond_completion(call_path, c(0L, 9L))
    item <- keep(result$items, ~ .$label == "[<-")
    expect_length(item, 1L)
    expect_equal(item[[1L]]$textEdit$range,
        range(position(0L, 6L), position(0L, 10L)), ignore_attr = TRUE)
    expect_identical(item[[1L]]$textEdit$newText, "`[<-`")
})

test_that("Namespace completions distinguish workspace and package functions", {
    package <- completion_test_namespace(
        "example", functions = c("alpha", "beta")
    )
    workspace <- completion_test_namespace(
        WORKSPACE, functions = "alpha_workspace"
    )

    package_items <- ns_function_completion(package, "al", TRUE, TRUE)
    expect_length(package_items, 1L)
    expect_equal(package_items[[1L]]$detail, "{example}")
    expect_equal(package_items[[1L]]$insertText, "alpha($0)")
    expect_equal(package_items[[1L]]$insertTextFormat, InsertTextFormat$Snippet)

    workspace_items <- ns_function_completion(
        workspace, "workspace", TRUE, FALSE
    )
    expect_length(workspace_items, 1L)
    expect_equal(workspace_items[[1L]]$detail, "[workspace]")
    expect_null(workspace_items[[1L]]$insertText)
})

test_that("Imported completions skip missing and non-function namespaces", {
    imports <- collections::dict()
    imports$set("alpha", "example")
    imports$set("value", "example")
    imports$set("missing", "missing-package")
    namespace <- completion_test_namespace(
        "example", functions = "alpha", values = "value"
    )
    workspace <- new.env(parent = baseenv())
    workspace$imported_objects <- imports
    workspace$get_namespace <- function(name) {
        if (identical(name, "example")) namespace else NULL
    }

    items <- imported_object_completion(workspace, "a", TRUE)
    expect_length(items, 1L)
    expect_equal(items[[1L]]$label, "alpha")
    expect_equal(items[[1L]]$insertText, "alpha($0)")

    plain <- imported_object_completion(workspace, "alpha", FALSE)
    expect_null(plain[[1L]]$insertText)
    expect_null(imported_object_completion(workspace, "unmatched", TRUE))
})

test_that("Workspace completion combines namespaces, imports, and limits", {
    imports <- collections::dict()
    imports$set("imported_fun", "example")
    global <- completion_test_namespace(
        WORKSPACE,
        functions = c("global_fun", "global_other"),
        values = "global_value"
    )
    package <- completion_test_namespace(
        "example",
        functions = c("exported_fun", "imported_fun"),
        values = "exported_value",
        lazydata = "example_data"
    )
    workspace <- new.env(parent = baseenv())
    workspace$loaded_packages <- "example"
    workspace$imported_objects <- imports
    workspace$get_namespace <- function(name) {
        if (identical(name, WORKSPACE)) global else if (identical(name, "example")) package
    }

    items <- workspace_completion(
        workspace, "", snippet_support = TRUE, limit = 4L
    )
    expect_length(items, 4L)
    expect_true(isTRUE(attr(items, "truncated")))
    expect_true(all(vapply(items, function(item) {
        !is.null(item$label) && !is.null(item$data$type)
    }, logical(1L))))

    private_items <- workspace_completion(
        workspace, "exported", package = "example",
        exported_only = FALSE, snippet_support = FALSE
    )
    expect_setequal(
        vapply(private_items, `[[`, character(1L), "label"),
        c("exported_fun", "exported_value")
    )
    expect_identical(
        workspace_completion(
            workspace, "nothing-matches", package = "example"
        ),
        list()
    )
})

test_that("Argument value completion accepts only literal character defaults", {
    defaults <- quote(c("first", I("second"), 3, identity("ignored")))
    expect_identical(extract_default_values(defaults), c("first", "second"))
    expect_identical(extract_default_values("single"), "single")
    expect_null(extract_default_values(quote(c(1, 2))))
    missing_default <- alist(value = )[[1L]]
    expect_null(extract_default_values(missing_default))

    workspace <- new.env(parent = baseenv())
    workspace$get_formals <- function(...) alist(
        mode = c("auto", "manual"),
        count = 1L
    )
    expect_identical(
        argument_value_completion(
            workspace, "fun", NULL, "missing", "", formals_list = list()
        ),
        list()
    )
    items <- argument_value_completion(
        workspace, "fun", NULL, "mode", "man"
    )
    expect_length(items, 1L)
    expect_equal(items[[1L]]$label, "manual")
    expect_equal(items[[1L]]$insertText, '"manual"')
})

test_that("Indexed and XML scope completions agree on local symbols", {
    content <- c(
        "outer <- function(argument) {",
        "  local_value <- 1",
        "  local_fun <- function() local_value",
        "  local_value",
        "}"
    )
    fixture <- provider_fixture(content)
    point <- list(row = 3L, col = 8L)

    indexed <- scope_completion(
        fixture$uri, fixture$workspace, "local_", point,
        snippet_support = TRUE
    )
    expect_length(indexed, 2L)

    limited <- scope_completion(
        fixture$uri, fixture$workspace, "local_", point,
        snippet_support = TRUE, limit = 1L
    )
    expect_length(limited, 1L)
    expect_true(isTRUE(attr(limited, "truncated")))

    parse_data <- fixture$document$parse_data
    parse_data$completion_data <- NULL
    legacy_workspace <- new.env(parent = baseenv())
    legacy_workspace$get_parse_data <- function(...) parse_data
    legacy <- scope_completion(
        fixture$uri, legacy_workspace, "local_", point,
        snippet_support = FALSE
    )
    expect_setequal(
        vapply(legacy, `[[`, character(1L), "label"),
        vapply(indexed, `[[`, character(1L), "label")
    )

    parse_data$xml_doc <- NULL
    restored <- scope_completion(fixture$uri, legacy_workspace, "local_", point,
        snippet_support = FALSE)
    expect_setequal(vapply(restored, `[[`, character(1L), "label"),
        vapply(legacy, `[[`, character(1L), "label"))
    expect_s3_class(parse_data$xml_doc, "xml_document")
    parse_data$xml_doc <- parse_data$xml_data <- NULL
    expect_identical(
        scope_completion(fixture$uri, legacy_workspace, "x", point),
        list()
    )
})

test_that("Token completion supports indexed and XML parse data", {
    content <- c("object$member", "target(named = 1)", "member_other <- 2")
    fixture <- provider_fixture(content)

    indexed <- token_completion(
        fixture$uri, fixture$workspace, "mem", exclude = "member_other",
        limit = 1L
    )
    expect_length(indexed, 1L)
    expect_equal(indexed[[1L]]$label, "member")

    parse_data <- fixture$document$parse_data
    parse_data$completion_data <- NULL
    legacy_workspace <- list(get_parse_data = function(...) parse_data)
    legacy <- token_completion(fixture$uri, legacy_workspace, "mem")
    expect_true("member" %in% vapply(legacy, `[[`, character(1L), "label"))

    parse_data$xml_doc <- NULL
    restored <- token_completion(fixture$uri, legacy_workspace, "mem")
    expect_equal(restored, legacy)
    expect_s3_class(parse_data$xml_doc, "xml_document")
    parse_data$xml_doc <- parse_data$xml_data <- NULL
    expect_identical(
        token_completion(fixture$uri, legacy_workspace, "mem"),
        list()
    )
})

test_that("Static chained members are delivered over LSP without evaluating arguments", {
    skip_on_cran()
    client <- language_client()
    temp_file <- withr::local_tempfile(fileext=".R")
    text <- paste0("factory <- function(x) list(step=function() list(finish=x))\n",
        "factory(stop(\"do not execute\"))$step()$")
    client %>% did_open(temp_file,text=text)
    result <- client %>% respond_completion(temp_file,c(1L,nchar(strsplit(text,"\n",fixed=TRUE)[[1L]][[2L]])))
    expect_true("finish" %in% vapply(result$items,`[[`,character(1L),"label"))
    member <- result$items[[which(vapply(result$items,`[[`,character(1L),"label") == "finish")]]
    expect_identical(member$data$type,"member")
})

test_that("Installed Polars metadata is prepared before chained LSP completion", {
    skip_on_cran()
    skip_if_not_installed("polars")
    client <- language_client()
    temp_file <- withr::local_tempfile(fileext=".R")
    lines <- c("library(polars)", "q <- pl$scan_csv(csv_file)$filter(predicate)", "q$collect")
    client %>% did_open(temp_file,text=paste(lines,collapse="\n"))
    # Resolution is asynchronous; requests never initialize the package.
    deadline <- Sys.time() + 15
    repeat {
        result <- client %>% respond_completion(temp_file,c(2L,2L))
        labels <- vapply(result$items,`[[`,character(1L),"label")
        if ("collect" %in% labels && any(vapply(result$items,function(x) identical(x$data$type,"member"),logical(1L)))) break
        if (Sys.time() > deadline) break
        Sys.sleep(0.1)
    }
    expect_true("group_by" %in% labels)
    expect_true(any(vapply(result$items,function(x) identical(x$data$type,"member"),logical(1L))))
})

test_that("The first dollar trigger after an edit uses current Polars members", {
    skip_on_cran()
    skip_if_not_installed("polars")
    client <- language_client()
    temp_file <- withr::local_tempfile(fileext = ".R")
    uri <- path_to_uri(temp_file)
    lines <- c(
        "library(polars)", "",
        "csv_file <- tempfile(fileext = \".csv\")",
        "write.csv(iris, csv_file, row.names = FALSE)", "",
        "q <- pl$scan_csv(csv_file, infer_schema_files = 10)",
        "q$group_by(\"Species\")"
    )
    client %>% did_open(temp_file, text = paste(lines, collapse = "\n"))
    # Warm only package metadata; the tested completion follows didChange with
    # no sleep, request retry, or second dollar trigger. A long debounce makes
    # the request arrive before the updated document has been parsed.
    deadline <- Sys.time() + 15
    repeat {
        ready <- respond_completion(client, temp_file, c(6L, 2L), retry = FALSE)
        if (any(vapply(ready$items, function(item) identical(item$data$type, "member"), logical(1L)))) break
        if (Sys.time() > deadline) break
        Sys.sleep(0.1)
    }
    expect_true(any(vapply(ready$items, function(item) identical(item$data$type, "member"), logical(1L))))
    notify(client, "workspace/didChangeConfiguration", list(settings = list(parse_delay = 0.5)))
    end <- list(line = 6L, character = nchar(lines[[7L]]))
    notify(client, "textDocument/didChange", list(
        textDocument = list(uri = uri, version = 2L),
        contentChanges = list(list(range = list(start = end, end = end), text = "$"))
    ))
    result <- respond(client, "textDocument/completion", list(
        textDocument = list(uri = uri),
        position = list(line = 6L, character = end$character + 1L),
        context = list(triggerKind = 2L, triggerCharacter = "$")
    ), retry = FALSE)
    labels <- vapply(result$items, `[[`, character(1L), "label")
    expect_true("agg" %in% labels)
    expect_false(any(c("fileext", "infer_schema_files", "row.names") %in% labels))
    expect_true(all(vapply(result$items, function(item) identical(item$data$type, "member"), logical(1L))))
})

test_that("The first dollar trigger completes assigned and collected Polars queries", {
    skip_on_cran()
    skip_if_not_installed("polars")
    client <- language_client()
    temp_file <- withr::local_tempfile(fileext = ".R")
    uri <- path_to_uri(temp_file)
    lines <- c(
        "library(polars)", "",
        "csv_file <- tempfile(fileext = \".csv\")",
        "write.csv(iris, csv_file, row.names = FALSE)", "",
        "q <- pl$scan_csv(csv_file, infer_schema_files = 10)", "",
        "q1 <- q$filter(pl$col(\"Sepal.Length\") > 5)", "q1", "",
        "q1 <- q$filter(pl$col(\"Sepal.Length\") > 5)$group_by(\"Species\")$agg(pl$all()$median())$collect()",
        "q1 # collected", "",
        "q2 <- q1$group_by(\"Species\")$agg(pl$all()$sum())", "q2 # collected"
    )
    did_open(client, temp_file, text = paste(lines, collapse = "\n"))
    # Prepare package metadata using only q; q1 and q2 must complete on their
    # first request, without warming their method summaries or retrying.
    deadline <- Sys.time() + 15
    repeat {
        ready <- respond_completion(client, temp_file, c(7L, 8L), retry = FALSE)
        if (any(vapply(ready$items, function(item) identical(item$data$type, "member"), logical(1L)))) break
        if (Sys.time() > deadline) break
        Sys.sleep(0.1)
    }
    expect_true(any(vapply(ready$items, function(item) identical(item$data$type, "member"), logical(1L))))
    notify(client, "workspace/didChangeConfiguration", list(settings = list(parse_delay = 0.5)))
    for (row in c(8L, 11L, 14L)) {
        end <- list(line = row, character = 2L)
        notify(client, "textDocument/didChange", list(
            textDocument = list(uri = uri, version = row),
            contentChanges = list(list(range = list(start = end, end = end), text = "$"))
        ))
        result <- respond(client, "textDocument/completion", list(
            textDocument = list(uri = uri),
            position = list(line = row, character = end$character + 1L),
            context = list(triggerKind = 2L, triggerCharacter = "$")
        ), retry = FALSE)
        labels <- vapply(result$items, `[[`, character(1L), "label")
        expect_true(all(c("filter", "group_by") %in% labels))
        expect_identical("collect" %in% labels, row == 8L)
        expect_identical("lazy" %in% labels, row != 8L)
        expect_true(all(vapply(result$items, function(item) identical(item$data$type, "member"), logical(1L))))
        expect_false(any(c("fileext", "infer_schema_files", "row.names") %in% labels))
        # Restore a complete statement before editing the next receiver.
        notify(client, "textDocument/didChange", list(
            textDocument = list(uri = uri, version = row + 1L),
            contentChanges = list(list(
                range = list(start = end, end = list(line = row, character = end$character + 1L)),
                text = ""
            ))
        ))
    }
})
