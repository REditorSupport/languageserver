classification_settings <- function(mode = "auto") {
    values <- list(index_mode = mode, index_persistent_cache = FALSE, diagnostics = FALSE)
    old <- stats::setNames(lapply(names(values), lsp_settings$get), names(values))
    withr::defer(for (name in names(old)) lsp_settings$set(name, old[[name]]), envir = parent.frame())
    for (name in names(values)) lsp_settings$set(name, values[[name]])
}

classification_manager <- function() {
    manager <- new.env(parent = emptyenv())
    manager$tasks <- collections::ordered_dict()
    manager$promoted <- character()
    manager$add_task <- function(id, task) {
        if (is.null(task)) manager$tasks$pop(id, NULL) else manager$tasks$set(id, task)
    }
    manager$cancel <- function(id) manager$tasks$pop(id, NULL)
    manager$promote <- function(id) manager$promoted <- c(manager$promoted, id)
    manager$has <- function(id) manager$tasks$has(id)
    manager$finish <- function(id = NULL) {
        if (!manager$tasks$size()) return(invisible(NULL))
        if (is.null(id)) id <- manager$tasks$keys()[[1L]]
        task <- manager$tasks$pop(id)$.__enclos_env__$private
        result <- do.call(task$target, task$args)
        if (is.function(task$callback)) task$callback(result)
        invisible(NULL)
    }
    manager
}

ClassificationServer <- R6::R6Class("ClassificationServer", inherit = LanguageServer,
    private = list(finalize = function() NULL), public = list(
        replies = NULL,
        initialize = function(root = NULL) {
            workspace <- testthat::with_mocked_bindings(Workspace$new(root),
                workspace_startup_packages = function() startup_packages,
                workspace_startup_s3 = function() list(), .package = "languageserver")
            self$workspaces <- collections::dict()
            self$workspaces$set(DEFAULT_WORKSPACE, workspace)
            self$workspace_cache <- collections::dict()
            self$index_task_manager <- classification_manager()
            self$parse_task_manager <- classification_manager()
            self$resolve_task_manager <- classification_manager()
            self$pending_replies <- collections::dict()
            self$replies <- list()
            self$load_workspace(workspace)
        },
        deliver = function(response, ...) {
            if (!is.null(response$id)) self$replies[[length(self$replies) + 1L]] <- response
        }
    ))

classification_open <- function(server, path) {
    uri <- path_to_uri(path)
    document <- Document$new(uri, "r", 1L, read_text_lines(path))
    document$did_open()
    server$get_workspace(uri)$documents$set(uri, document)
    server$text_sync(uri, document, parse = TRUE)
    document
}

classification_params <- function(document, col = 22L) {
    list(textDocument = list(uri = document$uri), position = position(0L, col),
        newName = "renamed", context = list(includeDeclaration = FALSE))
}

classification_step <- function(server, hold = character()) {
    for (workspace in server$workspaces$values()) workspace$index$process_batch()
    server$process_workspace_documents(time_budget_ms = Inf)
    server$index_task_manager$finish()
    ready <- setdiff(server$parse_task_manager$tasks$keys(), hold)
    if (length(ready)) server$parse_task_manager$finish(ready[[1L]])
    server$process_index_requests()
}

classification_drain <- function(server) {
    for (i in seq_len(200L)) {
        classification_step(server)
        if (!server$index_task_manager$tasks$size() && !server$parse_task_manager$tasks$size() &&
                (is.null(server$index_requests) || !server$index_requests$size()) &&
                all(vapply(server$workspaces$values(), function(workspace) workspace$index$complete(), logical(1L)))) return(invisible(NULL))
    }
    stop("classification requests did not settle")
}

test_that("pending local references and rename wait for parsing without discovery", {
    classification_settings()
    for (handler in list(text_document_references, text_document_rename)) {
        root <- index_normalize_path(withr::local_tempdir())
        path <- file.path(root, "main.R")
        writeLines("scope <- function() { value <- 1; value }", path)
        server <- ClassificationServer$new(root)
        document <- classification_open(server, path)
        workspace <- server$get_workspace(document$uri)
        handler(server, 1L, classification_params(document))
        expect_length(server$replies, 0L)
        expect_identical(server$index_requests$size(), 1L)
        expect_false(workspace$index$full_started)
        expect_identical(server$index_task_manager$tasks$size(), 0L)
        expect_true(document$uri %in% server$parse_task_manager$promoted)
        server$process_index_requests()
        expect_false(workspace$index$full_started)
        server$parse_task_manager$finish()
        server$process_index_requests()
        expect_false(workspace$index$full_started)
        expect_identical(server$index_requests$size(), 0L)
        expect_length(server$replies, 1L)
        reply <- server$replies[[1L]]
        expect_null(reply$error)
        if (identical(handler, text_document_rename)) {
            expect_length(reply$result$changes[[document$uri]], 2L)
            expect_true(all(vapply(reply$result$changes[[document$uri]], function(edit) edit$newText == "renamed", logical(1L))))
        } else {
            expect_length(reply$result, 1L)
            expect_identical(reply$result[[1L]]$range$start$character, 34L)
        }
    }
})

test_that("same-version stale parses do not classify current local requests", {
    classification_settings()
    for (handler in list(text_document_references, text_document_rename)) {
        root <- index_normalize_path(withr::local_tempdir())
        path <- file.path(root, "main.R")
        writeLines(paste0(strrep(" ", 22L), "global_name <- 1"), path)
        server <- ClassificationServer$new(root)
        document <- classification_open(server, path)
        server$parse_task_manager$finish()
        accepted_version <- document$version
        text_document_did_save(server, list(textDocument = list(uri = document$uri),
                text = "scope <- function() { value <- 1; value }"))
        expect_identical(document$version, accepted_version)
        expect_null(document$parse_data)
        expect_false(identical(document$content, document$parsed_content))
        handler(server, 1L, classification_params(document))
        server$process_index_requests()
        expect_false(server$get_workspace(document$uri)$index$full_started)
        expect_length(server$replies, 0L)
        server$parse_task_manager$finish()
        server$process_index_requests()
        expect_false(server$get_workspace(document$uri)$index$full_started)
        expect_length(server$replies, 1L)
        expect_null(server$replies[[1L]]$error)
        result <- server$replies[[1L]]$result
        if (identical(handler, text_document_rename)) {
            expect_length(result$changes[[document$uri]], 2L)
        } else {
            expect_identical(result[[1L]]$range$start$character, 34L)
        }
    }
})

test_that("pending global refactoring starts discovery after classification", {
    classification_settings()
    for (handler in list(text_document_references, text_document_rename)) {
        root <- index_normalize_path(withr::local_tempdir())
        path <- file.path(root, "main.R")
        caller <- file.path(root, "caller.R")
        writeLines("global_fn <- function(value) value", path)
        writeLines(c("source('main.R')", "global_fn(1)"), caller)
        server <- ClassificationServer$new(root)
        document <- classification_open(server, path)
        params <- classification_params(document, 3L)
        params$context$includeDeclaration <- TRUE
        handler(server, 1L, params)
        expect_false(server$get_workspace(document$uri)$index$full_started)
        server$parse_task_manager$finish()
        server$process_index_requests()
        expect_true(server$get_workspace(document$uri)$index$full_started)
        classification_drain(server)
        expect_length(server$replies, 1L)
        expect_null(server$replies[[1L]]$error)
        uris <- if (identical(handler, text_document_rename)) {
            names(server$replies[[1L]]$result$changes)
        } else {
            vapply(server$replies[[1L]]$result, `[[`, "", "uri")
        }
        expect_setequal(uris, c(document$uri, path_to_uri(caller)))
    }
})

test_that("pending classification remains usable without an enabled workspace index", {
    for (folderless in c(FALSE, TRUE)) {
        classification_settings("off")
        root <- index_normalize_path(withr::local_tempdir())
        path <- file.path(root, "main.R")
        writeLines("scope <- function() { value <- 1; value }", path)
        server <- ClassificationServer$new(if (folderless) NULL else root)
        document <- classification_open(server, path)
        text_document_references(server, 1L, classification_params(document))
        expect_identical(server$index_requests$size(), 1L)
        expect_true(document$uri %in% server$parse_task_manager$promoted)
        server$parse_task_manager$finish()
        server$process_index_requests()
        expect_length(server$replies, 1L)
        expect_null(server$replies[[1L]]$error)
        expect_length(server$replies[[1L]]$result, 1L)
    }
})

test_that("pending classification cancels changed snapshots and client cancellations", {
    classification_settings()
    for (change in c("content", "index", "workspace", "cancel")) {
        root <- index_normalize_path(withr::local_tempdir())
        path <- file.path(root, "main.R")
        writeLines("scope <- function() { value <- 1; value }", path)
        server <- ClassificationServer$new(root)
        document <- classification_open(server, path)
        workspace <- server$get_workspace(document$uri)
        text_document_references(server, "local-request", classification_params(document))
        if (change == "content") document$set_content(1L, "changed <- 1")
        if (change == "index") workspace$index <- WorkspaceIndex$new(root)
        if (change == "workspace") server$workspaces$clear()
        if (change == "cancel") cancel_request(server, list(id = "local-request"))
        server$process_index_requests()
        expect_identical(server$index_requests$size(), 0L)
        expect_length(server$replies, 1L)
        expect_identical(server$replies[[1L]]$error$code, -32800)
        expect_identical(server$replies[[1L]]$id, "local-request")
        expect_false(workspace$index$full_started)
    }
})

test_that("failed current parses settle pending refactoring without discovery", {
    classification_settings()
    root <- index_normalize_path(withr::local_tempdir())
    path <- file.path(root, "main.R")
    writeLines("scope <- function() { value <- 1; value }", path)
    server <- ClassificationServer$new(root)
    document <- classification_open(server, path)
    text_document_rename(server, 1L, classification_params(document))
    document$parse_failure <- list(version = document$version, message = "current parse failed")
    server$get_workspace(document$uri)$index$fail_parse(document$uri)
    server$parse_task_manager$cancel(document$uri)
    server$process_index_requests()
    expect_identical(server$index_requests$size(), 0L)
    expect_length(server$replies, 1L)
    expect_identical(server$replies[[1L]]$error$code, -32603)
    expect_false(server$get_workspace(document$uri)$index$full_started)
})

test_that("ready local requests are not starved behind pending workspace queries", {
    classification_settings()
    root <- index_normalize_path(withr::local_tempdir())
    path <- file.path(root, "main.R")
    writeLines("scope <- function() { value <- 1; value }", path)
    server <- ClassificationServer$new(root)
    document <- classification_open(server, path)
    for (id in seq_len(21L)) workspace_symbol(server, id, list(query = "local_fn"))
    text_document_references(server, 22L, classification_params(document))
    server$parse_task_manager$finish()
    server$process_index_requests(max_requests = 20L)
    server$process_index_requests(max_requests = 20L)
    expect_length(server$replies, 1L)
    expect_identical(server$replies[[1L]]$id, 22L)
    expect_null(server$replies[[1L]]$error)
    expect_length(server$replies[[1L]]$result, 1L)
    expect_identical(server$index_requests$size(), 21L)
    expect_true(server$get_workspace(document$uri)$index$discovering)
})

test_that("public formals and quoted semantic names discover unopened callers", {
    classification_settings()
    for (case in list(
        list(code = "api <- function(amount) amount", call = "api(amount = 1)", name = "amount", rename = TRUE),
        list(code = 'api <- list(); api[["run"]] <- function(x) x', call = "api$run(1)", name = "run", rename = TRUE),
        list(code = 'methods::setGeneric("draw", function(x) standardGeneric("draw"))',
            call = "draw(1)", name = "draw", rename = FALSE)
    )) {
        root <- index_normalize_path(withr::local_tempdir())
        path <- file.path(root, "api.R")
        caller <- file.path(root, "main.R")
        writeLines(case$code, path)
        writeLines(c('source("api.R")', case$call), caller)
        server <- ClassificationServer$new(root)
        document <- classification_open(server, path)
        params <- classification_params(document, as.integer(regexpr(case$name, case$code, fixed = TRUE)))
        params$context$includeDeclaration <- TRUE
        if (case$rename) text_document_rename(server, 1L, params) else text_document_references(server, 1L, params)
        classification_drain(server)
        reply <- server$replies[[1L]]
        expect_null(reply$error)
        locations <- if (case$rename) names(reply$result$changes) else vapply(reply$result, `[[`, "", "uri")
        expect_setequal(locations, c(document$uri, path_to_uri(caller)))
    }
})

test_that("a quoted key in a caller retrieves its unopened source declaration on first request", {
    classification_settings()
    root <- index_normalize_path(withr::local_tempdir())
    path <- file.path(root, "main.R")
    source <- file.path(root, "api.R")
    writeLines("api <- list(run = function(x) x)", source)
    writeLines(c('source("api.R")', 'api[["run"]](1)'), path)
    server <- ClassificationServer$new(root)
    document <- classification_open(server, path)
    params <- classification_params(document)
    params$position <- position(1L, 7L)
    params$context$includeDeclaration <- TRUE
    text_document_references(server, 1L, params)
    classification_drain(server)
    result <- server$replies[[1L]]
    expect_null(result$error)
    expect_setequal(vapply(result$result, `[[`, "", "uri"), c(document$uri, path_to_uri(source)))
})

test_that("anonymous callback parameters remain local without workspace discovery", {
    classification_settings()
    root <- index_normalize_path(withr::local_tempdir())
    path <- file.path(root, "main.R")
    code <- "out <- vapply(1:3, function(element) element, integer(1))"
    writeLines(code, path)
    writeLines('source("main.R")', file.path(root, "unused.R"))
    server <- ClassificationServer$new(root)
    document <- classification_open(server, path)
    server$parse_task_manager$finish()
    params <- classification_params(document, as.integer(regexpr("element", code, fixed = TRUE)))
    params$context$includeDeclaration <- TRUE
    text_document_references(server, 1L, params)
    expect_false(server$get_workspace(document$uri)$index$full_started)
    expect_length(server$replies[[1L]]$result, 2L)
    text_document_rename(server, 2L, params)
    expect_false(server$get_workspace(document$uri)$index$full_started)
    expect_length(server$replies[[2L]]$result$changes[[document$uri]], 2L)
})

classification_package_root <- function() {
    root <- index_normalize_path(withr::local_tempdir(.local_envir = parent.frame()))
    dir.create(file.path(root, "R"))
    writeLines("Package: reffixture", file.path(root, "DESCRIPTION"))
    root
}

test_that("package formal references preserve exact, partial and unsaved named callers", {
    classification_settings()
    root <- classification_package_root()
    source <- file.path(root, "R")
    path <- file.path(source, "api.R")
    writeLines(c("api <- function(allow_null) {", "  allow_null", "}"), path)
    writeLines(c("api(allow_null = FALSE)", "other(allow_null = TRUE)"), file.path(source, "exact.R"))
    writeLines("api(all = TRUE)", file.path(source, "partial.R"))
    buffer <- file.path(source, "buffer.R")
    writeLines("other <- function(allow_null) allow_null", buffer)
    changed <- file.path(source, "changed.R")
    writeLines("unused <- TRUE", changed)

    server <- ClassificationServer$new(root)
    document <- classification_open(server, path)
    params <- classification_params(document, 18L)
    references <- function(id, include) {
        params$context$includeDeclaration <- include
        text_document_references(server, id, params)
        classification_drain(server)
        reply <- server$replies[[id]]
        expect_null(reply$error)
        sort(vapply(reply$result, function(loc) {
            sprintf("%s %d:%d-%d:%d", basename(path_from_uri(loc$uri)),
                loc$range$start$line, loc$range$start$character, loc$range$end$line, loc$range$end$character)
        }, ""))
    }
    uses <- c("api.R 1:2-1:12", "exact.R 0:4-0:14", "partial.R 0:4-0:7")
    expect_identical(references(1L, TRUE), c("api.R 0:16-0:26", uses))
    expect_identical(references(2L, FALSE), uses)

    # A closed non-caller can gain a call in an unsaved editor buffer.
    classification_open(server, buffer)
    text_document_did_change(server, list(textDocument = list(uri = path_to_uri(buffer), version = 2L),
            contentChanges = list(list(text = "api(allow_n = TRUE)"))))
    expect_identical(references(3L, FALSE), sort(c(uses, "buffer.R 0:4-0:11")))
    text_document_did_change(server, list(textDocument = list(uri = path_to_uri(buffer), version = 3L),
            contentChanges = list(list(text = "api(TRUE)"))))
    expect_identical(references(4L, FALSE), uses)

    # The old disk summary excludes this file. Recovery still exposes the
    # callers on its complete lines, including a decoded tag and a glue call.
    writeLines(c('glue::glue("{api(allow_null = TRUE)}")', 'api("a\\u006cl" = TRUE)', "broken <- ("), changed)
    change <- list(uri = path_to_uri(changed), type = FileChangeType$Changed)
    workspace_did_change_watched_files(server, list(changes = list(change)))
    expect_identical(references(5L, FALSE), sort(c(uses, "changed.R 0:17-0:27", "changed.R 1:4-1:14")))
})

test_that("package formal references can reply while an unrelated full parse is pending", {
    classification_settings()
    root <- classification_package_root()
    source <- file.path(root, "R")
    path <- file.path(source, "api.R")
    caller <- file.path(source, "caller.R")
    unused <- file.path(source, "unused.R")
    writeLines("api <- function(allow_null) allow_null", path)
    writeLines("api(allow_null = TRUE)", caller)
    writeLines("unrelated <- function(value) value", unused)

    server <- ClassificationServer$new(root)
    document <- classification_open(server, path)
    params <- classification_params(document, 18L)
    blocked <- path_to_uri(unused)
    text_document_references(server, 1L, params)
    for (i in seq_len(50L)) {
        classification_step(server, hold = blocked)
        if (length(server$replies)) break
    }
    expect_length(server$replies, 1L)
    expect_null(server$replies[[1L]]$error)
    expect_setequal(vapply(server$replies[[1L]]$result, `[[`, "", "uri"), c(document$uri, path_to_uri(caller)))

    text_document_rename(server, 2L, params)
    for (i in seq_len(10L)) classification_step(server, hold = blocked)
    expect_true(server$parse_task_manager$has(blocked))
    expect_length(server$replies, 1L)
    classification_drain(server)
    expect_null(server$replies[[2L]]$error)
    expect_setequal(names(server$replies[[2L]]$result$changes), c(document$uri, path_to_uri(caller)))

    # Finish screening in a fresh server but keep the known caller pending.
    # A file that gains a call before the reply must now be parsed as well.
    disk_call <- "api(allow_n = FALSE)"
    for (close in c(FALSE, TRUE)) {
        writeLines(if (close) disk_call else "unrelated <- TRUE", unused)
        server <- ClassificationServer$new(root)
        document <- classification_open(server, path)
        if (close) {
            # The disk file now contains a call. An unsaved buffer hides it.
            text_document_did_open(server, list(textDocument = list(uri = blocked, languageId = "r",
                        version = 1L, text = "unrelated <- TRUE")))
        }
        text_document_references(server, 1L, params)
        for (i in seq_len(30L)) classification_step(server, hold = c(path_to_uri(caller), blocked))
        expect_length(server$replies, 0L)
        if (close) {
            text_document_did_close(server, list(textDocument = list(uri = blocked)))
            # The disk refresh hasn't run when the known caller is accepted.
            server$parse_task_manager$finish(path_to_uri(caller))
            server$process_index_requests()
        } else {
            writeLines(disk_call, unused)
            change <- list(uri = blocked, type = FileChangeType$Changed)
            workspace_did_change_watched_files(server, list(changes = list(change)))
            for (i in seq_len(10L)) classification_step(server, hold = blocked)
        }
        expect_length(server$replies, 0L)
        classification_drain(server)
        expect_null(server$replies[[1L]]$error)
        expect_setequal(vapply(server$replies[[1L]]$result, `[[`, "", "uri"), c(document$uri, path_to_uri(caller), blocked))
    }
})

test_that("glue callers can reference a package formal through an alias in R and Quarto", {
    classification_settings()
    root <- classification_package_root()
    source <- file.path(root, "R")
    path <- file.path(source, "api.R")
    writeLines("api <- function(allow_null) allow_null", path)
    alias <- file.path(source, "alias.R")
    writeLines("alias <- api", alias)
    path_peer <- file.path(source, "peer.R")
    peer <- path_to_uri(path_peer)
    call <- "glue::glue('{alias(allow_null = TRUE)}')"
    for (mode in c("r", "quarto", "reopen")) {
        # The raw R parser sees one string. Quarto sees a call inside an R cell.
        lines <- if (mode == "r") call else c('"', "```{r}", call, "```", '"')
        writeLines(lines, path_peer)
        server <- ClassificationServer$new(root)
        document <- classification_open(server, path)
        open <- list(textDocument = list(uri = peer, languageId = if (mode == "r") "r" else "quarto",
                version = 1L, text = paste(lines, collapse = "\n")))
        if (mode != "reopen") text_document_did_open(server, open)
        text_document_references(server, 1L, classification_params(document, 18L))
        if (mode == "reopen") {
            # Switch the screened peer to Quarto without changing its text.
            for (i in seq_len(30L)) classification_step(server, hold = c(path_to_uri(alias), peer))
            text_document_did_open(server, open)
        }
        for (i in seq_len(30L)) classification_step(server, hold = peer)
        expect_length(server$replies, 0L)
        classification_drain(server)
        expect_null(server$replies[[1L]]$error)
        hits <- Filter(function(loc) loc$uri == peer, server$replies[[1L]]$result)
        expect_length(hits, 1L)
        line <- if (mode == "r") 0L else 2L
        expect_equal(hits[[1L]]$range, range(position(line, 19L), position(line, 29L)))
    }
})
