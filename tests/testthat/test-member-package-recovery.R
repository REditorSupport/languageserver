test_that("Incomplete namespace members retain syntax-confirmed package requests", {
    for (code in c("recoveryfixture::frame$", "recoveryfixture:::object@",
            "f(recoveryfixture::frame$", "recoveryfixture::frame$ # trailing comment",
            "function() { recoveryfixture::frame$", "bad <- )\nrecoveryfixture::frame$",
            "recoveryfixture::frame$\n# trailing comment")) {
        content <- strsplit(code, "\n", fixed = TRUE)[[1L]]
        parsed <- parse_document("file:///package-recovery.R", content)
        expect_true(parsed$parse_error, info = code)
        expect_identical(parsed$member_data$packages, "recoveryfixture", info = code)
    }
    expect_identical(member_incomplete_packages('"recoveryfixture"::frame$'), "recoveryfixture")
    expect_identical(member_incomplete_packages("f(firstfixture::frame, secondfixture::frame$"), "secondfixture")
})

test_that("Package recovery ignores comments, strings, and unrelated incomplete syntax", {
    for (content in list("# recoveryfixture::frame$", '"recoveryfixture::frame$"',
            '"recoveryfixture::frame$', "`recoveryfixture::frame$`",
            'f("recoveryfixture::frame$")$', "recoveryfixture::frame",
            "opaque(recoveryfixture", "`::`()$")) {
        expect_length(member_incomplete_packages(content), 0L)
    }
    expect_length(member_incomplete_packages(rep("# comment", 130L)), 0L)
    expect_length(member_incomplete_packages(strrep(" ", 65537L)), 0L)
})

test_that("Recovered namespace requests do not create source bindings", {
    content <- "data <- recoveryfixture::frame$"
    index <- member_document_index(content)
    expect_identical(index$packages, "recoveryfixture")
    expect_length(index$items, 0L)
    expect_length(index$bindings, 0L)
    expect_length(index$imports, 0L)
})

test_that("Parse recovery schedules namespace metadata without changing valid imports", {
    uri <- "file:///package-recovery-routing.R"
    doc <- Document$new(uri, language = "r", version = 1L, content = "recoveryfixture::frame$")
    workspace <- new.env(parent = emptyenv())
    workspace$documents <- collections::dict()
    workspace$documents$set(uri, doc)
    workspace$index <- list(topology_revision = 0L, enabled = FALSE)
    workspace$update_parse_data <- function(uri, data) doc$update_parse_data(data)
    workspace$update_loaded_packages <- function() stop("Incomplete syntax must retain valid imports")
    server <- new.env(parent = emptyenv())
    server$get_workspace <- function(...) workspace
    server$queue_pending_replies <- function(...) NULL
    tasks <- list()
    server$resolve_task_manager <- list(add_task = function(uri, task) {
        tasks[[length(tasks) + 1L]] <<- task
    })
    testthat::local_mocked_bindings(resolve_task = function(self, uri, document, packages,
        delay = 0, namespace_packages = character()) {
        list(packages = packages, namespace_packages = namespace_packages)
    }, .package = "languageserver")
    parsed <- parse_document(uri, doc$content)
    parse_callback(server, uri, 1L, parsed, cache = FALSE)
    expect_length(tasks, 1L)
    expect_identical(tasks[[1L]]$namespace_packages, "recoveryfixture")
    expect_length(tasks[[1L]]$packages, 0L)
    parse_callback(server, uri, 1L, parsed, cache = FALSE)
    expect_length(tasks, 1L)

    doc$loaded_packages <- "priorfixture"
    doc$requested_packages <- list(packages = "priorfixture", namespace_packages = "oldnamespace")
    doc$set_content(2L, c("bad <- )", "nextfixture::object@"))
    parsed <- parse_document(uri, doc$content)
    parse_callback(server, uri, 2L, parsed, cache = FALSE)
    expect_length(tasks, 2L)
    expect_identical(tasks[[2L]]$packages, "priorfixture")
    expect_identical(tasks[[2L]]$namespace_packages, c("oldnamespace", "nextfixture"))
    expect_identical(doc$loaded_packages, "priorfixture")
})

package_recovery_wait_fixture <- function(code) {
    content <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    uri <- "file:///package-recovery-wait.R"
    doc <- Document$new(uri, language = "r", version = 1L, content = content)
    parsed <- parse_document(uri, content)
    parsed$version <- 1L
    doc$update_parse_data(parsed)
    doc$requested_packages <- list(packages = character(), namespace_packages = "recoveryfixture")
    workspace <- list(documents = collections::dict(), member_metadata = collections::dict())
    workspace$documents$set(uri, doc)
    server <- new.env(parent = emptyenv())
    server$get_workspace <- function(...) workspace
    server$resolve_task_manager <- list(has = function(...) TRUE, promote = function(...) NULL)
    server$member_task_manager <- list(has = function(...) TRUE, promote = function(...) NULL)
    list(server = server, workspace = workspace, document = doc, uri = uri,
        point = list(row = length(content) - 1L, col = nchar(tail(content, 1L))))
}

test_that("Cold member requests wait for only the receiver's pending namespace", {
    for (code in c("recoveryfixture::frame$", "value <- recoveryfixture::frame\nvalue$")) {
        fixture <- package_recovery_wait_fixture(code)
        expect_true(member_metadata_waiting(fixture$server, fixture$uri, fixture$document, fixture$point))
        fixture$workspace$member_metadata$set("recoveryfixture", list(package = "recoveryfixture", roots = list()))
        expect_false(member_metadata_waiting(fixture$server, fixture$uri, fixture$document, fixture$point))
    }
    fixture <- package_recovery_wait_fixture("list(local = TRUE)$")
    expect_false(member_metadata_waiting(fixture$server, fixture$uri, fixture$document, fixture$point))
    fixture <- package_recovery_wait_fixture("recoveryfixture::frame$")
    fixture$server$member_task_manager <- list(has = function(...) FALSE)
    expect_false(member_metadata_waiting(fixture$server, fixture$uri, fixture$document, fixture$point))
})

test_that("The first namespace member request queues independently of S3", {
    fixture <- package_recovery_wait_fixture("recoveryfixture::frame$")
    server <- fixture$server
    server$pending_replies <- collections::dict()
    queue <- collections::queue()
    server$pending_replies$set(fixture$uri, list(`textDocument/completion` = queue))
    server$ClientCapabilities <- list()
    replies <- list()
    server$deliver <- function(reply) replies[[length(replies) + 1L]] <<- reply
    promoted <- 0L
    server$member_task_manager$promote <- function(...) promoted <<- promoted + 1L
    text_document_completion(server, 1L, list(textDocument = list(uri = fixture$uri),
            position = list(line = fixture$point$row, character = fixture$point$col)))
    expect_length(replies, 0L)
    expect_equal(queue$size(), 1L)
    expect_identical(promoted, 1L)
})

test_that("Member worker failures wake pending namespace completion requests", {
    fixture <- package_recovery_wait_fixture("recoveryfixture::frame$")
    ready <- character()
    testthat::local_mocked_bindings(create_task = function(target, args, callback, error, ...) list(error = error),
        s3_queue_ready = function(self, uri) ready <<- c(ready, uri), .package = "languageserver")
    task <- resolve_member_task(fixture$server, fixture$uri, fixture$document, character(), "recoveryfixture")
    task$error(simpleError("worker failed"))
    expect_identical(ready, fixture$uri)
})
