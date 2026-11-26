inherited_s3_metadata <- function(workspace) {
    workspace$s3_metadata <- S3MetadataCache$new()
    for (snapshot in provider_startup_s3()) workspace$s3_metadata$set(snapshot$package, snapshot)
}

test_that("S3 resolves inherited formals, generic declarations and wrapper arguments", {
    methods <- c('draw <- function(x, ...) UseMethod("draw")', "draw.numeric <- function(x, digits = 3L, ...) x",
        "draw.character <- function(x, quote = TRUE, ...) x")
    donor <- c("#' @param x (`integer | double`) Numeric data.", "donor <- function(x) x")
    fixture <- function(lines) {
        f <- provider_fixture(lines)
        inherited_s3_metadata(f$workspace)
        f
    }
    call <- function(f, row) {
        resolve_s3_call(f$uri, f$workspace, f$document,
            list(row = row - 1L, col = nchar(f$document$content[[row]]) - 1L))
    }
    f <- fixture(c(methods, "#' @inheritParams donor", "use <- function(x) draw(x, )", donor))
    expect_identical(call(f, 5L)$method$name, "draw.numeric")
    # Put the more specific method in scope at the request.
    f <- fixture(c(methods, "draw.integer <- function(x, other = 2L) x", "#' @inheritParams donor",
            "use <- function(x) draw(x, )", donor))
    expect_null(call(f, 6L))
    f <- fixture(c("#' @inheritParams donor", methods, "draw(opaque(), )", donor))
    expect_identical(call(f, 5L)$method$name, "draw.numeric")
    f <- fixture(c(methods, "#' @inheritParams donor", "identity2 <- function(x, y = x) y",
            "value <- identity2(opaque())", "draw(value, )", 'draw(identity2("known"), )', donor))
    expect_identical(call(f, 7L)$method$name, "draw.numeric")
    expect_identical(call(f, 8L)$method$name, "draw.character")
    # The donor is below the unchanged assignment whose S3 class was cached.
    lines <- f$document$content
    lines[[9L]] <- "#' @param x (`character`) Text data."
    f$document$set_content(2L, lines)
    parsed <- parse_document(f$uri, lines)
    parsed$version <- f$document$version
    f$document$update_parse_data(parsed)
    expect_identical(call(f, 7L)$method$name, "draw.character")
})

test_that("inherited calls wait for the donor's parse and class model in their package", {
    old_members <- lsp_settings$get("member_completion")
    old_s3 <- lsp_settings$get("s3_dispatch")
    withr::defer({
        lsp_settings$set("member_completion", old_members)
        lsp_settings$set("s3_dispatch", old_s3)
    })
    for (members in c(FALSE, TRUE)) {
        lsp_settings$set("member_completion", members)
        lsp_settings$set("s3_dispatch", !members)
        root <- index_normalize_path(withr::local_tempdir())
        dir.create(file.path(root, "R"))
        writeLines("Package: inheritdispatch", file.path(root, "DESCRIPTION"))
        writeLines("", file.path(root, "NAMESPACE"))
        workspace <- Workspace$new(root)
        inherited_s3_metadata(workspace)
        workspace$member_metadata <- MemberMetadataCache$new(4 * 1024^2)
        owner_path <- file.path(root, "R", "donor.R")
        owner <- path_to_uri(owner_path)
        donor <- c("#' @param x (`typefixture::Child`) An instance.", "donor <- function(x) x")
        writeLines(donor, owner_path)
        workspace$index$update_content(owner, donor)
        path <- file.path(root, "R", "consumer.R")
        uri <- path_to_uri(path)
        lines <- c('draw <- function(x, ...) UseMethod("draw")', "draw.Parent <- function(x, inherited = TRUE, ...) x",
            "draw.character <- function(x, quote = FALSE, ...) x",
            "#' @inheritParams donor", if (members) "use <- function(x) x$run()" else "use <- function(x) draw(x, )",
            "#' @inheritParams donor", if (members) "known <- function(x) list(ready = TRUE)$ready" else "known <- function(x) draw(1L, )")
        writeLines(lines, path)
        doc <- Document$new(uri, "r", 1L, lines)
        workspace$documents$set(uri, doc)
        accept <- function(document) {
            parsed <- parse_document(document$uri, document$content)
            parsed$version <- document$version
            workspace$update_parse_data(document$uri, parsed)
            workspace$index$update_content(document$uri, document$content, parse_data = parsed)
            document$requested_packages <- member_package_request(parsed)
        }
        accept(doc)
        state <- new.env(parent = emptyenv())
        manager <- list(has = function(uri) identical(state$worker, uri),
            has_work = function() !is.null(state$worker),
            add_task = function(uri, task) state$worker <- uri,
            promote = function(uri) state$promoted <- uri,
            cancel = function(uri) state$worker <- NULL)
        server <- list(get_workspace = function(...) workspace, member_task_manager = manager,
            resolve_task_manager = list(has = function(...) FALSE),
            load_index_document = function(w, uri) {
                state$loaded <- uri
                if (!w$documents$has(uri)) {
                    w$documents$set(uri, Document$new(uri, "r", NULL, readLines(path_from_uri(uri))))
                    w$index$await_parse(uri, cacheable = TRUE)
                }
            }, queue_context_reply = function(uri, w, helpers = NULL, topic_root = NULL, ...) {
                if (length(helpers)) state$parses <- helpers
            })
        point <- list(row = 4L, col = if (members) nchar("use <- function(x) x$") else nchar(lines[[5L]]) - 1L)
        known <- list(row = 6L, col = if (members) nchar("known <- function(x) list(ready = TRUE)$") else nchar(lines[[7L]]) - 1L)
        waiting <- if (members) member_metadata_waiting else s3_metadata_waiting
        expect_false(waiting(server, uri, doc, known, request_id = 13L))
        expect_null(state$loaded)
        expect_true(waiting(server, uri, doc, point, request_id = 12L), info = as.character(members))
        expect_identical(state$loaded, owner)
        expect_identical(state$parses, owner)
        source <- workspace$documents$get(owner)
        accept(source)
        workspace$index$finish_document(owner)
        resolve_task(server, owner, source, source$requested_packages$packages,
            namespace_packages = source$requested_packages$namespace_packages)
        expect_true(waiting(server, uri, doc, point, request_id = 12L))
        expect_identical(state$promoted, owner)
        expect_identical(workspace$s3_hint_workers$get(owner)[[request_id_key(12L)]]$uri, uri)
        if (members) {
            hover <- list(row = 4L, col = nchar("use <- function(x) x$ru"))
            inside <- list(row = 4L, col = nchar("use <- function(x) x$run("))
            expect_true(member_metadata_waiting(server, uri, doc, hover, inherited_only = TRUE))
            expect_true(member_metadata_waiting(server, uri, doc, inside, call = doc$detect_call(inside, signature = TRUE)))
        }
        catalog <- member_generic_index("")
        catalog$package <- "typefixture"
        catalog$classes$Child <- c("Child", "Parent")
        catalog$members$Child <- c(run = NA_character_)
        workspace$member_metadata$set("typefixture", member_index_freeze(catalog))
        expect_false(waiting(server, uri, doc, point, request_id = 12L))
        if (members) {
            items <- member_completion(uri, workspace, doc, point, TRUE, 100L)
            expect_true("run" %in% vapply(items, `[[`, "", "label"))
        } else {
            expect_identical(resolve_s3_call(uri, workspace, doc, point)$method$name, "draw.Parent")
        }
        manager$cancel(owner)
        s3_queue_ready(server, owner)
        expect_false(workspace$s3_hint_workers$has(owner))
        state$loaded <- state$parses <- NULL
        source$set_content(2L, c("#' @param x (`character`) Text data.", donor[[2L]]))
        workspace$index$await_parse(owner, cacheable = FALSE)
        server$parse_task_manager <- list(has_work = function() TRUE, promote = function(...) NULL)
        expect_false(waiting(server, uri, doc, known, request_id = 13L))
        expect_true(waiting(server, uri, doc, point, request_id = 12L))
        expect_null(state$loaded)
        expect_identical(state$parses, owner)
        accept(source)
        workspace$index$finish_document(owner)
        if (members) {
            items <- member_completion(uri, workspace, doc, point, TRUE, 100L)
            expect_false("run" %in% vapply(items, `[[`, "", "label"))
            result <- member_resolve_cursor(uri, workspace, doc, point, member_cursor(doc, point))
            expect_identical(result$value$type, "character")
        } else {
            expect_identical(resolve_s3_call(uri, workspace, doc, point)$method$name, "draw.character")
        }
    }
})

test_that("the first LSP reply uses a closed package donor and follows its unsaved changes", {
    skip_on_cran()
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("Package: inheritclient", file.path(root, "DESCRIPTION"))
    writeLines("", file.path(root, "NAMESPACE"))
    origin <- file.path(root, "R", "donor.R")
    path <- file.path(root, "R", "use.R")
    donor <- c('First <- R6::R6Class("First", public = list(old_member = function(count = 1L) self))',
        'Second <- R6::R6Class("Second", public = list(new_member = function() self))',
        "#' @param x (`First`) An instance.", "donor <- function(x) x")
    use <- c("#' @inheritParams donor", "use <- function(x) x$old_member(1L)")
    writeLines(donor, origin)
    writeLines(use, path)
    client <- language_client(working_dir = root)
    open <- function(file, lines) {
        item <- list(uri = path_to_uri(file), languageId = "r", version = 1L, text = paste(lines, collapse = "\n"))
        notify(client, "textDocument/didOpen", list(textDocument = item))
    }
    members <- function() {
        reply <- respond_completion(client, path, c(1L, nchar("use <- function(x) x$")), retry = FALSE)
        intersect(vapply(reply$items, `[[`, "", "label"), c("old_member", "new_member"))
    }
    open(path, use)
    expect_identical(members(), "old_member")
    signature <- respond_signature(client, path, c(1L, nchar("use <- function(x) x$old_member(")), retry = FALSE)
    expect_identical(signature$signatures[[1L]]$label, "old_member(count = 1L)")
    open(origin, donor)
    edited <- donor
    edited[[3L]] <- "#' @param x (`Second`) An instance."
    changes <- list(list(text = paste(edited, collapse = "\n")))
    notify(client, "textDocument/didChange", list(textDocument = list(uri = path_to_uri(origin), version = 2L), contentChanges = changes))
    expect_identical(members(), "new_member")
    expect_identical(readLines(origin), donor)
    notify(client, "textDocument/didClose", list(textDocument = list(uri = path_to_uri(origin))))
    expect_identical(members(), "old_member")
})
