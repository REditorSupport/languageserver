test_that("Type definition selects the generator bound to a name or stored in a list", {
    declarations <- c("Alias <- methods::setClass('Widget')", "Alias <- S7::new_class('Widget')")
    for (declaration in declarations) {
        code <- c(declaration, "stored <- list(generator = Alias)", "Alias", "stored$generator")
        fixture <- provider_fixture(code)
        expected <- list(uri = fixture$uri, range = range(position(0L, 0L), position(0L, 5L)))
        for (row in 2:3) {
            col <- if (row == 2L) 2L else 10L
            at <- type_definition_reply(1L, fixture$uri, fixture$workspace, fixture$document, list(row = row, col = col))
            expect_equal(at$result, expected)
        }
    }
    previous <- lsp_settings$get("member_completion")
    withr::defer(lsp_settings$set("member_completion", previous))
    lsp_settings$set("member_completion", FALSE)
    expect_null(type_definition_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = 2L, col = 2L))$result)
})

test_that("Implementation navigation keeps physical source columns for open and unopened targets", {
    uri <- "file:///method.R"
    lines <- c("'\U0001F600'; method <- function(x) x", "method(1)")
    doc <- Document$new(uri, version = 1L, content = lines)
    docs <- collections::dict()
    workspace <- list(documents = docs)
    implementation <- list(uri = uri, start = c(0L, 5L), end = c(0L, 11L), columns = c(6L, 12L))
    target <- function(impl) s3_implementation_target(list(source = list(implementation = impl)), workspace, NULL)
    expected <- list(uri = uri, range = range(position(0L, 6L), position(0L, 12L)))
    expect_equal(target(implementation), expected)
    expect_null(target(implementation[names(implementation) != "columns"]))
    docs$set(uri, doc)
    expect_null(target(implementation))
    parsed <- parse_document(uri, lines)
    parsed$version <- 1L
    doc$update_parse_data(parsed)
    expect_equal(target(implementation), expected)
    doc$set_content(2L, c("# moved", lines))
    expect_null(target(implementation))
})

test_that("Verified implementation navigation falls back to generated source when the file changes", {
    path <- withr::local_tempfile(fileext = ".R")
    writeLines("draw.record <- function(x) x", path)
    ns <- new.env(parent = baseenv())
    sys.source(path, envir = ns, keep.source = TRUE)
    hash <- s3_function_identity(member_function_syntax(ns$draw.record), "sample")
    verify <- function(name, owner = "sample") s3_verified_method_body("sample", name, owner, hash, TRUE, ns)
    expect_null(verify("missing"))
    expect_null(verify("draw.record", "other"))
    verified <- verify("draw.record")
    method <- list(package = "sample", name = "draw.record")
    workspace <- list(documents = collections::dict())
    store <- TextDocumentContent$new()
    source <- s3_implementation_location(method, workspace, store, verified)
    expect_equal(source, list(uri = path_to_uri(path), range = range(position(0L, 15L), position(0L, 23L))))
    expect_equal(store$size(), 0L)
    writeLines(c("# moved", "draw.record <- function(x) x"), path)
    virtual <- s3_implementation_location(method, workspace, store, verified)
    expect_identical(virtual$uri, paste0(store$scheme, ":/sample/draw.record.R"))
    expect_match(store$get(virtual$uri), "function", fixed = TRUE)

    tiny <- TextDocumentContent$new(max_bytes = 1L)
    disk <- s3_implementation_location(method, workspace, tiny, verified)
    expect_equal(tiny$size(), 0L)
    expect_match(disk$uri, "^file:")
    expect_identical(readLines(path_from_uri(disk$uri))[-1L], verified$code)
})

test_that("Navigation requests finish immediately for missing documents and failed parses", {
    f <- provider_fixture("f <- function() 1")
    replies <- list()
    self <- list(get_workspace = function(...) f$workspace,
        deliver = function(reply) replies[[length(replies) + 1L]] <<- reply)
    request <- function(id, method, uri) {
        class_navigation_request(self, id, list(textDocument = list(uri = uri), position = position(0L, 1L)), method)
        replies[[length(replies)]]
    }
    missing <- request(7L, "textDocument/typeDefinition", "file:///removed.R")
    expect_identical(missing$id, 7L)
    expect_null(missing$result)
    f$document$parse_failure <- list(version = f$document$version, message = "worker failed")
    failed <- request(8L, "textDocument/implementation", f$uri)
    expect_equal(failed$error$code, ErrorCodes$InternalError)
    expect_identical(failed$error$message, "worker failed")
})
