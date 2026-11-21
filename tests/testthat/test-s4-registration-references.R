test_that("S4 references isolate source scopes, ordinary shadows, and ambiguous registrations", {
    root <- index_normalize_path(withr::local_tempdir())
    w <- Workspace$new(root)
    put <- function(file, code) {
        path <- file.path(root, file)
        writeLines(code, path)
        uri <- path_to_uri(path)
        doc <- Document$new(uri, "r", 1L, code)
        w$documents$set(uri, doc)
        parsed <- parse_document(uri, code)
        parsed$version <- doc$version
        w$update_parse_data(uri, parsed)
        list(uri = uri, document = doc, workspace = w)
    }
    definition <- 'methods::setGeneric("f", function(x) standardGeneric("f"))'
    first <- put("one.R", definition)
    other <- put("two.R", definition)
    calls <- put("calls.R", c('source("one.R")', 'methods::setMethod("f", "numeric", function(x) x)', "f(1)",
            "local <- function(f) f(2)", "f <- function(x) x", "f(3)"))
    unrelated <- put("unrelated.R", c('source("two.R")', "f(4)"))
    ambiguous <- put("ambiguous.R", c('source("one.R")', 'source("two.R")', "f(5)"))
    refer <- function(f, row, col) references_reply(1L, f$uri, w, f$document, list(row = row, col = col))$result
    method <- put("method.R", 'methods::setMethod("f", "integer", function(x) x)')
    joined <- put("joined.R", c('source("one.R")', 'source("method.R")', "f(7)"))
    shared <- put("shared.R", 'methods::setMethod("f", "character", function(x) x)')
    shared_one <- put("shared-one.R", c('source("one.R")', 'source("shared.R")', "f(8)"))
    shared_two <- put("shared-two.R", c('source("two.R")', 'source("shared.R")', "f(9)"))
    refs <- refer(first, 0L, 22L)
    joined_refs <- refer(method, 0L, 21L)
    expect_setequal(vapply(joined_refs, `[[`, "", "uri"), vapply(refs, `[[`, "", "uri"))
    expect_false(shared$uri %in% vapply(refs, `[[`, "", "uri"))
    expect_setequal(vapply(refs, `[[`, "", "uri"), c(first$uri, calls$uri, method$uri, joined$uri, shared_one$uri))
    in_calls <- Filter(function(x) x$uri == calls$uri, refs)
    expect_equal(vapply(in_calls, function(x) x$range$start$line, integer(1L)), c(1L, 2L))
    expect_setequal(vapply(refer(other, 0L, 22L), `[[`, "", "uri"), c(other$uri, unrelated$uri, shared_two$uri))
    expect_length(refer(ambiguous, 2L, 0L), 0L)
    local <- refer(calls, 5L, 0L)
    expect_true(all(vapply(local, function(x) x$uri == calls$uri, logical(1L))))
    for (i in list(list(first, 0L, 22L), list(calls, 1L, 21L), list(calls, 2L, 0L))) {
        f <- i[[1L]]
        point <- list(row = i[[2L]], col = i[[3L]])
        expect_equal(prepare_rename_reply(1L, f$uri, w, f$document, point)$error$code, ErrorCodes$RequestFailed)
        expect_equal(rename_reply(1L, f$uri, w, f$document, point, "changed")$error$code, ErrorCodes$RequestFailed)
    }
})

test_that("ordinary S4-reference misses reuse the registration catalog without walking scope", {
    w <- Workspace$new(NULL)
    f <- provider_fixture(c('methods::setGeneric("draw", function(x) standardGeneric("draw"))', "ordinary(1)"))
    w$documents$set(f$uri, f$document)
    w$update_parse_data(f$uri, f$document$parse_data)
    expect_length(s4_reference_registrations(w, f$uri, "draw"), 1L)
    testthat::local_mocked_bindings(reference_source_scope = function(...) stop("unexpected scope walk"),
        workspace_s4_source_data = function(...) stop("unexpected registration lookup"), .package = "languageserver")
    expect_null(s4_reference_target(w, f$document, list(row = 1L, col = 3L)))
})

test_that("renaming a later ordinary function preserves earlier uses of the generic", {
    f <- provider_fixture(c('methods::setGeneric("draw", function(x) standardGeneric("draw"))',
            "draw(1)", "draw <- function(x) x", "draw(2)"))
    for (row in 2:3) {
        edits <- rename_reply(1L, f$uri, f$workspace, f$document, list(row = row, col = 2L), "local_draw")$result$changes
        expect_equal(vapply(edits[[f$uri]], function(x) x$range$start$line, integer(1L)), 2:3)
    }
})

test_that("S4 replacement and getter registrations have independent call references", {
    f <- provider_fixture(c('methods::setMethod("rate", "X", function(x) 1)',
            'methods::setReplaceMethod("rate", "X", function(x, value) x)',
            "rate(x)", "rate(x) <- 1", "1 -> rate(x)", "`rate<-`(x, 1)"))
    lookup <- function(row, col) {
        refs <- references_reply(1L, f$uri, f$workspace, f$document, list(row = row, col = col))$result
        vapply(refs, function(x) x$range$start$line, integer(1L))
    }
    expect_equal(lookup(0L, 23L), c(0L, 2L))
    expect_equal(lookup(1L, 30L), c(1L, 3:5))
    expect_equal(lookup(3L, 2L), c(1L, 3:5))
})

test_that("references share F12's promotion and masking rules and exclude qualified package calls", {
    f <- provider_fixture(c("median <- function(x) x", "median(1)", 'methods::setGeneric("median")',
            'methods::setMethod("median", "Model", function(x) 1)', "median(2)",
            "fun <- function(median) median(3)", "stats::median(4)", "median <- function(y) y", "median(5)"))
    w <- Workspace$new(NULL)
    w$documents$set(f$uri, f$document)
    w$update_parse_data(f$uri, f$document$parse_data)
    point <- function(row, col = 2L) list(row = row, col = col)
    refs <- function(row, col = 2L) references_reply(1L, f$uri, w, f$document, point(row, col))$result
    rows <- function(xs) vapply(xs, function(x) as.integer(x$range$start$line), integer(1L))
    expect_equal(rows(refs(4L)), 2:4)
    expect_equal(rows(refs(6L, 9L)), 6L)
    expect_equal(rename_reply(1L, f$uri, w, f$document, point(4L), "mid")$error$code, ErrorCodes$RequestFailed)
    expect_equal(prepare_rename_reply(1L, f$uri, w, f$document, point(4L))$error$code, ErrorCodes$RequestFailed)
    edits <- rename_reply(1L, f$uri, w, f$document, point(8L), "middle")$result$changes[[f$uri]]
    expect_false(4L %in% rows(edits))
    expect_true(all(c(7L, 8L) %in% rows(edits)))
    # An older snapshot can still contain the symbol outline but cannot
    # establish a registration absent from the authoritative S4 index.
    f$document$parse_data$s4_declarations <- NULL
    w$invalidate_s3_context()
    expect_null(s4_reference_target(w, f$document, point(4L)))

    # A package fallback can resolve the bare name to stats::median, but
    # the source registration owns the bare call at this point.
    f <- provider_fixture(c('methods::setGeneric("median", function(x) standardGeneric("median"))',
            "median(1)", "stats::median(2)"))
    w$documents$set(f$uri, f$document)
    w$update_parse_data(f$uri, f$document$parse_data)
    expect_equal(rows(refs(2L, 9L)), 2L)
})

test_that("recognized relative S4 helper literals share compound replacement identity", {
    f <- provider_fixture(c('methods::setReplaceMethod("value", "Box", function(x, value) x)',
            "value(x)[1] <- 3", 'methods::selectMethod("value<-", "Box")',
            'other::selectMethod("value<-", "Box")'))
    req <- function(row, col) references_reply(1L, f$uri, f$workspace, f$document, list(row = row, col = col))$result
    for (site in list(c(0L, 31L), c(1L, 2L), c(2L, 26L))) {
        locs <- req(site[[1L]], site[[2L]])
        expect_setequal(vapply(locs, function(x) x$range$start$line, integer(1L)), 0:2)
    }
    expect_length(req(3L, 25L), 0L)
})
