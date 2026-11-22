test_that("S7 named bindings expose the remaining formals at the factory call", {
    skip_if_not_installed("S7", "1.0.0")
    workspace <- Workspace$new(NULL)
    workspace$load_packages("S7")
    workspace$member_metadata$set("S7", member_prepare_package("S7"))
    uri <- "file:///s7-named-bind.R"
    probe <- function(line, prefix = "library(S7)", callee = "new_generic") {
        lines <- c(prefix, line)
        doc <- Document$new(uri, version = 1L, content = lines)
        doc$loaded_packages <- "S7"
        workspace$documents$set(uri, doc)
        parsed <- parse_document(uri, lines)
        parsed$version <- 1L
        workspace$update_parse_data(uri, parsed)
        opening <- regexpr(paste0(callee, "("), line, fixed = TRUE)
        point <- list(row = length(prefix), col = as.integer(opening) + nchar(callee))
        sig <- signature_reply(1L, uri, workspace, doc, point)$result$signatures[[1L]]$label
        items <- completion_reply(1L, uri, workspace, doc, point, list())$result$items
        params <- Filter(function(x) isTRUE(x$data$type %in% c("parameter", "member")), items)
        bounds <- range(position(point$row, 0L), position(point$row, nchar(line)))
        hints <- inlay_hint_reply(1L, uri, workspace, doc, bounds)$result
        list(parameters = extract_parameter_names(sig), arguments = vapply(params, `[[`, "", "label"),
            hints = vapply(hints, `[[`, "", "label"))
    }
    for (line in c('describe := new_generic("x", function(x) x)', "describe := S7::new_generic(",
            'f <- function() { describe := new_generic("x", function(x) x) }')) {
        result <- probe(line)
        expect_identical(result$parameters, c("dispatch_args", "fun"))
        expect_true("dispatch_args" %in% result$arguments)
        expect_false("name" %in% result$arguments)
        if (endsWith(line, "(")) next
        expect_true(all(c("dispatch_args =", "fun =") %in% result$hints))
    }
    # S7 supplies name to user factories too, including when it is not first.
    prefix <- c("library(S7)", "factory <- function(x, name, flag = FALSE) NULL")
    result <- probe("item := factory(1, TRUE)", prefix, "factory")
    expect_identical(result$parameters, c("x", "flag"))
    expect_true("flag =" %in% result$hints)
    factory_params <- function(parameters) {
        prefix <- c("library(S7)", paste0("factory <- function(", parameters, ") NULL"))
        probe("item := factory()", prefix, "factory")$parameters
    }
    expect_identical(factory_params("namely, value"), "value")
    expect_identical(factory_params("..., namely, value"), c("...", "namely", "value"))
    expect_identical(factory_params("named, ..., name"), c("named", "..."))
    for (line in c('item <- new_generic("item", "x")', 'item := identity(new_generic("item", "x"))')) {
        expect_identical(probe(line)$parameters, c("name", "dispatch_args", "fun"))
    }
    masked <- probe('item := new_generic("item", "x")', c("library(S7)", "`:=` <- function(lhs, rhs) rhs"))
    expect_identical(masked$parameters, c("name", "dispatch_args", "fun"))
})

test_that("package imports supply S7 constructors and properties only where visible", {
    skip_if_not_installed("S7", "1.0.0")
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("Package: geometry", file.path(root, "DESCRIPTION"))
    namespace <- file.path(root, "NAMESPACE")
    uri <- path_to_uri(file.path(root, "R", "point.R"))
    lines <- c("Point := new_class(properties = list(x = class_numeric, y = class_numeric))",
        "p <- Point(x = 3, y = 4)", "p@x")
    snapshot <- member_prepare_package("S7")
    check <- function(directives, code = lines, file = uri) {
        writeLines(directives, namespace)
        data <- parse_document(file, code)
        doc <- Document$new(file, language = "r", content = code, version = 1L)
        data$version <- 1L
        doc$update_parse_data(data)
        workspace <- Workspace$new(root)
        request <- member_package_request(data)
        if ("S7" %in% request$namespace_packages) workspace$member_metadata$set("S7", snapshot)
        items <- member_completion(file, workspace, doc,
            list(row = length(code) - 1L, col = 2L), TRUE, 100L)
        vapply(items, `[[`, character(1L), "label")
    }
    expect_equal(check("import(S7)"), c("x", "y"))
    expect_equal(check('importFrom(S7, ":=", new_class, class_numeric)'), c("x", "y"))
    expect_length(check('import(S7, except = ":=")'), 0L)
    expect_length(check('importFrom(S7, ":=", class_numeric)'), 0L)
    expect_length(check("import(S7)", file = path_to_uri(file.path(root, "analysis.R"))), 0L)
    expect_length(check("import(S7)", c("new_class <- function(...) NULL", lines)), 0L)
    # Preparing one qualified use must not make other bare S7 functions visible.
    expect_length(check("export(Point)", c("S7::new_class", lines)), 0L)
})

test_that("stored S7 properties support direct writes, aliases and method-free hover", {
    skip_if_not_installed("S7", "1.0.0")
    fixture <- function(code, point) {
        uri <- "file:///s7-writes.R"
        doc <- Document$new(uri, language = "r", content = code, version = 1L)
        data <- parse_document(uri, code)
        data$version <- 1L
        doc$update_parse_data(data)
        hover_reply(1L, uri, workspace, doc, point)$result$contents
    }
    workspace <- Workspace$new(NULL)
    workspace$member_metadata$set("S7", member_prepare_package("S7"))
    code <- c("library(S7)", "Point := new_class(properties=list(x=class_numeric, y=class_numeric))",
        "p <- Point(3, 4)", "copy <- p", "p@x <- 6")
    hover <- function(expr, prefix = code) fixture(c(prefix, expr), list(row = length(prefix), col = nchar(expr)))
    expect_identical(hover("p@x"), "```r\n6\n```")
    expect_identical(hover("p@y"), "```r\n4\n```")
    expect_identical(hover("copy@x"), "```r\n3\n```")
    expect_identical(hover("f()@x", c(code, "f <- function() { p <- Point(1, 2); p@x <- 7; p }")), "```r\n7\n```")
    expect_identical(fixture(c(code, "f <- function() {", "p2 <- Point(1, 2)", "p2@x <- 8", "p2@x", "}"),
            list(row = 8L, col = 4L)), "```r\n8\n```")
})

test_that("S4_contains preserves stored S7 properties in an S4 subclass", {
    skip_if_not_installed("S7", "1.0.0")
    uri <- "file:///s7-compatibility.R"
    workspace <- Workspace$new(NULL)
    workspace$member_metadata$set("S7", member_prepare_package("S7"))
    code <- c("library(S7)",
        'Sample <- new_class("Sample", properties = list(name = class_character, score = class_numeric))',
        "S4_register(Sample)", "Sample_S4 <- S4_contains(Sample)",
        'methods::setClass("ReplicatedSample", contains = Sample_S4, slots = c(replicate = "integer"))',
        'rs <- methods::new("ReplicatedSample", name = "A", score = 3, replicate = 1L)')
    labels <- function(lines) {
        doc <- Document$new(uri, language = "r", content = c(lines, "rs@"), version = 1L)
        data <- parse_document(uri, doc$content)
        data$version <- 1L
        doc$update_parse_data(data)
        items <- member_completion(uri, workspace, doc, list(row = length(lines), col = 3L), TRUE, 100L)
        vapply(items, `[[`, character(1L), "label")
    }
    expect_equal(labels(code), c("name", "replicate", "score"))
    direct <- code
    direct[[5L]] <- sub("Sample_S4", "S7::S4_contains(Sample)", direct[[5L]], fixed = TRUE)
    expect_equal(labels(direct), c("name", "replicate", "score"))
    expect_length(labels(append(code, "S4_contains <- function(x) NULL", after = 3L)), 0L)
    computed <- code
    computed[[2L]] <- sub("name = class_character", "name = new_property(class_character, getter = function(self) stop())",
        computed[[2L]], fixed = TRUE)
    expect_length(labels(computed), 0L)
})

test_that("library attachment exclusions follow S7's installed hooks", {
    skip_if_not_installed("S7", "1.0.0")
    skip_if_not_installed("rlang")
    # The resolver normally attaches dependencies in a separate worker.
    exclusions <- callr::r(function(inspect) {
        library(S7)
        library(rlang)
        inspect(c("S7", "rlang"), .packages())
    }, args = list(inspect = member_attachment_exclusions), libpath = .libPaths())
    expect_true(":=" %in% exclusions$rlang)
    uri <- "file:///attached-s7.R"
    code <- c("library(S7)", "library(rlang)", "Point := new_class(properties = list(x = class_double))", "Point()@x")
    doc <- Document$new(uri, language = "r", version = 1L, content = code)
    doc$attachment_exclusions <- exclusions
    parsed <- parse_document(uri, code, attachment_exclusions = exclusions)
    parsed$version <- doc$version
    doc$update_parse_data(parsed)
    workspace <- Workspace$new(NULL)
    workspace$member_metadata$set("S7", member_prepare_package("S7"))
    # Only the competing exported closure is relevant to this request.
    mask <- member_generic_index("")
    mask$package <- "rlang"
    mask$exports <- ":="
    mask$definitions <- list(`:=` = quote(function(...) NULL))
    workspace$member_metadata$set("rlang", member_index_freeze(mask))
    labels <- member_completion(uri, workspace, doc, list(row = 3L, col = 8L), TRUE, 100L)
    expect_identical(vapply(labels, `[[`, "", "label"), "x")
    expect_identical(parsed$definitions$Point$type, "S7")
})

test_that("semantic deltas notice S7 declarations changing under identical text", {
    skip_if_not_installed("S7", "1.0.0")
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("Package: geometry", file.path(root, "DESCRIPTION"))
    path <- file.path(root, "NAMESPACE")
    uri <- path_to_uri(file.path(root, "R", "point.R"))
    source <- "Point := new_class(properties = list(x = class_double))"
    doc <- Document$new(uri, version = 1L, content = source)
    ws <- Workspace$new(root)
    ws$documents$set(uri, doc)
    read <- function(directive) {
        writeLines(directive, path)
        data <- parse_document(uri, source)
        data$version <- 1L
        doc$update_parse_data(data)
        semantic_tokens_full_reply(1L, uri, ws, doc)$result
    }
    before <- read('import(S7, except = ":=")')
    after <- read("import(S7)")
    expect_false(identical(before$data, after$data))
    expect_false(identical(before$resultId, after$resultId))
    delta <- semantic_tokens_delta_reply(1L, uri, ws, doc, before$resultId)$result
    expect_identical(delta$data, after$data)
})

test_that("a nested package manifest refreshes its accepted source imports", {
    root <- index_normalize_path(withr::local_tempdir())
    package <- file.path(root, "geometry")
    dir.create(file.path(package, "R"), recursive = TRUE)
    manifest <- file.path(package, "NAMESPACE")
    writeLines("Package: geometry", file.path(package, "DESCRIPTION"))
    writeLines("import(S7)", manifest)
    uri <- path_to_uri(file.path(package, "R", "point.R"))
    doc <- Document$new(uri, version = 1L, content = "Point := new_class()")
    parsed <- parse_document(uri, doc$content)
    parsed$version <- doc$version
    doc$update_parse_data(parsed)
    workspace <- Workspace$new(root)
    workspace$documents$set(uri, doc)
    calls <- collections::queue()
    server <- list(get_workspace = function(uri) workspace, text_sync = function(uri, doc, parse, ...) {
        if (parse) calls$push(uri)
    })
    testthat::local_mocked_bindings(backend_watched_configuration = function(...) NULL, .package = "languageserver")
    event <- list(changes = list(list(uri = path_to_uri(manifest), type = FileChangeType$Changed)))
    workspace_did_change_watched_files(server, event)
    expect_equal(calls$size(), 0L)
    writeLines('import(S7, except = ":=")', manifest)
    workspace_did_change_watched_files(server, event)
    expect_identical(calls$pop(), uri)
})

test_that("S7 member providers follow unconditional braced attachments and declarations in order", {
    skip_if_not_installed("S7", "1.0.0")
    uri <- "file:///s7-block.R"
    workspace <- Workspace$new(NULL)
    workspace$member_metadata$set("S7", member_prepare_package("S7"))
    mask <- member_generic_index("")
    mask$package <- "mask"
    mask$exports <- "new_class"
    mask$definitions <- list(new_class = quote(function(...) NULL))
    workspace$member_metadata$set("mask", member_index_freeze(mask))

    inspect <- function(lines, version = 1L) {
        doc <- Document$new(uri, version = version, content = lines)
        parsed <- parse_document(uri, lines)
        parsed$version <- version
        doc$update_parse_data(parsed)
        workspace$documents$set(uri, doc)
        doc
    }
    labels <- function(doc, line) {
        items <- member_completion(uri, workspace, doc, list(row = line, col = 8L), TRUE, 100L)
        vapply(items, `[[`, "", "label")
    }
    code <- c("{", "  library(S7)", "  { Point := new_class(properties = list(x = class_double)) }",
        "  Point()@x", "}", "Point()@x", "Point()")
    doc <- inspect(code)
    expect_identical(doc$parse_data$packages, "S7")
    expect_identical(labels(doc, 5L), "x")
    signature <- signature_reply(1L, uri, workspace, doc, list(row = 6L, col = 6L))$result
    expect_identical(signature$signatures[[1L]]$label, "Point(x = numeric(0))")
    inside <- member_completion(uri, workspace, doc, list(row = 3L, col = 10L), TRUE, 100L)
    expect_identical(vapply(inside, `[[`, "", "label"), "x")
    property <- definition_reply(1L, uri, workspace, doc, list(row = 5L, col = 8L))$result
    x <- regexpr("x =", code[[3L]], fixed = TRUE)[[1L]] - 1L
    expect_equal(property, list(uri = uri, range = range(position(2L, x), position(2L, x + 1L))))

    # Adding an unrelated syntax error retains the prior, complete brace
    # block and its source positions for consumers and other files.
    partial <- inspect(c("bad <- )", code))
    expect_identical(labels(partial, 6L), "x")
    complete <- member_complete_item_ids(partial$parse_data)
    expect_identical(vapply(partial$parse_data$member_data$items[complete], function(item) item$start[[1L]], 0L),
        c(2L, 3L, 4L, 6L, 7L))

    declaration <- "Point := new_class(properties = list(x = class_double))"
    doc <- inspect(c("library(S7)", "{ library(mask) }", declaration, "Point()@x"))
    expect_length(labels(doc, 3L), 0L)
    doc <- inspect(c("library(S7)", declaration, "{ library(mask) }", "Point()@x"))
    expect_identical(labels(doc, 3L), "x")
    doc <- inspect(c("require <- function(...) TRUE", '{ { base::require(quietly = TRUE, package = "S7") } }',
            declaration, "Point()@x"))
    expect_identical(labels(doc, 3L), "x")
    doc <- inspect(c("library(S7)", '{ base::library(mask, include.only = "new_class") }', declaration, "Point()@x"))
    expect_length(labels(doc, 3L), 0L)
    doc <- inspect(c("library(S7)", '{ library(mask, exclude = "other") }', declaration, "Point()@x"))
    doc$attachment_exclusions <- list(mask = "new_class")
    expect_length(labels(doc, 3L), 0L)
    for (scope in c('{ base::library(S7, exclude = "new_class") }', "if (condition) { library(S7) }", "f <- function() { library(S7) }",
            "{ library <- function(...) TRUE; library(S7) }", "quote({ library(S7) })")) {
        expect_length(labels(inspect(c(scope, declaration, "Point()@x")), 2L), 0L)
    }
})

test_that("sourced complete blocks in a partially parsed file keep their property origins", {
    skip_if_not_installed("S7", "1.0.0")
    root <- index_normalize_path(withr::local_tempdir())
    workspace <- Workspace$new(root)
    workspace$member_metadata$set("S7", member_prepare_package("S7"))
    open <- function(name, code) {
        path <- file.path(root, name)
        writeLines(code, path)
        uri <- path_to_uri(path)
        doc <- Document$new(uri, version = 1L, content = code)
        doc$is_open <- TRUE
        workspace$documents$set(uri, doc)
        parsed <- parse_document(uri, code)
        parsed$version <- doc$version
        workspace$update_parse_data(uri, parsed)
        doc
    }
    declaration <- '{ base::require(S7); { "\U0001f4cd"; Point := new_class(properties = list(x = class_double)) } }'
    api <- open("api.R", c("bad <- )", declaration, "other <- list(key = 1)"))
    caller <- open("main.R", c('source("api.R")', "Point()@x", "other$key"))
    for (i in 1:2) {
        key <- c("x", "key")[[i]]
        pos <- list(row = i, col = nchar(caller$line0(i)))
        location <- definition_reply(1L, caller$uri, workspace, caller, pos)$result
        start <- regexpr(paste(key, "="), api$line0(i), fixed = TRUE)[[1L]] - 1L
        if (i == 1L) start <- start + 1L # The supplementary character occupies two UTF-16 code units.
        expect_equal(location, list(uri = api$uri, range = range(position(i, start), position(i, start + nchar(key)))))
    }
})
