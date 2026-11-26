roxygen_s3_call <- function(lines, member_metadata = NULL) {
    row <- grep("@@", lines, fixed = TRUE)
    col <- regexpr("@@", lines[[row]], fixed = TRUE)[[1L]] - 1L
    lines[[row]] <- sub("@@", "", lines[[row]], fixed = TRUE)
    f <- provider_fixture(lines)
    f$workspace$member_metadata <- member_metadata
    f$workspace$s3_metadata <- S3MetadataCache$new()
    for (snapshot in provider_startup_s3()) f$workspace$s3_metadata$set(snapshot$package, snapshot)
    resolve_s3_call(f$uri, f$workspace, f$document, list(row = row - 1L, col = col))
}

test_that("S3 dispatch follows documented parameter and return types through calls", {
    generic <- c('draw <- function(x, ...) UseMethod("draw")',
        'draw.widget <- function(x, color = "blue", ...) x',
        "draw.integer <- function(x, digits = 1L, ...) x")
    docs <- c("#' @param x (`widget`) A widget.", "use <- function(x) {")
    call <- roxygen_s3_call(c(generic, docs, "  draw(x, @@)", "}"))
    expect_identical(call$method$name, "draw.widget")
    expect_match(call$signature, "color", fixed = TRUE)
    expect_identical(roxygen_s3_call(c(generic, docs, "  x <- 2L", "  draw(x, @@)", "}"))$method$name,
        "draw.integer")
    expect_null(roxygen_s3_call(c(generic, docs, "  inner <- function(x) draw(x, @@)", "}")))
    expect_identical(roxygen_s3_call(c("#' @param x (`widget`) A widget.", generic,
                "draw(opaque(), @@)"))$method$name, "draw.widget")

    local <- c('G <- R6::R6Class("Global")', 'draw <- function(x, ...) UseMethod("draw")',
        "draw.Global <- function(x, option) x", "draw.Other <- function(x, local) x", "outer <- function() {")
    inner <- c("#' @param x (`G`) Input.", "inner <- function(x) draw(x, @@)", "}")
    expect_identical(roxygen_s3_call(c(local, inner))$method$name, "draw.Global")
    expect_identical(roxygen_s3_call(c(local, 'G <- R6::R6Class("Other")', inner))$method$name, "draw.Other")
    expect_null(roxygen_s3_call(c(local, "G <- opaque()", inner)))

    factories <- c("#' @returns (`widget`) The restored widget.",
        "restore <- function(path) opaque(path)", "wrap <- function(path) restore(path)",
        "#' @param x (`widget`) A widget.", "relay <- function(x, result = x) result")
    for (expression in c("wrap(opaque())", "relay(opaque())")) {
        expect_identical(roxygen_s3_call(c(generic, factories, paste0("draw(", expression, ", @@)")))$method$name,
            "draw.widget")
    }
    expect_identical(roxygen_s3_call(c(generic, factories, "draw(relay(1L), @@)"))$method$name, "draw.integer")
    choose <- c("#' @return (`widget`) Usually a widget.", "choose <- function(flag) if (flag) 1L else opaque()")
    expect_identical(roxygen_s3_call(c(generic, choose, "draw(choose(TRUE), @@)"))$method$name, "draw.integer")
    expect_identical(roxygen_s3_call(c(generic, choose, "draw(choose(FALSE), @@)"))$method$name, "draw.widget")
    expect_null(roxygen_s3_call(c(generic, choose, "draw(choose(opaque()), @@)")))
})

test_that("union parameter and return types only select an agreed S3 method", {
    generic <- c('draw <- function(x, ...) UseMethod("draw")', "draw.numeric <- function(x, digits = 2L, ...) x")
    param <- c("#' @param x (`integer | double`) A numeric vector.", "use <- function(x) draw(x, @@)")
    result <- c("#' @return (`integer | double`) A numeric vector.", "read <- function() opaque()",
        "use <- function() draw(read(), @@)")
    for (code in list(param, result)) {
        call <- roxygen_s3_call(c(generic, code))
        expect_identical(call$method$name, "draw.numeric")
        expect_match(call$description, "`integer` or `double`", fixed = TRUE)
        expect_null(roxygen_s3_call(c(generic, "draw.integer <- function(x, other = TRUE) x", code)))
    }
})

test_that("S3 dispatch follows documented member values through a local assignment", {
    generic <- c('paint <- function(x, ...) UseMethod("paint")', "paint.dye <- function(x, color = TRUE, ...) x")
    declaration <- c("#' @slot item (`dye`)", 'Holder <- methods::setClass("Holder", slots = c(item = "ANY"))')
    for (expr in c("Holder()@item", 'methods::slot(Holder(), "item")', "contents")) {
        use <- c("use <- function() {", "  contents <- Holder()@item", paste0("  paint(", expr, ", @@)"), "}")
        expect_identical(roxygen_s3_call(c(generic, declaration, use))$method$name, "paint.dye")
    }
    override <- c(generic, "paint.integer <- function(x, digits = 1L, ...) x", declaration, "paint(Holder(item = 1L)@item, @@)")
    expect_identical(roxygen_s3_call(override)$method$name, "paint.integer")
    declaration[[1L]] <- "#' @slot item (`dye | other`)"
    expect_null(roxygen_s3_call(c(generic, "paint.other <- function(x, opacity = 1, ...) x", declaration,
                "paint(Holder()@item, @@)")))
    r6 <- c('Bin <- R6::R6Class("Bin", public = list(', "#' @field item (`dye`)", "item = NULL))")
    expect_identical(roxygen_s3_call(c(generic, r6, "paint(Bin$new()$item, @@)"))$method$name, "paint.dye")
    expect_identical(roxygen_s3_call(c(generic, "#' @param x (`dye`)", 'Bin <- R6::R6Class("Bin", public = list(',
                "m = function(x) paint(x, @@)))"))$method$name, "paint.dye")
})

test_that("S3 dispatch can read an S7 computed property's hint", {
    skip_if_not_installed("S7")
    metadata <- MemberMetadataCache$new(16 * 1024^2)
    metadata$set("S7", member_prepare_package("S7"))
    lines <- c('paint <- function(x, ...) UseMethod("paint")', "paint.dye <- function(x, color = TRUE, ...) x",
        "#' @prop item (`dye`)", 'Bin <- S7::new_class("Bin", properties = list(',
        "  item = S7::new_property(getter = function(self) opaque())))", "paint(Bin()@item, @@)")
    expect_identical(roxygen_s3_call(lines, metadata)$method$name, "paint.dye")
})

test_that("documented S3 dispatch observes known ancestry and proven return alternatives", {
    generic <- 'g <- function(x, ...) UseMethod("g")'
    fallback <- c(generic, "g.default <- function(x, fallback = TRUE) x")
    use <- function(type) c(paste0("#' @param x (`", type, "`) Input."), "use <- function(x) g(x, @@)")
    for (type in c("ordered", "glm")) {
        parent <- if (type == "ordered") "factor" else "lm"
        method <- paste0("g.", parent)
        classes <- c(fallback, paste0(method, " <- function(x, inherited = TRUE) x"))
        expect_identical(roxygen_s3_call(c(classes, use(type)))$method$name, method)
    }
    expect_null(roxygen_s3_call(c(fallback, use("undocumented_ancestry"))))
    direct <- "g.undocumented_ancestry <- function(x, direct = TRUE) x"
    expect_identical(roxygen_s3_call(c(fallback, direct, use("undocumented_ancestry")))$method$name,
        "g.undocumented_ancestry")
    union <- c("#' @return (`integer | double`) A numeric vector.",
        "make <- function(flag) if (flag) 1L else 1.5", "g(make(opaque()), @@)")
    numeric <- c(generic, "g.numeric <- function(x, digits = 2L) x")
    expect_identical(roxygen_s3_call(c(numeric, union))$method$name, "g.numeric")
    expect_null(roxygen_s3_call(c(numeric, "g.integer <- function(x, digits = 4L) x", union)))

    methods <- c(generic, "g.Parent <- function(x, general = TRUE) x", "g.Child <- function(x, specific = TRUE) x")
    docs <- "#' @return (`Parent`) A Parent instance."
    for (body in c('if (flag) eval(NULL); structure(1L, class = c("Child", "Parent"))',
            'return(structure(1L, class = c("Child", "Parent"))); source(unknown)')) {
        make <- paste0("make <- function(flag) { ", body, " }")
        expect_identical(roxygen_s3_call(c(methods, docs, make, "g(make(FALSE), @@)"))$method$name, "g.Child")
    }
})

test_that("classless R6 instances dispatch as environments and keep their members", {
    code <- c('G <- R6::R6Class("G", class = FALSE, public = list(run = function() self))',
        'g <- function(x, ...) UseMethod("g")', "g.G <- function(x, classed = TRUE) x",
        "g.environment <- function(x, environment = TRUE) x", "#' @param x (`G`) An instance.")
    call <- roxygen_s3_call(c(code, "use <- function(x) g(x, @@)"))
    expect_identical(call$method$name, "g.environment")
    f <- provider_fixture(c(code, "use <- function(x) x$run()"))
    point <- list(row = length(code), col = nchar("use <- function(x) x$"))
    labels <- vapply(member_completion(f$uri, f$workspace, f$document, point, TRUE, 100L), `[[`, "", "label")
    expect_true("run" %in% labels)
})

test_that("an annotated factory can capture generators in a separate buffer", {
    caller <- c('g <- function(x, ...) UseMethod("g")', "g.Inner <- function(x, option = TRUE) x", "g(make(), )")
    lines <- c("make <- function() {", '  G <- R6::R6Class("Inner")', "  #' @return (`G`) An instance.",
        "  factory <- function() opaque()", "  factory()", "}")
    f <- provider_fixture(caller)
    owner <- Document$new("file:///factory.R", "r", 1L, lines)
    parsed <- parse_document(owner$uri, lines)
    parsed$version <- owner$version
    owner$update_parse_data(parsed)
    f$workspace$documents$set(owner$uri, owner)
    result <- resolve_s3_call(f$uri, f$workspace, f$document, list(row = 2L, col = nchar(caller[[3L]]) - 1L))
    expect_identical(result$method$name, "g.Inner")
})

test_that("S3 dispatch on S4 and reference class hints respects ordered superclass methods", {
    generic <- c('g <- function(x, ...) UseMethod("g")', "g.RoParent <- function(x, inherited = TRUE, ...) NULL",
        "g.default <- function(x, fallback = TRUE, ...) NULL")
    use <- c("#' @param x (`RoChild`) Input.", "f <- function(x) g(x, @@)")
    s4 <- c('methods::setClass("RoParent", slots = c(x = "integer"))',
        'methods::setClass("RoChild", contains = "RoParent")')
    rc <- c('RoParent <- methods::setRefClass("RoParent", fields = c(x = "integer"))',
        'RoChild <- methods::setRefClass("RoChild", contains = "RoParent")')
    for (code in list(s4, rc)) {
        expect_identical(roxygen_s3_call(c(code, generic, use))$method$name, "g.RoParent")
    }
    branches <- c(s4[[1L]], 'methods::setClass("RoLeft", contains = "RoParent")',
        'methods::setClass("RoRight", slots = c(y = "integer"))',
        'methods::setClass("RoChild", contains = c("RoLeft", "RoRight"))')
    expect_identical(roxygen_s3_call(c(branches, generic, "g.RoRight <- function(x, ...) NULL", use))$method$name,
        "g.RoRight")
    for (partial in list('methods::setClass("RoChild", contains = unknown_parent)',
            'RoChild <- methods::setRefClass("RoChild", contains = unknown_parent)')) {
        expect_null(roxygen_s3_call(c(partial, generic, use)))
        direct <- "g.RoChild <- function(x, specific = TRUE, ...) NULL"
        expect_identical(roxygen_s3_call(c(partial, generic, direct, use))$method$name, "g.RoChild")
    }
})

test_that("installed S4 metadata retains the S3 superclass order without constructing an instance", {
    class <- methods::getClass("refObjectGenerator")
    expect_null(member_s4_runtime_descriptor(class)$dispatch_path)
    descriptor <- member_s4_runtime_descriptor(class, dispatch = TRUE)
    index <- member_generic_index("")
    index$package <- descriptor$package
    index$s4_classes[[descriptor$name]] <- descriptor
    instance <- member_s4_shape(descriptor$name, index, list(), descriptor$package)
    expect_null(instance$s4_class$dispatch_path)
    expected <- as.character(methods::extends(class, maybe = FALSE))
    expect_identical(member_hint_dispatch_classes(instance, index), expected)
    shape <- member_hinted_value(descriptor$name, index, list(), member_request_budget())
    expect_identical(member_hint_dispatch_classes(shape, index), expected)
    descriptor$dispatch_path <- NULL
    index$s4_classes[[descriptor$name]] <- descriptor
    shape <- member_hinted_value(descriptor$name, index, list(), member_request_budget())
    expect_identical(s3_dispatch_path(member_hint_dispatch_classes(shape, index)), descriptor$name)
})

test_that("optional class ancestry does not displace a bounded package snapshot", {
    index <- member_generic_index("")
    index$s4_classes <- list(Child = list(name = "Child", slots = list(id = "integer")))
    structural <- member_index_freeze(index)
    index$s4_classes$Child$dispatch_path <- c(Child = 0, Parent = 1)
    testthat::local_mocked_bindings(member_snapshot_max_bytes = as.numeric(utils::object.size(structural)),
        .package = "languageserver")
    expect_identical(member_index_freeze(index), structural)
    index$large <- raw(1024L)
    expect_error(member_index_freeze(index), "Namespace exceeds snapshot budget", fixed = TRUE)
})

test_that("S3 waits only for class catalogs used by a documented dispatch", {
    old <- lsp_settings$get("member_completion")
    withr::defer(lsp_settings$set("member_completion", old))
    lsp_settings$set("member_completion", FALSE)
    lines <- c("library(R6)", "unused <- unrelated::factory",
        'draw <- function(x, ...) UseMethod("draw")', "draw.fixture_parent <- function(x, display = TRUE, ...) x",
        "draw.integer <- function(x, digits = 2L, ...) x", "#' @param x (`fixture_child`) An instance.",
        "use <- function(x) {", "  draw(1L, )", "  draw(x, )", "}")
    f <- provider_fixture(lines)
    requested <- member_package_request(f$document$parse_data)
    f$document$requested_packages <- requested
    expect_identical(f$document$parse_data$member_data$type_hint_packages, "R6")
    f$workspace$member_metadata <- MemberMetadataCache$new(4 * 1024^2)
    f$workspace$s3_metadata <- S3MetadataCache$new()
    for (snapshot in provider_startup_s3()) f$workspace$s3_metadata$set(snapshot$package, snapshot)
    for (pkg in c("R6", "unrelated")) {
        snapshot <- list(schema = 1L, package = pkg, generation = pkg, functions = list(), methods = list(), objects = list())
        f$workspace$s3_metadata$set(pkg, snapshot)
    }
    tasks <- new.env(parent = emptyenv())
    manager <- list(has = function(...) !is.null(tasks$member),
        add_task = function(uri, task) tasks$member <- task, cancel = function(uri) tasks$member <- NULL)
    server <- list(get_workspace = function(...) f$workspace, member_task_manager = manager)
    resolve_task(server, f$uri, f$document, requested$packages, namespace_packages = requested$namespace_packages)
    expect_identical(tasks$member$context$request$member_packages, "R6")
    point <- list(row = 8L, col = 10L)
    expect_false(s3_metadata_waiting(server, f$uri, f$document, list(row = 7L, col = 11L)))
    expect_true(s3_metadata_waiting(server, f$uri, f$document, point))
    expect_null(resolve_s3_call(f$uri, f$workspace, f$document, point))
    index <- member_generic_index("")
    index$package <- "R6"
    index$classes$fixture_child <- c("fixture_child", "fixture_parent")
    f$workspace$member_metadata$set("R6", member_index_freeze(index))
    expect_false(s3_metadata_waiting(server, f$uri, f$document, point))
    expect_identical(resolve_s3_call(f$uri, f$workspace, f$document, point)$method$name, "draw.fixture_parent")

    plain <- provider_fixture(c("library(R6)", 'Pipeline <- R6::R6Class("Pipeline")',
            "#' @param x (`integer | Pipeline`) A value.", "run <- function(x) draw(x)"))
    expect_length(plain$document$parse_data$member_data$type_hint_packages, 0L)
    qualified <- provider_fixture(c("library(unrelated)", "#' @return (`typefixture::child`) An instance.",
            "create <- function() opaque()"))
    expect_identical(qualified$document$parse_data$member_data$type_hint_packages, "typefixture")
})

test_that("the defining buffer wakes S3 requests waiting for its class catalog", {
    old <- lsp_settings$get("member_completion")
    withr::defer(lsp_settings$set("member_completion", old))
    lsp_settings$set("member_completion", FALSE)
    code <- c('draw <- function(x, ...) UseMethod("draw")', "draw.Parent <- function(x, inherited = TRUE) x",
        "draw.default <- function(x, fallback = TRUE) x", "draw(make(), )", "draw(make_known(), )")
    f <- provider_fixture(code)
    workspace <- f$workspace
    workspace$member_metadata <- MemberMetadataCache$new(4 * 1024^2)
    workspace$s3_metadata <- S3MetadataCache$new()
    for (snapshot in provider_startup_s3()) workspace$s3_metadata$set(snapshot$package, snapshot)
    owner <- Document$new("file:///factory.R", "r", 1L,
        c("#' @return (`typefixture::Child`) A widget.", "make <- function() opaque()",
            "#' @return (`typefixture::Child`) Usually a widget.", "make_known <- function() 1L"))
    parsed <- parse_document(owner$uri, owner$content)
    parsed$version <- owner$version
    owner$update_parse_data(parsed)
    workspace$documents$set(owner$uri, owner)
    workspace$document_uris_for_context <- function(uri) unique(c(uri, owner$uri))
    owner$requested_packages <- member_package_request(parsed)
    state <- new.env(parent = emptyenv())
    manager <- list(has = function(uri) identical(state$owner, uri),
        get_task = function(uri) if (identical(state$owner, uri)) state$task,
        add_task = function(uri, task) {
            state$task <- task
            state$owner <- uri
        },
        cancel = function(uri) if (identical(state$owner, uri)) state$owner <- NULL, promote = function(uri) state$promoted <- uri)
    server <- list(get_workspace = function(...) workspace, member_task_manager = manager,
        pending_replies = collections::dict(), queue_pending_replies = function(uri) state$ready <- c(state$ready, uri),
        deliver = function(message) state$messages <- c(state$messages, list(message)))
    method <- "textDocument/signatureHelp"
    queue <- collections::queue()
    server$pending_replies$set(f$uri, stats::setNames(list(queue), method))
    workspace$root <- withr::local_tempdir()
    workspace$type_hierarchy_cache <- collections::dict()
    workspace$update_loaded_packages <- function() NULL
    point <- list(row = 3L, col = nchar(code[[4L]]) - 1L)
    schedule <- function() {
        resolve_task(server, owner$uri, owner, owner$requested_packages$packages,
            namespace_packages = owner$requested_packages$namespace_packages)
    }
    schedule()
    expect_false(s3_metadata_waiting(server, f$uri, f$document, list(row = 4L, col = nchar(code[[5L]]) - 1L)))
    subscribe <- function(id) {
        expect_true(s3_metadata_waiting(server, f$uri, f$document, point, request_id = id))
        enqueue_latest_reply(server, f$uri, method, list(id = id, version = f$document$version))
    }
    subscribe(1L)
    s3_queue_ready(server, owner$uri)
    expect_false(f$uri %in% state$ready)
    # Superseding a request drops its subscription even if the replacement
    # is waiting for a different reason in the same document.
    enqueue_latest_reply(server, f$uri, method, list(id = "1", version = f$document$version))
    expect_length(workspace$s3_hint_workers$get(owner$uri), 0L)
    s3_wake_hint_dependents(server, workspace, owner$uri, recheck = TRUE)
    expect_false(f$uri %in% state$ready)
    subscribe(2L)
    expect_false(s3_metadata_waiting(server, f$uri, f$document,
            list(row = 4L, col = nchar(code[[5L]]) - 1L), request_id = 2L))
    expect_length(workspace$s3_hint_workers$get(owner$uri), 0L)
    expect_true(s3_metadata_waiting(server, f$uri, f$document, point, request_id = 2L))
    cancel_request(server, list(id = 2L))
    expect_length(workspace$s3_hint_workers$get(owner$uri), 0L)
    expect_identical(queue$size(), 0L)
    # The owner stays pending as transient editor buffers arrive and leave.
    for (i in 1:3) {
        uri <- paste0("file:///transient-", i, ".R")
        doc <- Document$new(uri, "r", 1L, code)
        data <- parse_document(uri, code)
        data$version <- doc$version
        doc$update_parse_data(data)
        workspace$documents$set(uri, doc)
        server$pending_replies$set(uri, stats::setNames(list(collections::queue()), method))
        request <- paste0("transient-", i)
        expect_true(s3_metadata_waiting(server, uri, doc, point, request_id = request))
        enqueue_latest_reply(server, uri, method, list(id = request, version = doc$version))
        text_document_did_close(server, list(textDocument = list(uri = uri)))
        expect_length(workspace$s3_hint_workers$get(owner$uri), 0L)
        expect_false(workspace$documents$has(uri))
    }
    subscribe(3L)
    expect_identical(state$promoted, owner$uri)
    # Cancelling the only worker must also release the caller.
    manager$cancel(owner$uri)
    s3_queue_ready(server, owner$uri)
    expect_true(f$uri %in% state$ready)
    expect_false(s3_metadata_waiting(server, f$uri, f$document, point))
    state$ready <- NULL
    schedule()
    subscribe("worker error")
    previous <- state$task
    owner$requested_packages$namespace_packages <- c(owner$requested_packages$namespace_packages, "otherfixture")
    schedule()
    expect_false(identical(state$task, previous))
    previous$fail(simpleError("stale failure"))
    expect_false(f$uri %in% state$ready)
    state$task$fail(simpleError("class preparation failed"))
    expect_true(f$uri %in% state$ready)
    expect_false(workspace$s3_hint_workers$has(owner$uri))
    manager$cancel(owner$uri)
    owner$requested_packages <- member_package_request(parsed)
    state$ready <- NULL
    schedule()
    subscribe("configuration")
    # The owner can be between parses when dispatch is switched off. A late
    # callback from that configuration must not wake a replacement request.
    saved_dispatch <- lsp_settings$get("s3_dispatch")
    withr::defer(lsp_settings$set("s3_dispatch", saved_dispatch))
    server$workspaces <- collections::dict()
    server$workspaces$set(workspace$root, workspace)
    server$resolve_task_manager <- list(has = function(uri) FALSE, add_task = function(...) NULL)
    owner$parse_data$parse_error <- TRUE
    workspace_did_change_configuration(server, list(settings = list(s3_dispatch = FALSE, member_completion = FALSE)))
    expect_true(f$uri %in% state$ready)
    expect_false(workspace$s3_hint_workers$has(owner$uri))
    state$ready <- NULL
    s3_queue_ready(server, owner$uri)
    expect_false(f$uri %in% state$ready)
    owner$parse_data$parse_error <- FALSE
    lsp_settings$set("s3_dispatch", TRUE)
    schedule()
    subscribe(4L)
    index <- member_generic_index("")
    index$package <- "typefixture"
    index$classes$Child <- c("Child", "Parent")
    workspace$member_metadata$set("typefixture", member_index_freeze(index))
    manager$cancel(owner$uri)
    s3_queue_ready(server, owner$uri)
    expect_true(f$uri %in% state$ready)
    expect_false(s3_metadata_waiting(server, f$uri, f$document, point))
    expect_identical(resolve_s3_call(f$uri, workspace, f$document, point)$method$name, "draw.Parent")
    # The unsplit manager likewise invokes its final handler while the same
    # task is still visible. There is no later shared-settled notification.
    workspace$member_metadata <- MemberMetadataCache$new(4 * 1024^2)
    server$member_task_manager <- NULL
    server$resolve_task_manager <- manager
    state$ready <- NULL
    manager$add_task(owner$uri, schedule())
    subscribe("combined failure")
    state$task$fail(simpleError("combined worker failed"))
    expect_true(f$uri %in% state$ready)
    expect_false(workspace$s3_hint_workers$has(owner$uri))
})
