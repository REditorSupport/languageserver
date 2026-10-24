s3_fixture_catalogs <- local({
    cached <- list()
    function(packages) {
        for (package in setdiff(packages, names(cached))) {
            cached[[package]] <<- s3_prepare_namespace(package)
        }
        cached[packages]
    }
})

s3_fixture <- function(lines, packages = startup_packages) {
    fixture <- provider_fixture(lines)
    fixture$workspace$s3_metadata <- S3MetadataCache$new()
    catalogs <- s3_fixture_catalogs(packages)
    for (package in packages) fixture$workspace$s3_metadata$set(package, catalogs[[package]])
    fixture$workspace$get_documentation <- function(...) NULL
    fixture$workspace$get_signature <- function(...) NULL
    fixture$workspace$get_help <- function(...) NULL
    fixture
}

s3_result <- function(fixture, row = fixture$document$nline - 1L, col = nchar(fixture$document$line0(row)) - 1L) {
    resolve_s3_call(fixture$uri, fixture$workspace, fixture$document, list(row = row, col = col))
}

test_that("S3 source methods share completion, signatures and hover", {
    fixture <- s3_fixture(c(
        "render <- function(x, ...) UseMethod('render')",
        "#' Render a data frame",
        "#' @param columns Columns to show.",
        "render.data.frame <- function(x, columns = NULL, digits = 3, ...) NULL",
        "obj <- data.frame(a = 1:3)", "render(obj, )"
    ))
    result <- s3_result(fixture)
    expect_identical(result$classes, "data.frame")
    expect_identical(result$method$name, "render.data.frame")
    expect_identical(vapply(s3_arguments(result, ""), `[[`, "", "label"), c("columns", "digits"))
    signature <- signature_reply(1L, fixture$uri, fixture$workspace, fixture$document, list(row = 5L, col = 12L))$result
    expect_identical(signature$signatures[[1L]]$label, "render(x, columns = NULL, digits = 3, ...)")
    expect_match(signature$signatures[[1L]]$documentation$value, "Render a data frame")
    hover <- hover_reply(1L, fixture$uri, fixture$workspace, fixture$document, list(row = 5L, col = 3L))$result
    expect_match(paste(hover$contents, collapse = " "), "render.data.frame", fixed = TRUE)
})

test_that("S3 class chains preserve order and implicit numeric fallback", {
    base <- c(
        "g <- function(x, ...) UseMethod('g')", "g.numeric <- function(x, digits=2, ...) NULL",
        "g.parent <- function(x, inherited=TRUE, ...) NULL", "g.default <- function(x, ...) NULL"
    )
    for (expr in c("1", "1L", "structure(list(), class=c('child','parent'))")) {
        fixture <- s3_fixture(c(base, paste0("g(", expr, ", )")))
        result <- s3_result(fixture)
        expect_identical(result$method$name, if (startsWith(expr, "structure")) "g.parent" else "g.numeric")
    }
    fixture <- s3_fixture(c(base, "x <- 1:3", "class(x) <- c('child','parent')", "g(x, )"))
    expect_identical(s3_result(fixture)$classes, c("child", "parent"))
})

test_that("S3 dispatch respects explicit and reordered generic formals", {
    fixture <- s3_fixture(c(
        "g <- function(meta, x, ...) UseMethod('g', x)",
        "g.data.frame <- function(meta, x, columns=NULL, ...) NULL", "x <- data.frame(a=1)", "g(columns=NULL, x=x, meta=1)"
    ))
    expect_identical(s3_result(fixture)$method$name, "g.data.frame")
    expect_null(s3_result(s3_fixture(c("g <- function(x, ...) UseMethod('g')", "g.default <- function(x, ...) NULL", "g(unknown, )"))))
})

test_that("S3 namespace methods and dataset metadata work without evaluation", {
    fixture <- s3_fixture(c("data(iris)", "summary(iris, )"))
    result <- s3_result(fixture)
    expect_identical(result$classes, "data.frame")
    expect_identical(result$method$name, "summary.data.frame")
    fixture <- s3_fixture(c("fit <- stats::lm(mpg ~ wt, data=mtcars)", "stats::predict(fit, )"))
    expect_identical(s3_result(fixture)$classes, "lm")
    expect_identical(s3_result(fixture)$method$name, "predict.lm")
})

test_that("S3 plotting methods can omit generic formals", {
    for (code in c(
        "plot(lm(mpg ~ wt, data=mtcars), )",
        "graphics::plot(stats::lm(mpg ~ wt, data=mtcars), )",
        "graphics::plot(stats::lm(mpg ~ wt, data=mtcars), y=NULL, )"
    )) {
        fixture <- s3_fixture(code)
        result <- s3_result(fixture)
        expect_identical(result$classes, "lm", info = code)
        expect_identical(result$method$name, "plot.lm", info = code)
        labels <- vapply(s3_arguments(result, ""), `[[`, "", "label")
        expect_true("which" %in% labels, info = code)
        expect_false("y" %in% labels, info = code)
        point <- list(row = 0L, col = nchar(code) - 1L)
        signature <- signature_reply(1L, fixture$uri, fixture$workspace, fixture$document, point)$result
        expect_match(signature$signatures[[1L]]$label, "^plot\\(x, which = ")
        point$col <- regexpr("plot", code)[[1L]] - 1L
        hover <- hover_reply(1L, fixture$uri, fixture$workspace, fixture$document, point)$result
        expect_match(paste(hover$contents, collapse = " "), "plot.lm", fixed = TRUE)
    }
})

test_that("S3 re-exports retain ownership and respect active bindings", {
    catalog <- s3_fixture_catalogs("graphics")$graphics
    owner <- unname(getNamespaceName(environment(graphics::plot)))
    expect_identical(catalog$functions$plot$owner, owner)
    expect_identical(catalog$functions$plot$package, "graphics")
    imports <- new.env(parent = asNamespace("base"))
    ns <- new.env(parent = imports)
    for (scope in list(ns, imports)) {
        makeActiveBinding("plot", function() stop("must not execute"), scope)
        expect_identical(s3_namespace_binding(ns, "plot")[[1L]], "active")
        rm("plot", envir = scope)
    }
})

test_that("S3 methods validate supplied arguments independently of generic formals", {
    base <- c(
        "g <- function(x, y=NULL, ...) UseMethod('g')",
        "g.numeric <- function(x, digits=3) NULL"
    )
    for (call in c("g(1)", "g(1, digits=4)", "g(x=1, dig=4)")) {
        expect_identical(s3_result(s3_fixture(c(base, call)))$method$name, "g.numeric", info = call)
    }
    for (call in c("g(1, y=2)", "g(1, unknown=2)", "g(1, digits=2, digits=3)", "g(1, 2, 3)")) {
        expect_null(s3_result(s3_fixture(c(base, call))), info = call)
    }
    base <- c("g <- function(x, y=NULL) UseMethod('g')", "g.numeric <- function(x) NULL")
    expect_identical(s3_result(s3_fixture(c(base, "g(1)")))$method$name, "g.numeric")
    expect_null(s3_result(s3_fixture(c(base, "g(1, y=2)"))))
})

test_that("S3 startup catalogs are collected in a clean worker", {
    catalogs <- callr::r(package_call(s3_prepare_packages), args = list(startup_packages),
        system_profile = FALSE, user_profile = FALSE)
    expect_true(all(startup_packages %in% names(catalogs)))
    expect_true(all(vapply(catalogs, function(x) is.null(x$error), logical(1L))))
    expect_identical(catalogs$datasets$objects$iris$classes, "data.frame")
})

test_that("S3 dispatch follows native and magrittr pipe input", {
    base <- c("g <- function(x, ...) UseMethod('g')", "g.data.frame <- function(x, columns=NULL, ...) NULL", "x <- data.frame(a=1)")
    for (call in c("x |> g( )", "x %>% g( )", "x %T>% g( )", "x %<>% g( )")) {
        if (grepl("|>", call, fixed = TRUE) && getRversion() < "4.1.0") next
        fixture <- s3_fixture(c(base, call))
        expect_identical(s3_result(fixture)$classes, "data.frame", info = call)
    }
})

test_that("S3 factory pipes retain implicit input with nested placeholders", {
    fixture <- s3_fixture(c(
        "g <- function(x, ...) UseMethod('g')",
        "g.numeric <- function(x, digits=3, ...) NULL",
        "g.data.frame <- function(x, columns=NULL, ...) NULL",
        "pick <- function(x=data.frame(a=1), z=NULL) x",
        "make <- function(input) input %>% pick(z=list(.))", "g(make(1), )"
    ))
    expect_identical(s3_result(fixture)$method$name, "g.numeric")
})

test_that("S3 model summaries keep shadowed formula helpers unknown", {
    base <- c(
        "g <- function(x, ...) UseMethod('g')",
        "g.lm <- function(x, model=TRUE, ...) NULL", "g.mlm <- function(x, multivariate=TRUE, ...) NULL"
    )
    for (lines in list(
        c("cbind <- function(...) mtcars$mpg", "g(stats::lm(cbind(mpg,wt) ~ hp,data=mtcars), )"),
        c("I <- function(x) cbind(x,x)", "g(stats::lm(I(mpg > 0) ~ wt,data=mtcars), )")
    )) {
        expect_null(s3_result(s3_fixture(c(base, lines))))
    }
})

test_that("S3 pipe syntax does not override a source-defined operator", {
    fixture <- s3_fixture(c(
        "g <- function(x, ...) UseMethod('g')",
        "g.numeric <- function(x, digits=3, ...) NULL",
        "`%>%` <- function(x, rhs) NULL", "1 %>% g( )"
    ))
    expect_null(s3_result(fixture))
})

test_that("S3 keeps unknown and class-changing cases conservative", {
    base <- c("g <- function(x, ...) UseMethod('g')", "g.data.frame <- function(x, columns=NULL, ...) NULL", "g.default <- function(x, ...) NULL")
    fixture <- s3_fixture(c(base, "x <- data.frame(a=1)", "assign('x', 1)", "g(x, )"))
    expect_null(s3_result(fixture))
    fixture <- s3_fixture(c(base, "x <- if(opaque()) data.frame(a=1) else 1", "g(x, )"))
    expect_null(s3_result(fixture))
    fixture <- s3_fixture(c(base, "as.data.frame.custom <- function(x, ...) stop('never run')", "x <- structure(list(), class='custom')", "g(as.data.frame(x), )"))
    expect_null(s3_result(fixture))
})

test_that("S3 source factories, aliases and literal defaults stay inert", {
    fixture <- s3_fixture(c(
        "g <- function(x, ...) UseMethod('g')",
        "g.box <- function(x, count=stop('method default'), ...) NULL",
        "make <- function(value) {box <- list(value=value); class(box) <- 'box'; box}",
        "object <- make(stop('argument'))", "alias <- g", "alias(object, )"
    ))
    expect_identical(s3_result(fixture)$method$name, "g.box")
    fixture <- s3_fixture(c(
        "g <- function(x=data.frame(a=1), ...) UseMethod('g')",
        "g.data.frame <- function(x, columns=NULL, ...) NULL", "g( )"
    ))
    expect_identical(s3_result(fixture)$classes, "data.frame")
    fixture <- s3_fixture(c(
        "g <- function(x=y, y=data.frame(a=1), ...) UseMethod('g')",
        "g.data.frame <- function(x, y, columns=NULL, ...) NULL", "g()"
    ))
    expect_identical(s3_result(fixture)$classes, "data.frame")
    fixture <- s3_fixture(c(
        "y <- data.frame(a=1)", "g <- function(x=y, ...) UseMethod('g')",
        "g.data.frame <- function(x, columns=NULL, ...) NULL",
        "f <- function() {y <- 1; g()}"
    ))
    expect_identical(resolve_s3_call(fixture$uri, fixture$workspace, fixture$document,
            list(row = 3L, col = 27L))$classes, "data.frame")
})

test_that("S3 method-only arguments respect a closed generic interface", {
    fixture <- s3_fixture(c(
        "g <- function(x) UseMethod('g')",
        "g.numeric <- function(x, digits=3) NULL", "g(1)"
    ))
    result <- s3_result(fixture)
    expect_identical(result$signature, "g(x)")
    expect_length(s3_arguments(result, ""), 0L)
    fixture <- s3_fixture(c("g <- function(x) UseMethod('g')", "g.numeric <- function(x, digits=3) NULL", "g(1,digits=2)"))
    expect_null(s3_result(fixture))
    fixture <- s3_fixture(c("g <- function(x, ...) UseMethod('g')", "g.numeric <- function(other, digits=3) NULL", "g(1)"))
    result <- s3_result(fixture)
    expect_identical(result$signature, "g(other, digits = 3)")
    expect_identical(vapply(s3_arguments(result, ""), `[[`, "", "label"), "digits")
    fixture <- s3_fixture(c("g <- function(x, ...) UseMethod('g')", "g.numeric <- function(other, digits=3) NULL", "g(x=1)"))
    expect_null(s3_result(fixture))
    fixture <- s3_fixture(c("g <- function(x) UseMethod('g')", "g.numeric <- function(other, digits=3) NULL", "g(1)"))
    result <- s3_result(fixture)
    expect_identical(result$signature, "g(other)")
    expect_length(s3_arguments(result, ""), 0L)
    for (call in c("g(1)", "g(1, 2)", "g(1, dig)")) {
        fixture <- s3_fixture(c("g <- function(x, y=NULL) UseMethod('g')", "g.numeric <- function(other, digits=3) NULL", call))
        result <- s3_result(fixture)
        expect_identical(result$signature, "g(other, digits = 3)", info = call)
        expect_length(s3_arguments(result, if (grepl("dig", call)) "dig" else ""), 0L)
    }
})

test_that("S3 missing metadata cannot prove that only a default method applies", {
    fixture <- s3_fixture(c(
        "library(unavailable_package)", "g <- function(x, ...) UseMethod('g')",
        "g.default <- function(x, ...) NULL", "g(structure(list(),class='custom'), )"
    ))
    expect_null(s3_result(fixture))
    expect_length(fixture$document$s3_calls$keys(), 1L)
    expect_null(s3_result(fixture))
})

test_that("S3 recovery sees following and named dispatch arguments", {
    base <- c("g <- function(x, ...) UseMethod('g')", "g.data.frame <- function(x, columns=NULL, ...) NULL", "x <- data.frame(a=1)")
    fixture <- s3_fixture(c(base, "g(columns=NULL,", "  x=x)"))
    hover <- resolve_s3_call(fixture$uri, fixture$workspace, fixture$document, list(row = 3L, col = 0L), hover = TRUE)
    expect_identical(hover$classes, "data.frame")
    for (suffix in c("g(x, col", "g(x, columns = ", "g(x, columns = \"a", "    columns = NULL")) {
        fixture <- s3_fixture(c(base, suffix))
        point <- list(row = 3L, col = nchar(suffix))
        if (startsWith(suffix, "    ")) next
        expect_identical(resolve_s3_call(fixture$uri, fixture$workspace, fixture$document, point)$classes, "data.frame", info = suffix)
    }
})

test_that("S3 parameter docs and values use the selected method", {
    fixture <- s3_fixture(c(
        "g <- function(x, ...) UseMethod('g')",
        "#' @param mode Rendering mode.", "g.numeric <- function(x, mode=c('short','long'), ...) NULL", "g(1, mode='short')"
    ))
    result <- s3_result(fixture)
    expect_identical(vapply(s3_argument_values(result, "l"), `[[`, "", "label"), "long")
    item <- s3_arguments(s3_result(s3_fixture(c("g <- function(x, ...) UseMethod('g')", "g.numeric <- function(x, mode='short', ...) NULL", "g(1, )"))), "m")[[1L]]
    expect_identical(item$data$type, "s3_parameter")
    hover <- hover_reply(1L, fixture$uri, fixture$workspace, fixture$document, list(row = 3L, col = 7L))$result
    expect_match(paste(hover$contents, collapse = " "), "Rendering mode", fixed = TRUE)
})

test_that("S3 catalog replacement and document changes invalidate selection", {
    fixture <- s3_fixture(c("summary(iris, )"))
    first <- s3_result(fixture)
    catalog <- fixture$workspace$s3_metadata$get("base")
    catalog$methods[["base::summary.data.frame"]]$formals <- as.pairlist(alist(object = , changed = TRUE, ... = ))
    catalog$generation <- "replacement"
    fixture$workspace$s3_metadata$set("base", catalog)
    expect_match(s3_result(fixture)$signature, "changed", fixed = TRUE)
    fixture$document$set_content(2L, c("summary(1, )"))
    expect_null(s3_result(fixture))
})

test_that("S3 works independently of member completion and can be disabled", {
    old <- lsp_settings$get("member_completion")
    old_s3 <- lsp_settings$get("s3_dispatch")
    withr::defer({
        lsp_settings$set("member_completion", old)
        lsp_settings$set("s3_dispatch", old_s3)
    })
    fixture <- s3_fixture("summary(iris, )")
    lsp_settings$set("member_completion", FALSE)
    expect_identical(s3_result(fixture)$classes, "data.frame")
    lsp_settings$set("s3_dispatch", FALSE)
    expect_null(s3_result(fixture))
})

s3_workspace_fixture <- function(lines) {
    workspace <- Workspace$new(NULL)
    uri <- "file:///s3-workspace.R"
    document <- Document$new(uri, language = "r", version = 1L, content = lines)
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, lines)
    parsed$version <- 1L
    workspace$update_parse_data(uri, parsed)
    list(workspace = workspace, document = document, uri = uri)
}

test_that("S3 argument completion runs through the real provider", {
    fixture <- s3_workspace_fixture(c(
        "g <- function(x, ...) UseMethod('g')",
        "g.numeric <- function(x, digits=3, mode=c('short','long'), ...) NULL",
        "g(1, )"
    ))
    point <- list(row = 2L, col = 5L)
    result <- completion_reply(1L, fixture$uri, fixture$workspace, fixture$document, point, list())$result
    labels <- vapply(result$items, `[[`, "", "label")
    expect_true(all(c("digits", "mode") %in% labels))
    parameter <- result$items[[which(labels == "mode")[[1L]]]]
    expect_identical(parameter$data$type, "s3_parameter")
    fixture$document$set_content(2L, c(fixture$document$content[1:2], "g(1, mode=\"l"))
    parsed <- parse_document(fixture$uri, fixture$document$content)
    parsed$version <- 2L
    fixture$workspace$update_parse_data(fixture$uri, parsed)
    point <- list(row = 2L, col = nchar(fixture$document$line0(2L)))
    quoted <- completion_reply(1L, fixture$uri, fixture$workspace, fixture$document, point, list())$result
    expect_identical(quoted$items[[1L]]$label, "long")
    expect_identical(quoted$items[[1L]]$textEdit$newText, "long")
})

test_that("S3 dispatch resolves cross-file source definitions and namespace aliases", {
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines(c("Package: s3fixture", "Version: 0.0.1"), file.path(root, "DESCRIPTION"))
    writeLines("S3method(g, numeric, numeric_impl)", file.path(root, "NAMESPACE"))
    definitions <- file.path(root, "R", "definitions.R")
    target <- file.path(root, "R", "target.R")
    writeLines(c(
        "g <- function(x, ...) UseMethod('g')",
        "#' @param digits Digits to retain.", "numeric_impl <- function(x, digits=3, ...) NULL",
        "replacement_impl <- function(x, fresh=TRUE, ...) NULL"
    ), definitions)
    writeLines("g(1, )", target)
    workspace <- Workspace$new(root)
    workspace$index$update_path(definitions)
    workspace$index$update_path(target)
    uri <- path_to_uri(target)
    doc <- Document$new(uri, content = "g(1, )", version = 1L)
    workspace$documents$set(uri, doc)
    parsed <- parse_document(uri, doc$content)
    parsed$version <- 1L
    workspace$update_parse_data(uri, parsed)
    result <- resolve_s3_call(uri, workspace, doc, list(row = 0L, col = 5L))
    expect_identical(result$method$name, "numeric_impl")
    expect_match(result$signature, "digits", fixed = TRUE)
    expect_match(s3_documentation(workspace, result)$arguments$digits, "Digits", fixed = TRUE)
    writeLines("S3method(g, numeric, replacement_impl)", file.path(root, "NAMESPACE"))
    result <- resolve_s3_call(uri, workspace, doc, list(row = 0L, col = 5L))
    expect_identical(result$method$name, "replacement_impl")
    expect_match(result$signature, "fresh", fixed = TRUE)
})

test_that("S3 catalog handles registration closures and namespace dependencies", {
    skip_if_not_installed("tibble")
    catalogs <- s3_prepare_packages("tibble")
    expect_true("pillar" %in% names(catalogs))
    fixture <- s3_fixture(c("x <- tibble::tibble(a=1:3)", "print(x, )"))
    for (package in names(catalogs)) fixture$workspace$s3_metadata$set(package, catalogs[[package]])
    result <- s3_result(fixture)
    expect_identical(result$classes, c("tbl_df", "tbl", "data.frame"))
    expect_identical(result$method$name, "print.tbl")
})

test_that("S3 source registrations match names and retain uncertain environments", {
    base <- c(
        "g <- function(x, ...) UseMethod('g')", "g.default <- function(x, ...) NULL",
        "impl <- function(x, registered=TRUE, ...) NULL"
    )
    fixture <- s3_fixture(c(
        base,
        "base::registerS3method(class='numeric',method=impl,genname='g')", "g(1, )"
    ))
    expect_match(s3_result(fixture)$signature, "registered", fixed = TRUE)
    fixture <- s3_fixture(c(
        base,
        "registerS3method('g','numeric',impl,envir=opaque())", "g(1, )"
    ))
    expect_null(s3_result(fixture))
    fixture <- s3_fixture(c(
        base,
        "registerS3method <- function(...) NULL", "registerS3method('g','numeric',impl)", "g(1, )"
    ))
    expect_null(s3_result(fixture))
    fixture <- s3_fixture(c(
        base, "registerS3method('g','numeric',impl)", "impl <- function(x, replaced=TRUE, ...) NULL", "g(1, )"
    ))
    expect_match(s3_result(fixture)$signature, "registered", fixed = TRUE)
    expect_false(grepl("replaced", s3_result(fixture)$signature, fixed = TRUE))
})

test_that("S3 namespace method aliases retain implementation ownership", {
    fixture <- s3_fixture(c(
        "g <- function(x, ...) UseMethod('g')", "g.numeric <- base::format", "g(1, )"
    ))
    result <- s3_result(fixture)
    original <- fixture$workspace$s3_metadata$get("base")$functions$format
    expect_identical(result$method$package, "base")
    expect_identical(result$method$body_hash, original$body_hash)
})

test_that("S3 registered base replacements preserve calling-environment precedence", {
    catalogs <- callr::r(function() {
        replacement <- function(x, registered = TRUE, ...) "registered"
        registerS3method("print", "data.frame", replacement)
        stopifnot(identical(print(data.frame(a = 1)), "registered"))
        get("s3_prepare_packages", asNamespace("languageserver"))(character())
    }, system_profile = FALSE, user_profile = FALSE)
    fixture <- s3_fixture("base::print(data.frame(a=1), )")
    for (package in names(catalogs)) fixture$workspace$s3_metadata$set(package, catalogs[[package]])
    expect_match(s3_result(fixture)$signature, "registered", fixed = TRUE)
    fixture <- s3_fixture(c(
        "print.data.frame <- function(x, local=TRUE, ...) NULL", "base::print(data.frame(a=1), )"
    ))
    for (package in names(catalogs)) fixture$workspace$s3_metadata$set(package, catalogs[[package]])
    expect_match(s3_result(fixture)$signature, "local", fixed = TRUE)
    fixture <- s3_fixture(c(
        "replacement <- function(x, registered=TRUE, ...) NULL",
        "base::registerS3method('print','data.frame',replacement)", "base::print(data.frame(a=1), )"
    ))
    expect_match(s3_result(fixture)$signature, "registered", fixed = TRUE)
})

test_that("S3 package load hooks are captured before startup registries are frozen", {
    root <- withr::local_tempdir()
    package_path <- file.path(root, "s3replacementfixture")
    library_path <- file.path(root, "library")
    dir.create(file.path(package_path, "R"), recursive = TRUE)
    dir.create(library_path)
    writeLines(c(
        "Package: s3replacementfixture", "Version: 0.0.1", "Title: S3 Registration Fixture",
        "Description: Verify isolated S3 method registration metadata.", "License: MIT",
        "Author: Test Fixture", "Maintainer: Test Fixture <fixture@example.org>"
    ), file.path(package_path, "DESCRIPTION"))
    writeLines(character(), file.path(package_path, "NAMESPACE"))
    writeLines(c(
        "replacement <- function(x, from_hook=TRUE, ...) 'registered'",
        ".onLoad <- function(...) registerS3method('print','data.frame',replacement)"
    ), file.path(package_path, "R", "methods.R"))
    install <- system2(file.path(R.home("bin"), "R"),
        c("CMD", "INSTALL", paste0("--library=", shQuote(library_path)), shQuote(package_path)),
        stdout = TRUE, stderr = TRUE)
    expect_identical(attr(install, "status") %s3_or% 0L, 0L, info = paste(install, collapse = "\n"))
    catalogs <- callr::r(function(library_path) {
        .libPaths(c(library_path, .libPaths()))
        catalogs <- get("s3_prepare_packages", asNamespace("languageserver"))("s3replacementfixture")
        stopifnot(identical(print(data.frame(a = 1)), "registered"))
        catalogs
    }, args = list(library_path), system_profile = FALSE, user_profile = FALSE)
    fixture <- s3_fixture("base::print(data.frame(a=1), )")
    for (package in names(catalogs)) fixture$workspace$s3_metadata$set(package, catalogs[[package]])
    result <- s3_result(fixture)
    expect_match(result$signature, "from_hook", fixed = TRUE)
    expect_identical(result$method$package, "s3replacementfixture")
})

test_that("S3 works over LSP on the first request after an edit", {
    skip_on_cran()
    client <- language_client()
    path <- withr::local_tempfile(fileext = ".R")
    lines <- c(
        "g <- function(x, ...) UseMethod('g')",
        "g.data.frame <- function(x, columns=NULL, ...) NULL",
        "g.numeric <- function(x, digits=3, ...) NULL", "x <- data.frame(a=1)", "g(x, )"
    )
    did_open(client, path, text = lines)
    result <- respond_signature(client, path, c(4L, 5L), retry = FALSE)
    expect_identical(result$signatures[[1L]]$label, "g(x, columns = NULL, ...)")
    edited <- c(lines[1:3], "x <- 1", lines[[5L]])
    notify(client, "textDocument/didChange", list(textDocument = list(uri = path_to_uri(path), version = 2L), contentChanges = list(list(text = paste(edited, collapse = "\n")))))
    result <- respond_completion(client, path, c(4L, 5L), retry = FALSE)
    labels <- vapply(result$items, `[[`, "", "label")
    expect_true("digits" %in% labels)
    expect_false("columns" %in% labels)
    result <- respond_signature(client, path, c(4L, 5L), retry = FALSE)
    expect_identical(result$signatures[[1L]]$label, "g(x, digits = 3, ...)")
    result <- respond_hover(client, path, c(4L, 0L), retry = FALSE)
    expect_match(paste(unlist(result$contents), collapse = " "), "g.numeric", fixed = TRUE)
})

test_that("S3 cold namespace requests wait for metadata", {
    skip_on_cran()
    skip_if_not_installed("datawizard")
    client <- language_client()
    path <- withr::local_tempfile(fileext = ".R")
    uri <- path_to_uri(path)
    notify(client, "textDocument/didOpen", list(textDocument = list(
        uri = uri, languageId = "r", version = 1L,
        text = "library(datawizard)\ndata(iris)\nrescale(iris, )"
    )))
    signature <- respond_signature(client, path, c(2L, 13L), retry = FALSE)
    expect_match(signature$signatures[[1L]]$label, "select", fixed = TRUE)
    expect_match(signature$signatures[[1L]]$documentation$value, "rescale.data.frame", fixed = TRUE)
    completion <- respond_completion(client, path, c(2L, 13L), retry = FALSE)
    labels <- vapply(completion$items, `[[`, "", "label")
    expect_true(all(c("select", "exclude", "append") %in% labels))
    hover <- respond_hover(client, path, c(2L, 2L), retry = FALSE)
    expect_match(paste(unlist(hover$contents), collapse = " "), "rescale.data.frame", fixed = TRUE)
})

test_that("S3 enabling the setting prepares already open namespace calls", {
    skip_on_cran()
    skip_if_not_installed("datawizard")
    client <- language_client()
    path <- withr::local_tempfile(fileext = ".R")
    notify(client, "workspace/didChangeConfiguration", list(settings = list(s3_dispatch = FALSE)))
    did_open(client, path, text = "datawizard::rescale(iris, )")
    generic <- respond_signature(client, path, c(0L, 24L), retry = FALSE)
    expect_false(grepl("select", generic$signatures[[1L]]$label, fixed = TRUE))
    notify(client, "workspace/didChangeConfiguration", list(settings = list(s3_dispatch = TRUE)))
    method <- respond_signature(client, path, c(0L, 24L), retry = FALSE)
    expect_match(method$signatures[[1L]]$label, "select", fixed = TRUE)
})

test_that("S3 caches do not reuse selections across metadata identities", {
    fixture <- s3_fixture("summary(iris, )")
    expect_identical(s3_result(fixture)$classes, "data.frame")
    replacement <- S3MetadataCache$new()
    for (p in c("base", "stats", "datasets")) replacement$set(p, s3_prepare_namespace(p))
    catalog <- replacement$get("base")
    catalog$methods[["base::summary.data.frame"]]$formals <- as.pairlist(alist(object = , fresh = TRUE, ... = ))
    catalog$generation <- "new_identity"
    replacement$set("base", catalog)
    fixture$workspace$s3_metadata <- replacement
    expect_match(s3_result(fixture)$signature, "fresh", fixed = TRUE)
})

test_that("S3 names may begin with dots and constructors match named inputs", {
    fixture <- s3_fixture(c(
        ".g <- function(x, ...) UseMethod('.g')",
        ".g.numeric <- function(x, digits=3, ...) NULL", ".x <- 1", ".g(.x, )"
    ))
    expect_identical(s3_result(fixture)$method$name, ".g.numeric")
    fixture <- s3_fixture(c(
        ".g <- function(x, ...) UseMethod('.g')",
        ".g.numeric <- function(x, digits=3, ...) NULL", ".x <- 1",
        "assign('.x', opaque())", ".g(.x, )"
    ))
    expect_null(s3_result(fixture))
    fixture <- s3_fixture("summary(ts(matrix(data=1:12,ncol=2),frequency=4), )")
    expect_identical(s3_result(fixture)$classes, c("mts", "ts", "matrix", "array"))
    expect_identical(s3_result(fixture)$method$name, "summary.matrix")
})

test_that("S3 time-series classes account for dimensions and class overrides", {
    base <- c(
        "g <- function(x, ...) UseMethod('g')", "g.ts <- function(x, single=TRUE, ...) NULL",
        "g.mts <- function(x, multiple=TRUE, ...) NULL", "g.custom <- function(x, custom=TRUE, ...) NULL"
    )
    for (columns in 1:2) {
        fixture <- s3_fixture(c(base, sprintf("g(stats::ts(matrix(1:6,ncol=%d)), )", columns)))
        expect_identical(s3_result(fixture)$method$name, if (columns == 1L) "g.ts" else "g.mts")
    }
    fixture <- s3_fixture(c(base, "g(stats::ts(1:6,class='custom'), )"))
    expect_identical(s3_result(fixture)$method$name, "g.custom")
    fixture <- s3_fixture(c(base, "g(stats::ts(matrix(1:6,ncol=opaque())), )"))
    expect_null(s3_result(fixture))
    fixture <- s3_fixture(c(base, "g(stats::ts(data.frame(a=1:6,b=1:6)), )"))
    expect_null(s3_result(fixture))
})

test_that("S3 matrix coercion does not assume explicitly classed storage is plain", {
    fixture <- s3_fixture(c(
        "g <- function(x, ...) UseMethod('g')", "g.matrix <- function(x, matrix=TRUE, ...) NULL",
        "g(matrix(structure(1:6,class=c('integer','numeric')),ncol=2), )"
    ))
    expect_null(s3_result(fixture))
    for (constructor in c("matrix(NULL,nrow=2,ncol=2)", "array(NULL,dim=c(2,2))")) {
        fixture <- s3_fixture(c(
            "g <- function(x, ...) UseMethod('g')", "g.logical <- function(x, logical=TRUE, ...) NULL",
            "g.default <- function(x, default=TRUE, ...) NULL", paste0("g(", constructor, ", )")
        ))
        expect_null(s3_result(fixture))
    }
})

test_that("S3 argument matching applies exact names before partial names", {
    fn <- function(abc, abd, ..., after = NULL) NULL
    for (code in c("fn(a=1,abc=2)", "fn(abc=1,a=2)", "fn(abc=1,abd=2,ab=3)", "fn(abc=1,after=2,3)")) {
        call <- parse(text = code)[[1L]]
        actuals <- as.list(call)[-1L]
        runtime <- as.list(match.call(fn, call, expand.dots = FALSE))[-1L]
        expected <- vapply(actuals, function(value) {
            ordinary <- setdiff(names(runtime), "...")
            found <- ordinary[vapply(runtime[ordinary], identical, logical(1L), value)]
            if (length(found)) found[[1L]] else "..."
        }, character(1L))
        matched <- s3_match_arguments(formals(fn), actuals)
        expect_identical(names(formals(fn))[matched], unname(expected), info = code)
    }
    expect_null(s3_match_arguments(formals(fn), as.list(quote(fn(a = 1, ab = 2)))[-1L]))
    fixture <- s3_fixture(c(
        "g <- function(abc, abd, ...) UseMethod('g', abd)",
        "g.numeric <- function(abc, abd, digits=3, ...) NULL",
        "make <- function(abc, abd) abd", "g(a=make(a=1,abc=NULL),abc=NULL)"
    ))
    expect_identical(s3_result(fixture)$method$name, "g.numeric")
})

test_that("S3 quoted completion uses the active method parameter", {
    fixture <- s3_fixture(c(
        "g <- function(x, ...) UseMethod('g')",
        "g.numeric <- function(x, first=c('alpha','beta'), second=c('gamma','delta'), ...) NULL", "g(1, second=\"g"
    ))
    point <- list(row = 2L, col = nchar(fixture$document$line0(2L)))
    items <- s3_quoted_values(fixture$workspace, fixture$document, point)
    expect_identical(vapply(items, `[[`, "", "label"), "gamma")
})

test_that("S3 source formals shadow global object classes", {
    fixture <- s3_fixture(c(
        "g <- function(x, ...) UseMethod('g')",
        "g.data.frame <- function(x, columns=NULL, ...) NULL", "x <- data.frame(a=1)",
        "f <- function(x) {", "  g(x, )", "}"
    ))
    expect_null(resolve_s3_call(fixture$uri, fixture$workspace, fixture$document, list(row = 4L, col = 7L)))
})

test_that("S3 factories retain uncertainty from early returns and global writes", {
    base <- c(
        "g <- function(x, ...) UseMethod('g')",
        "g.data.frame <- function(x, columns=NULL, ...) NULL",
        "g.numeric <- function(x, digits=3, ...) NULL"
    )
    for (factory in c(
        "make <- function(flag) {if(flag) return(1); data.frame(a=1)}",
        "make <- function(flag) {x <<- 1; x}"
    )) {
        fixture <- s3_fixture(c(base, "x <- data.frame(a=1)", factory, "g(make(opaque()), )"))
        expect_null(s3_result(fixture), info = factory)
    }
    fixture <- s3_fixture(c(
        base,
        "make <- function(flag) {if(flag) return(data.frame(a=1)) else return(data.frame(b=2))}",
        "g(make(opaque()), )"
    ))
    expect_identical(s3_result(fixture)$method$name, "g.data.frame")
})

test_that("S3 source helpers and datasets respect shadowing and rebinding", {
    base <- c("g <- function(x, ...) UseMethod('g')", "g.data.frame <- function(x, columns=NULL, ...) NULL")
    for (lines in list(
        c("data <- function(...) NULL", "data(iris)", "g(iris, )"),
        c("data(iris)", "iris <- opaque()", "g(iris, )"),
        c("UseMethod <- function(...) NULL", "g(data.frame(a=1), )"),
        c("structure <- function(...) opaque()", "g(structure(list(),class='data.frame'), )"),
        c("`class<-` <- function(x,value) 1", "x <- data.frame(a=1)", "class(x) <- 'data.frame'", "g(x, )")
    )) {
        expect_null(s3_result(s3_fixture(c(base, lines))))
    }
})

test_that("S3 analysis never executes document expressions or method defaults", {
    assign("s3_dispatch_sentinel", FALSE, .GlobalEnv)
    withr::defer(rm("s3_dispatch_sentinel", envir = .GlobalEnv))
    fixture <- s3_fixture(c(
        "g <- function(x, ...) UseMethod('g')",
        "g.box <- function(x, mode={.GlobalEnv$s3_dispatch_sentinel <- TRUE; 'short'}, ...) {.GlobalEnv$s3_dispatch_sentinel <- TRUE}",
        "make <- function(value) {.GlobalEnv$s3_dispatch_sentinel <- TRUE; structure(list(value=value), class='box')}",
        "g(make({.GlobalEnv$s3_dispatch_sentinel <- TRUE; stop('never execute')}), )"
    ))
    expect_identical(s3_result(fixture)$method$name, "g.box")
    expect_false(get("s3_dispatch_sentinel", envir = .GlobalEnv))
})

test_that("S3 does not treat an unknown range as an integer vector", {
    fixture <- s3_fixture(c(
        "g <- function(x, ...) UseMethod('g')",
        "g.integer <- function(x, exact=TRUE, ...) NULL",
        "low <- opaque()", "g(low:10, )"
    ))
    expect_null(s3_result(fixture))
})

test_that("S3 placeholders and fallback indexes agree across providers", {
    base <- c(
        "g <- function(meta, x, ...) UseMethod('g', x)",
        "g.data.frame <- function(meta, x, columns=NULL, ...) NULL", "x <- data.frame(a=1)"
    )
    calls <- "x %>% g(meta=1,x=., )"
    if (getRversion() >= "4.2.0") calls <- c(calls, "x |> g(meta=1,x=_, )")
    for (call in calls) {
        fixture <- s3_fixture(c(base, call))
        result <- s3_result(fixture)
        expect_identical(result$method$name, "g.data.frame", info = call)
        fixture$document$parse_data$syntax_data <- NULL
        fixture$document$parse_data$completion_data <- NULL
        fixture$document$s3_calls$clear()
        expect_identical(s3_result(fixture)$method$name, "g.data.frame", info = call)
    }
})
