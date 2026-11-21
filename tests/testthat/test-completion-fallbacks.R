test_that("argument completion uses the nearest visible declaration in XML-only snapshots", {
    f <- provider_fixture(c(
        "render <- function(global) NULL", "render()",
        "outer <- function() {",
        "  render <- function(local, `special name`) NULL",
        "  render()",
        "}"
    ))
    f$workspace$guess_namespace <- function(...) NULL
    arguments <- function(row) arg_completion(f$uri, f$workspace, list(row = row, col = 8L), "", "render")
    indexed <- arguments(4L)
    f$document$parse_data$syntax_data <- NULL
    actual <- arguments(4L)
    expect_identical(actual, indexed)
    expect_identical(vapply(actual, `[[`, "", "label"), c("local", "`special name`"))
    expect_identical(actual[[2L]]$data$matching_name, "special name")
    expect_identical(actual[[2L]]$data$line, 4L)
    expect_identical(vapply(arguments(1L), `[[`, "", "label"), "global")

    f$workspace$guess_namespace <- function(...) "pkg"
    f$workspace$get_formals <- function(name, package, exported_only, uri) {
        expect_identical(list(name, package, exported_only, uri), list("other", "pkg", FALSE, f$uri))
        alist(style = c("short", "long"))
    }
    args <- arg_completion(f$uri, f$workspace, list(row = 3L, col = 8L), "sty", "other",
        exported_only = FALSE)
    expect_identical(args[[1L]]$data$context_uri, f$uri)
    values <- argument_value_completion(f$workspace, "other", "pkg", "style", "sh",
        exported_only = FALSE, uri = f$uri)
    expect_identical(values[[1L]]$label, "short")
    expect_identical(values[[1L]]$insertText, '"short"')
    f$workspace$get_formals <- function(...) NULL
    expect_length(argument_value_completion(f$workspace, "other", "pkg", "style", "", uri = f$uri), 0L)
})

test_that("completion signatures fall back to document navigation and workspace namespaces", {
    f <- provider_fixture(c("render_plot <- function(colour) NULL", "rend"))
    f$document$parse_data$member_data <- NULL
    point <- list(row = 1L, col = 4L)
    token <- completion_token(f$document, point)
    enrich <- function(name, package = NULL) {
        item <- list(label = name, data = list(type = "function", package = package))
        completion_symbol_info(f$uri, f$workspace, f$document, point, token, list(item))[[1L]]
    }
    local <- enrich("render_plot")
    expect_identical(local$data$signature, "render_plot(colour)")
    expect_identical(local$labelDetails$detail, "(colour)")

    f$workspace$get_namespace <- function(package, uri) {
        expect_identical(list(package, uri), list(WORKSPACE, f$uri))
        list(get_signature = function(name) paste0(name, "(current)"))
    }
    source <- enrich("peer_render", WORKSPACE)
    expect_identical(source$labelDetails, list(detail = "(current)", description = "static"))
    f$workspace$get_namespace <- NULL
    f$workspace$global_env <- list(get_signature = function(name) paste0(name, "(cached)"))
    expect_identical(enrich("peer_render", WORKSPACE)$labelDetails$detail, "(cached)")
    f$workspace$global_env <- list(get_signature = function(...) stop("source no longer indexed"))
    expect_null(enrich("peer_render", WORKSPACE)$labelDetails)
})

test_that("resolving a package function adds a signature only when the client can display it", {
    old <- lsp_settings$get("completion_label_details")
    withr::defer(lsp_settings$set("completion_label_details", old))
    lsp_settings$set("completion_label_details", TRUE)
    f <- provider_fixture("render")
    f$workspace$get_signature <- function(name, package, uri) {
        expect_identical(list(name, package, uri), list("render plot", "pkg", f$uri))
        "`render plot`(object, format)"
    }
    f$workspace$get_documentation <- function(...) "Draw a plot."
    item <- list(label = "render plot", data = list(type = "function", package = "pkg", context_uri = f$uri))
    capabilities <- list(completionItem = list(
        labelDetailsSupport = TRUE, resolveSupport = list(properties = "labelDetails")
    ))
    resolve <- function() completion_item_resolve_reply(1L, f$workspace, item, capabilities)$result
    selected <- resolve()
    expect_identical(selected$labelDetails, list(detail = "(object, format)", description = "static"))
    expect_identical(selected$documentation$value, "Draw a plot.")
    capabilities$completionItem$resolveSupport$properties <- "documentation"
    expect_null(resolve()$labelDetails)
    capabilities$completionItem$resolveSupport$properties <- "labelDetails"
    f$workspace$get_signature <- function(...) NULL
    expect_null(resolve()$labelDetails)
})

test_that("unreferenced parsed source retains global declarations without running code", {
    code <- c("if (flag) maybe <- 3L", "for (entry in values) { found <- entry }",
        "record <- function(colour) { only_local <- colour }", "plain <- 3L")
    outline <- completion_global_outline(parse(text = code, keep.source = FALSE), previews = TRUE)
    expect_setequal(names(outline), c("maybe", "entry", "found", "record", "plain"))
    expect_true(outline$record$function_kind)
    expect_true(outline$maybe$uncertain)
    expect_true(outline$found$uncertain)
    expect_identical(outline$entry$line, 1L)
    expect_identical(outline$plain$column, 1L)
    expect_null(outline$maybe$value)
    expect_false(outline$plain$uncertain)
})
