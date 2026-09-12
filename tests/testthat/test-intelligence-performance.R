test_that("Native signature ranges preserve Unicode and escaped defaults", {
    signature <- "f(α = \"😀\", β = 2)"
    expect_identical(extract_parameter_names(signature), c("α", "β"))
    expect_identical(parse_signature_parameters(signature), list(
        list(label = c(2L, 10L)), list(label = c(12L, 17L))))

    signature <- "fun(first = \"a\\\",b\", `odd=name` = list(1, 2), ...)"
    expect_identical(extract_parameter_names(signature),
        c("first", "`odd=name`", "..."))
    parameters <- parse_signature_parameters(signature)
    expect_length(parameters, 3L)
    expect_identical(substr(signature, parameters[[1L]]$label[[1L]] + 1L,
            parameters[[1L]]$label[[2L]]), "first = \"a\\\",b\"")

    raw_signature <- 'fun(first = r"--(a",b)--", second = 2)'
    expect_identical(extract_parameter_names(raw_signature), c("first", "second"))
    expect_identical(extract_parameter_names("incomplete(first,"), character())
})

test_that("Native active arguments respect comments, raw strings and cursor bounds", {
    signature <- "f(first, second, ..., `odd name` = NULL, β = 2)"
    active <- function(content, col = nchar(tail(content, 1L))) {
        detect_active_parameter(content, 0L, 1L, length(content) - 1L, col, signature)
    }
    expect_identical(active(c("f(1, # commas, in, comment", " β = ")), 4L)
    expect_identical(active('f(r"--(a",b)--", '), 1L)
    expect_identical(active("f(`a,b`, "), 1L)
    expect_identical(active("f(1,`odd name`="), 3L)
    expect_identical(active("f(1,β="), 4L)
    expect_identical(active("f(1, second == 2"), 1L)
    expect_identical(active("f(1, 2, 3, 4, "), 2L)
    # The character at the cursor belongs to the text after the request.
    expect_identical(active("f(1, second = 2)", col = 3L), 0L)
})

test_that("Local signatures depend on the header, including during body edits", {
    content <- c(
        "fun <- function(",
        "    α = list('a,b', function(x, y) x + y),",
        "    ...",
        ") {",
        "    α",
        "}")
    parsed <- parse(text = content, keep.source = TRUE)
    xdoc <- xml2::read_xml(xmlparsedata::xml_parse_data(utils::getParseData(parsed)))
    node <- xml_find_first(xdoc, "//expr[FUNCTION]")
    document <- Document$new("file:///header-signature.R", content = content)
    expected <- get_signature("fun", parsed[[1L]][[3L]])
    expect_identical(local_function_signature(document, node, "fun"), expected)
    # Editors can keep the last successful parse while the current function
    # body is unfinished; signature extraction still has a complete header.
    document$content[[5L]] <- "    α <- function("
    expect_identical(local_function_signature(document, node, "fun"), expected)
})

test_that("Workspace namespace indexes follow parse and membership changes", {
    documents <- collections::dict()
    first <- new.env(parent = emptyenv())
    first$uri <- "file:///first.R"
    first$parse_data <- list(
        functs = "fun", nonfuncts = "value", objects = c("fun", "value"),
        signatures = list(fun = "fun(first)"),
        functions = list(fun = function(first) NULL),
        documentation = list(fun = "First function"),
        definitions = list(fun = list(range = range(position(1, 0), position(1, 3)))))
    second <- new.env(parent = emptyenv())
    second$uri <- "file:///second.R"
    second$parse_data <- first$parse_data
    second$parse_data$signatures$fun <- "fun(second)"
    documents$set(first$uri, first)
    documents$set(second$uri, second)
    ns <- GlobalEnv$new(documents, c(first$uri, second$uri))

    expect_identical(ns$get_symbols(), "fun")
    expect_identical(ns$get_symbols(FALSE), "value")
    expect_true(ns$exists("value"))
    expect_false(ns$exists(""))
    expect_null(ns$get_signature(""))
    expect_true(ns$exists_funct("fun"))
    expect_identical(ns$get_signature("fun"), "fun(first)")
    expect_identical(names(ns$get_formals("fun")), "first")
    expect_identical(ns$get_documentation("fun"), "First function")
    expect_identical(ns$get_definition("fun")$uri, first$uri)

    first$parse_data$signatures$fun <- "fun(changed)"
    expect_identical(ns$get_signature("fun"), "fun(changed)")
    first$parse_data <- NULL
    expect_identical(ns$get_signature("fun"), "fun(second)")
    documents$remove(second$uri)
    expect_identical(ns$get_symbols(), character())
    expect_false(ns$exists_funct("fun"))
    expect_null(ns$get_definition("fun"))

    documents$set(second$uri, second)
    expect_identical(ns$get_signature("fun"), "fun(second)")
    ns$document_uris <- first$uri
    expect_null(ns$get_signature("fun"))
})

test_that("Package metadata caches retain export visibility and refresh functions", {
    ns <- PackageNamespace$new("stats")
    expect_true(ns$exists_funct("lm"))
    expect_false(ns$exists_funct("not_a_real_function"))
    expect_false(ns$exists(""))
    expect_null(ns$get_signature(""))
    expect_identical(ns$exists_funct(c("lm", "", NA_character_)), c(TRUE, FALSE, FALSE))
    expect_identical(ns$get_formals("lm"), formals(stats::lm))
    expect_null(PackageNamespace$new("base")$get_signature("if"))
    original <- ns$get_signature("lm")
    expect_identical(ns$get_signature("lm"), original)
    internal <- setdiff(ns$get_symbols(exported_only = FALSE), ns$get_symbols())[[1L]]
    expect_null(ns$get_signature(internal))
    expect_type(ns$get_signature(internal, exported_only = FALSE), "character")

    private <- ns$.__enclos_env__$private
    info <- private$function_info
    current_function <- function(replacement = "new") NULL
    mockery::stub(info, "get", function(...) current_function)
    expect_identical(names(info("lm", TRUE)$formals), "replacement")
    current_function <- function(changed_again) NULL
    expect_identical(names(info("lm", TRUE)$formals), "changed_again")

    # A namespace replacement invalidates all metadata, including documentation.
    private$namespace <- new.env(parent = emptyenv())
    expect_identical(ns$get_signature("lm"), original)
    expect_identical(ns$get_formals("lm"), formals(stats::lm))
})

test_that("Installed package completion cache follows library directory changes", {
    lib <- withr::local_tempdir()
    expect_identical(installed_package_names(lib), character())
    dir.create(file.path(lib, "perfFixture", "Meta"), recursive = TRUE)
    file.create(file.path(lib, "perfFixture", "Meta", "package.rds"))
    Sys.setFileTime(lib, Sys.time() + 10)
    expect_identical(installed_package_names(lib), "perfFixture")
    expect_identical(installed_package_names(lib), "perfFixture")
    unlink(file.path(lib, "perfFixture"), recursive = TRUE)
    Sys.setFileTime(lib, Sys.time() + 20)
    expect_identical(installed_package_names(lib), character())
    expect_identical(installed_package_names(withr::local_tempdir()), character())
})
