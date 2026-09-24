test_that("lintr works", {
    skip_on_cran()

    dir <- tempdir()
    client <- language_client(working_dir = dir, diagnostics = TRUE)

    lintr_file <- file.path(dir, ".lintr")
    on.exit(unlink(lintr_file), add = TRUE)
    writeLines("linters: linters_with_defaults()", lintr_file)

    temp_file <- withr::local_tempfile(tmpdir = dir, fileext = ".R")
    writeLines("a = 1", temp_file)

    client %>% did_open(temp_file)
    data <- client %>% wait_for("textDocument/publishDiagnostics")

    expect_equal(client$diagnostics$size(), 1)
    expect_equal(client$diagnostics$get(data$uri), data$diagnostics)
    expect_equal(data$diagnostics[[1]]$code, "assignment_linter")
    expect_true(stringi::stri_detect_fixed(data$diagnostics[[1]]$message, "assignment"))
    expect_true(stringi::stri_detect_fixed(data$diagnostics[[1]]$message, "not ="))
})

test_that("lintr config file works", {
    skip_on_cran()

    dir <- tempdir()
    lintr_file <- file.path(dir, ".lintr")
    on.exit(unlink(lintr_file))

    writeLines("linters: linters_with_defaults()", lintr_file)

    client <- language_client(working_dir = dir, diagnostics = TRUE)

    temp_file <- withr::local_tempfile(tmpdir = dir, fileext = ".R")
    writeLines("a=1", temp_file)

    client %>% did_open(temp_file)
    data <- client %>% wait_for("textDocument/publishDiagnostics")

    expect_equal(client$diagnostics$size(), 1)
    expect_equal(client$diagnostics$get(data$uri), data$diagnostics)
    expect_length(data$diagnostics, 2)
    expect_setequal(vapply(data$diagnostics, "[[", character(1), "code"),
        c("assignment_linter", "infix_spaces_linter"))


    writeLines("linters: linters_with_defaults(assignment_linter=NULL)", lintr_file)

    client <- language_client(working_dir = dir, diagnostics = TRUE)

    temp_file <- withr::local_tempfile(tmpdir = dir, fileext = ".R")
    writeLines("a=1", temp_file)

    client %>% did_open(temp_file)
    data <- client %>% wait_for("textDocument/publishDiagnostics")

    expect_equal(client$diagnostics$size(), 1)
    expect_equal(client$diagnostics$get(data$uri), data$diagnostics)
    expect_length(data$diagnostics, 1)
    expect_setequal(vapply(data$diagnostics, "[[", character(1), "code"),
        c("infix_spaces_linter"))

    writeLines("linters: list()", lintr_file)

    client <- language_client(working_dir = dir, diagnostics = TRUE)

    temp_file <- withr::local_tempfile(tmpdir = dir, fileext = ".R")
    writeLines("a=1", temp_file)

    client %>% did_open(temp_file)
    data <- client %>% wait_for("textDocument/publishDiagnostics")

    expect_equal(client$diagnostics$size(), 1)
    expect_equal(client$diagnostics$get(data$uri), data$diagnostics)
    expect_length(data$diagnostics, 0)
})

test_that("lintr is disabled", {
    skip_on_cran()
    client <- language_client(diagnostics = FALSE)

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines("a = 1", temp_file)

    client %>% did_open(temp_file)
    data <- client %>% wait_for("textDocument/publishDiagnostics", timeout = runif(1, 1, 3))
    expect_null(data)
})

test_that("Diagnostic conversion handles ranges, Unicode, and severities", {
    content <- paste0("x", intToUtf8(0x10400), "y")
    point_lint <- list(
        line_number = 1L,
        column_number = NA_integer_,
        ranges = NULL,
        type = "error",
        message = "problem",
        linter = "example_linter"
    )
    ranged_lint <- within(point_lint, {
        column_number <- 2L
        ranges <- list(c(2L, 2L))
        type <- "warning"
    })

    expect_equal(diagnostic_range(point_lint, content)$start$character, 0L)
    expect_equal(diagnostic_range(ranged_lint, content)$end$character, 3L)
    expect_equal(diagnostic_severity(point_lint), DiagnosticSeverity$Error)
    expect_equal(diagnostic_severity(ranged_lint), DiagnosticSeverity$Warning)
    expect_equal(
        diagnostic_severity(within(point_lint, type <- "style")),
        DiagnosticSeverity$Information
    )
    expect_equal(
        diagnostic_severity(within(point_lint, type <- "other")),
        DiagnosticSeverity$Information
    )

    converted <- diagnostic_from_lint(ranged_lint, content)
    expect_equal(converted$source, "lintr")
    expect_equal(converted$code, "example_linter")
    expect_match(converted$codeDescription$href, "example_linter.html", fixed = TRUE)
})

test_that("diagnose_file handles empty, prose-only, and untitled content", {
    expect_identical(diagnose_file("untitled:1", character()), list())
    expect_identical(
        diagnose_file(
            "file:///prose.Rmd", c("# Title", "Plain prose"),
            is_rmarkdown = TRUE
        ),
        list()
    )

    diagnostics <- suppressWarnings(diagnose_file(
        "untitled:1", "value=1", cache = FALSE
    ))
    expect_true(length(diagnostics) >= 1L)
    expect_true(all(vapply(diagnostics, function(item) {
        identical(item$source, "lintr")
    }, logical(1L))))

    globals <- new.env(parent = emptyenv())
    globals$known_global <- TRUE
    expect_type(suppressWarnings(diagnose_file(
        "untitled:1", "known_global", globals = globals, cache = FALSE
    )), "list")
    expect_false("languageserver:globals" %in% search())
})

test_that("diagnostic callbacks reject stale results and publish current ones", {
    uri <- "file:///diagnostics-callback.R"
    document <- Document$new(uri, version = 2L, content = "value <- 1")
    documents <- collections::dict()
    documents$set(uri, document)
    workspace <- list(documents = documents)
    self <- new.env(parent = baseenv())
    self$get_workspace <- function(...) workspace
    self$deliveries <- list()
    self$deliver <- function(message) {
        self$deliveries[[length(self$deliveries) + 1L]] <- message
    }

    expect_null(diagnostics_callback(self, uri, 1L, list()))
    expect_length(self$deliveries, 0L)
    expect_null(diagnostics_callback(self, uri, 2L, NULL))

    diagnostics_callback(self, uri, 2L, list())
    expect_length(self$deliveries, 1L)
    expect_equal(
        self$deliveries[[1L]]$method,
        "textDocument/publishDiagnostics"
    )
    expect_equal(self$deliveries[[1L]]$params$version, 2L)
})

test_that("diagnostics_task reuses fresh cached results", {
    uri <- "file:///cached-diagnostics.R"
    document <- Document$new(uri, version = 3L, content = "value <- 1")
    documents <- collections::dict()
    documents$set(uri, document)
    workspace <- new.env(parent = baseenv())
    workspace$root <- tempdir()
    workspace$documents <- documents
    workspace$diagnostics_cache <- ByteLruCache$new(1024^2)
    key <- paste(uri, get_content_hash(document$content), sep = "::")
    cached <- list(list(message = "cached"))
    workspace$diagnostics_cache$set(key, list(
        time = Sys.time(), diagnostics = cached
    ))

    self <- new.env(parent = baseenv())
    self$get_workspace <- function(...) workspace
    self$deliveries <- list()
    self$deliver <- function(message) {
        self$deliveries[[length(self$deliveries) + 1L]] <- message
    }
    old_ttl <- lsp_settings$get("diagnostics_cache_ttl")
    withr::defer(lsp_settings$set("diagnostics_cache_ttl", old_ttl))
    lsp_settings$set("diagnostics_cache_ttl", 60)

    expect_null(diagnostics_task(self, uri, document))
    expect_length(self$deliveries, 1L)
    expect_identical(self$deliveries[[1L]]$params$diagnostics, cached)

    lsp_settings$set("diagnostics_cache_ttl", NULL)
    task <- diagnostics_task(self, uri, document, delay = -1)
    expect_s3_class(task, "Task")
    expect_equal(task$delay, 0)
})

test_that("diagnose_file object_usage_linter acknowledges NAMESPACE imports and package globals", {
    pkg_dir <- withr::local_tempdir()
    dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
    writeLines(c(
        "Package: uninstalledgeompkg",
        "Version: 0.1.0"
    ), file.path(pkg_dir, "DESCRIPTION"))
    writeLines(c(
        "import(stats)",
        "importFrom(utils, head)",
        "importFrom(grDevices, rgb)",
        "importFrom(tools, file_path_sans_ext)"
    ), file.path(pkg_dir, "NAMESPACE"))
    writeLines(
        "linters: list(object_usage_linter())",
        file.path(pkg_dir, ".lintr")
    )

    helper_file <- file.path(pkg_dir, "R", "helper.R")
    writeLines(c(
        "pkg_helper <- function(x) file_path_sans_ext(x)",
        ".pkg_pronoun <- list(col = 1)",
        ""
    ), helper_file)

    main_file <- file.path(pkg_dir, "R", "main.R")
    main_code <- c(
        "my_fun <- function(df) {",
        "  val <- sd(c(1, 2, 3))",
        "  col <- rgb(1, 0, 0)",
        "  top <- head(df)",
        "  ext <- file_path_sans_ext('a.txt')",
        "  res <- pkg_helper(.pkg_pronoun$col)",
        "  list(val, col, top, ext, res)",
        "}",
        ""
    )
    writeLines(main_code, main_file)

    ws <- Workspace$new(pkg_dir)
    globals <- ws$get_diagnostics_globals(path_to_uri(main_file))
    diags <- diagnose_file(path_to_uri(main_file), main_code, globals = globals, cache = FALSE)
    expect_length(diags, 0L)

    bad_code <- c(
        main_code[seq_len(length(main_code) - 2L)],
        "  missing_fn(missing_var)",
        "}",
        ""
    )
    bad_diags <- diagnose_file(path_to_uri(main_file), bad_code, globals = globals, cache = FALSE)
    messages <- vapply(bad_diags, `[[`, character(1L), "message")
    expect_true(any(grepl("no visible global function definition for 'missing_fn'", messages, fixed = TRUE)))
    expect_true(any(grepl("no visible binding for global variable 'missing_var'", messages, fixed = TRUE)))
})
