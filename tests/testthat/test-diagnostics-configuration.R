skip_if_not_installed("lintr", "3.3.0.1")

test_that("diagnostics use nested configuration for saved and new buffers", {
    root <- withr::local_tempdir()
    nested <- file.path(root, "nested")
    dir.create(nested)
    withr::local_dir(root)
    writeLines("linters: list()", file.path(root, ".lintr"))
    writeLines("linters: list(assignment_linter())", file.path(nested, ".lintr"))

    for (extension in c("R", "Rmd", "qmd")) {
        literate <- extension != "R"
        content <- if (literate) {
            c("# Title", "", "```{r}", "value = 1", "```",
                "~~~{r}", "unfinished <- (", "~~~",
                "```{python}", "invalid =", "```")
        } else {
            c("value = 1", "")
        }
        for (saved in c(TRUE, FALSE)) {
            path <- file.path(nested, paste0(if (saved) "saved." else "new.", extension))
            if (saved) writeLines(content, path)
            diagnostics <- diagnose_file(path_to_uri(path), content,
                is_rmarkdown = literate)
            expect_length(diagnostics, 1L)
            expect_equal(diagnostics[[1L]]$code, "assignment_linter")
            expect_equal(diagnostics[[1L]]$range$start$line, if (literate) 3L else 0L)
            expect_equal(file.exists(path), saved)
        }
    }
})

test_that("literate diagnostics preserve custom markers and config exclusions", {
    root <- withr::local_tempdir()
    withr::local_dir(root)
    content <- c("# Title", "```{r}", "first = 1", "second = 2 # quiet",
        "third = 3", "```")
    writeLines(c(
        "linters: list(assignment_linter())",
        "exclude: '# quiet'",
        "exclusions: list('source.Rmd' = 3L, 'source.qmd' = 3L)"
    ), file.path(root, ".lintr"))

    for (extension in c("Rmd", "qmd")) {
        path <- file.path(root, paste0("source.", extension))
        writeLines(content, path)
        diagnostics <- diagnose_file(path_to_uri(path), content, is_rmarkdown = TRUE)
        expect_length(diagnostics, 1L)
        expect_equal(diagnostics[[1L]]$code, "assignment_linter")
        expect_equal(diagnostics[[1L]]$range$start$line, 4L)
    }

    writeLines(c("linters: list(assignment_linter())",
            "exclusions: list('source.Rmd', 'source.qmd')"), file.path(root, ".lintr"))
    for (extension in c("Rmd", "qmd")) {
        path <- file.path(root, paste0("source.", extension))
        expect_length(diagnose_file(path_to_uri(path), content,
                is_rmarkdown = TRUE), 0L)
    }
})

test_that("literate diagnostics restore lintr settings after errors", {
    root <- withr::local_tempdir()
    config <- file.path(root, ".lintr")
    path <- file.path(root, "source.Rmd")
    settings <- asNamespace("lintr")$settings
    setting_values <- function() mget(sort(ls(settings, all.names = TRUE)), settings)
    before <- setting_values()
    writeLines("linters: list(assignment_linter())", config)
    diagnose_file(path_to_uri(path), c("```{r}", "value = 1", "```"),
        is_rmarkdown = TRUE)
    expect_equal(setting_values(), before)

    writeLines("linters: list(", config)
    expect_error(diagnose_file(path_to_uri(path),
            c("```{r}", "value = 1", "```"), is_rmarkdown = TRUE))
    expect_equal(setting_values(), before)
})

test_that("pathless diagnostics use working directory settings and reload edits", {
    root <- withr::local_tempdir()
    withr::local_options(lintr.cache_directory = withr::local_tempdir())
    nested <- file.path(root, "nested")
    dir.create(nested)
    withr::local_dir(nested)
    config <- file.path(root, ".lintr")
    content <- c("value=1", "# value <- sum(1, 2)",
        paste0('long_value <- "', strrep("a", 90), '"'))
    uris <- c("untitled:1", "vscode-interactive-input:InteractiveInput-1",
        "vscode-r-cell:cell-1")
    diagnostic_codes <- function(uri) {
        vapply(diagnose_file(uri, content, cache = TRUE), "[[", character(1), "code")
    }

    writeLines("linters: list(infix_spaces_linter())", config)
    for (uri in uris) {
        expect_identical(diagnostic_codes(uri), "infix_spaces_linter")
    }

    writeLines("linters: list(line_length_linter(40), commented_code_linter())", config)
    for (uri in uris) {
        expect_setequal(diagnostic_codes(uri),
            c("line_length_linter", "commented_code_linter"))
    }

    writeLines("linters: list()", config)
    for (uri in uris) {
        expect_length(diagnostic_codes(uri), 0L)
    }
})

test_that("pathless diagnostics honor custom configuration and linter options", {
    root <- withr::local_tempdir()
    withr::local_dir(root)
    config <- withr::local_tempfile(fileext = ".lintr")
    writeLines(c("linters: list(assignment_linter())", "exclude: '# quiet'"), config)
    withr::local_options(lintr.linter_file = config)
    content <- c("first = 1", "second = 2 # quiet")
    diagnostics <- diagnose_file("untitled:1", content)
    expect_length(diagnostics, 1L)
    expect_equal(diagnostics[[1L]]$code, "assignment_linter")
    expect_equal(diagnostics[[1L]]$range$start$line, 0L)

    withr::local_options(lintr.linters = list())
    expect_length(diagnose_file("untitled:1", content), 0L)

    unlink(config)
    expect_length(diagnose_file("untitled:1", content), 0L)
})

test_that("pathless diagnostics restore lintr settings after success and errors", {
    root <- withr::local_tempdir()
    withr::local_dir(root)
    config <- file.path(root, ".lintr")
    settings <- asNamespace("lintr")$settings
    setting_values <- function() mget(sort(ls(settings, all.names = TRUE)), settings)
    before <- setting_values()

    writeLines("linters: list(assignment_linter())", config)
    diagnose_file("untitled:1", "value = 1")
    expect_equal(setting_values(), before)

    writeLines("linters: list(", config)
    expect_error(diagnose_file("untitled:1", "value = 1"))
    expect_equal(setting_values(), before)
})

test_that("pathless diagnostics support linters that inspect the source filename", {
    root <- withr::local_tempdir()
    withr::local_dir(root)
    writeLines("linters: list(namespace_linter())", file.path(root, ".lintr"))
    diagnostics <- diagnose_file("untitled:1", "base::missing_lintr_test_export()")
    expect_length(diagnostics, 1L)
    expect_equal(diagnostics[[1L]]$code, "namespace_linter")
})

test_that("missing files support filename-inspecting linters and local configuration", {
    root <- withr::local_tempdir()
    nested <- file.path(root, "nested")
    dir.create(nested)
    withr::local_dir(root)
    withr::local_options(lintr.linter_file = ".lintr",
        lintr.cache_directory = withr::local_tempdir())
    writeLines("linters: list(object_length_linter(5L))", file.path(root, ".lintr"))
    writeLines("linters: list(assignment_linter())", file.path(nested, ".lintr"))
    content <- c("long_name <- 1", "")
    local_path <- file.path(nested, "new.R")
    remote_path <- file.path(withr::local_tempdir(), "client-only", "source.R")

    expect_length(diagnose_file(path_to_uri(local_path), content, cache = TRUE), 0L)
    diagnostics <- diagnose_file(path_to_uri(remote_path), content, cache = TRUE)
    expect_length(diagnostics, 1L)
    expect_equal(diagnostics[[1L]]$code, "object_length_linter")
    expect_equal(diagnostics[[1L]]$range$start$line, 0L)
    expect_false(file.exists(local_path))
    expect_false(file.exists(remote_path))
    expect_false(dir.exists(dirname(remote_path)))

    content <- c("long_name = 1", "")
    local_diagnostics <- diagnose_file(path_to_uri(local_path), content)
    expect_length(local_diagnostics, 1L)
    expect_equal(local_diagnostics[[1L]]$code, "assignment_linter")
    paths <- lintr_configuration_paths(remote_path, root, list(lintr.linter_file = ".lintr"))
    expect_equal(paths[[1L]], normalizePath(file.path(root, ".lintr"), winslash = "/"))
})

test_that("missing file diagnostics retain options, exclusions and terminal newlines", {
    root <- withr::local_tempdir()
    withr::local_dir(root)
    cache <- withr::local_tempdir()
    config <- withr::local_tempfile(fileext = ".lintr")
    withr::local_options(lintr.linter_file = config, lintr.cache_directory = cache)
    writeLines(c("linters: list(object_length_linter(5L), trailing_blank_lines_linter())",
            "exclude: '# quiet'"), config)
    path <- file.path(root, "client-only", "source.R")
    uri <- path_to_uri(path)
    codes <- function(content) {
        vapply(diagnose_file(uri, content, cache = TRUE), "[[", character(1L), "code")
    }

    expect_setequal(codes(c("long_name <- 1", "x <- 1")),
        c("object_length_linter", "trailing_blank_lines_linter"))
    expect_identical(codes(c("long_name <- 1", "x <- 1", "")), "object_length_linter")
    expect_length(codes(c("long_name <- 1 # quiet", "x <- 1 # quiet")), 0L)
    expect_length(list.files(cache, all.files = TRUE, no.. = TRUE), 0L)

    writeLines("linters: list(object_length_linter(99L))", config)
    expect_length(codes(c("long_name <- 1", "")), 0L)
    withr::local_options(lintr.linters = list(assignment_linter = lintr::assignment_linter()))
    expect_identical(codes(c("value = 1", "")), "assignment_linter")
})

test_that("missing file diagnostics restore lintr settings after errors", {
    root <- withr::local_tempdir()
    withr::local_dir(root)
    config <- file.path(root, ".lintr")
    withr::local_options(lintr.linter_file = ".lintr")
    settings <- asNamespace("lintr")$settings
    setting_values <- function() mget(sort(ls(settings, all.names = TRUE)), settings)
    before <- setting_values()
    uri <- path_to_uri(file.path(root, "client-only", "source.R"))

    writeLines("linters: list(object_length_linter(5L))", config)
    diagnose_file(uri, c("long_name <- 1", ""))
    expect_equal(setting_values(), before)
    writeLines("linters: list(", config)
    expect_error(diagnose_file(uri, "value = 1"))
    expect_equal(setting_values(), before)
})

test_that("disabling diagnostics clears documents and cancels pending work", {
    old_diagnostics <- lsp_settings$get("diagnostics")
    withr::defer(lsp_settings$set("diagnostics", old_diagnostics))
    withr::local_options(languageserver.diagnostics = NULL)
    lsp_settings$set("diagnostics", TRUE)
    uri <- "file:///disable-diagnostics.R"
    workspace <- list(documents = collections::dict())
    workspace$documents$set(uri, Document$new(uri, version = 1L, content = "x = 1"))
    self <- new.env(parent = baseenv())
    self$get_workspace <- function(...) workspace
    self$workspaces <- collections::dict()
    self$workspaces$set("root", workspace)
    self$deliveries <- list()
    self$deliver <- function(message) {
        self$deliveries[[length(self$deliveries) + 1L]] <- message
    }
    self$diagnostics_task_manager <- TaskManager$new("diagnostics")
    withr::defer(self$diagnostics_task_manager$stop())
    warning <- list(list(message = "existing warning"))
    diagnostics_callback(self, uri, 1L, warning)
    self$diagnostics_task_manager$add_task(uri, create_task(function() NULL, list()))
    expect_true(self$diagnostics_task_manager$has_work())

    workspace_did_change_configuration(self, list(settings = list(diagnostics = FALSE)))
    expect_length(self$deliveries, 2L)
    expect_identical(self$deliveries[[2L]]$params$diagnostics, list())
    expect_equal(self$deliveries[[2L]]$params$uri, uri)
    expect_false(self$diagnostics_task_manager$has_work())

    diagnostics_callback(self, uri, 1L, warning)
    diagnostics_callback(self, uri, 1L, list())
    expect_length(self$deliveries, 2L)
    workspace_did_change_configuration(self, list(settings = list(diagnostics = TRUE)))
    diagnostics_callback(self, uri, 1L, warning)
    expect_length(self$deliveries, 4L)
    expect_identical(self$deliveries[[3L]]$params$diagnostics, list())
    expect_identical(self$deliveries[[4L]]$params$diagnostics, warning)
})
