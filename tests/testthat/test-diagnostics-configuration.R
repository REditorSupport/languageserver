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
    expect_length(self$deliveries, 3L)
    expect_identical(self$deliveries[[3L]]$params$diagnostics, warning)
})
