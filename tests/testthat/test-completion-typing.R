test_that("Pruning each completion group preserves global rank and metadata", {
    namespaces <- lapply(seq_len(3L), function(i) {
        list(
            get_symbols = function(want_functs, ...) {
                prefix <- if (want_functs) "value_fun" else "value_field"
                c(sprintf("%s_%03d", prefix, 80:1),
                    sprintf("other_%s_%03d", prefix, 80:1), "value_\u03b1")
            },
            get_lazydata = function() sprintf("value_data_%03d", 80:1),
            exists_funct = function(...) TRUE
        )
    })
    names(namespaces) <- c(WORKSPACE, "first", "second")
    imports <- collections::dict()
    for (i in 80:1) {
        imports$set(sprintf("value_import_%03d", i),
            if (i %% 2L) "first" else "second")
    }
    workspace <- list(
        loaded_packages = c("first", "second"),
        imported_objects = imports,
        get_namespace = function(name) namespaces[[name]]
    )
    for (token in c("", "v", "value", "value_import", "\u03b1")) {
        complete <- workspace_completion(workspace, token, snippet_support = TRUE)
        labels <- vapply(complete, `[[`, character(1L), "label")
        sort_text <- vapply(complete, `[[`, character(1L), "sortText")
        for (limit in c(1L, 20L, 100L, length(complete))) {
            selected <- if (length(complete) > limit) {
                order(!startsWith(labels, token), sort_text, method = "radix")[seq_len(limit)]
            } else {
                seq_along(complete)
            }
            expected <- complete[selected]
            if (length(complete) > limit) attr(expected, "truncated") <- TRUE
            expect_identical(
                workspace_completion(workspace, token, snippet_support = TRUE, limit = limit),
                expected,
                info = paste("prefix", token, "limit", limit)
            )
        }
    }
})
