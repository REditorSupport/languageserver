Settings <- R6::R6Class("Settings",
    private = list(
        settings = list(
            debug = FALSE,
            log_file = NULL,
            diagnostics = TRUE,
            rich_documentation = TRUE,
            snippet_support = TRUE,
            max_completions = 200,
            lint_cache = TRUE,
            server_capabilities = list(),
            link_file_size_limit = 16L * 1024L^2
        )
    ),
    public = list(
        update_from_options = function() {
            # update default settings
            for (key in names(private$settings)) {
                prefixed_key <- paste0("languageserver.", key)
                if (hasName(options(), prefixed_key)) {
                    value <- getOption(prefixed_key)
                    logger$info("Update setting", key, "to", value)
                    self$set(key, value)
                }
            }
        },
        update_from_workspace = function(settings) {
            setting_keys <- names(settings)
            for (key in setting_keys) {
                prefixed_key <- paste0("languageserver.", key)
                if (hasName(options(), prefixed_key)) {
                    logger$info("Setting", key, "is masked by options(...).")
                } else {
                    self$set(key, settings[[key]])
                }
            }
        },
        get = function(key) {
            return(private$settings[[key]])
        },
        set = function(key, value) {
            private$settings[[key]] <- value
            return(self)
        }
    )
)


# create the settings object
lsp_settings <- Settings$new()
