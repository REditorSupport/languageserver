Settings <- R6::R6Class("Settings",
    private = list(
        settings = list(
            debug = FALSE,
            trace = FALSE,
            log_file = NULL,
            diagnostics = TRUE,
            diagnostics_backend = "lintr",
            formatting_backend = "styler",
            backend_options = list(),
            rich_documentation = TRUE,
            snippet_support = TRUE,
            completion_label_details = FALSE,
            max_completions = 200,
            glue_highlighting = TRUE,
            glue_highlight_functions = list(),
            member_completion = TRUE,
            roxygen_type_hints = TRUE,
            s3_dispatch = TRUE,
            lint_cache = FALSE,
            diagnostics_cache_ttl = 5,
            parse_delay = 0.15,
            diagnostics_delay = 0.75,
            parse_cache_max_mb = 64,
            diagnostics_cache_max_mb = 16,
            index_mode = "auto",
            index_include = "**/*.R",
            index_exclude = c(
                "**/.git/**",
                "**/.svn/**",
                "**/.hg/**",
                "**/renv/**",
                "**/packrat/**",
                "**/.Rproj.user/**",
                "**/.cache/**",
                "**/node_modules/**",
                "**/build/**",
                "**/dist/**"
            ),
            index_max_files = 10000L,
            index_max_file_size_mb = 2,
            index_batch_size = 20L,
            index_time_budget_ms = 25,
            index_persistent_cache = TRUE,
            project_helpers = list(),
            server_capabilities = list(),
            link_file_size_limit = 16L * 1024L^2,
            nline_to_break_succession = 2L,
            inlay_hints_minimum_arguments = 2L,
            inlay_hints_minimum_argument_length = 2L
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
# note that this object should not be used in sessions created by callr because
# settings may be updated from the lsp configuration
lsp_settings <- Settings$new()

roxygen_type_hints_enabled <- function() isTRUE(lsp_settings$get("roxygen_type_hints"))

# Read the setting in the server process so a change applies to accepted parses.
roxygen_inference_hints <- function(node) {
    hints <- attr(node, "member_type_hints", exact = TRUE)
    if (!is.null(hints) && roxygen_type_hints_enabled()) hints
}
