# for testing purpose

LanguageClient <- R6::R6Class("LanguageClient",
    inherit = LanguageBase,
    public = list(
        error_buffer = NULL,
        process = NULL,
        rootUri = NULL,
        ClientCapabilities = NULL,
        ServerCapabilities = NULL,

        initialize = function(cmd, args) {
            self$process <- processx::process$new(cmd, args,
                stdin = "|", stdout = "|", stderr = "|", supervise = TRUE)
            self$error_buffer <- ""
            super$initialize()
        },

        finalize = function() {
            self$process$kill()
            super$finalize()
        },

        check_connection = function() {
            if (!self$process$is_alive())
                stop("Server is dead.")
        },

        write_text = function(text) {
            self$process$write_input(text)
        },

        read_line = function() {
            line <- self$process$read_output_lines(1)
            trimws(line, "right")
        },

        read_char = function(n) {
            self$process$read_output(n)
        },

        fetch_error = function() {
            self$error_buffer <- paste0(
                self$error_buffer, "\n",
                paste0(self$process$read_error_lines(), collapse = "\n")
                )
        },

        read_error = function() {
            buf <- self$error_buffer
            self$error_buffer <- ""
            buf
        },

        welcome = function() {
            self$deliver(
                self$request(
                    "initialize",
                    list(
                        rootUri = self$rootUri,
                        capabilities = self$ClientCapabilities
                    )
                ),
                callback = function(self, result) {
                    self$ServerCapabilities <- result$capabilities
                }
            )
        },

        start = function(working_dir = getwd(), capabilities = NULL) {
            self$rootUri <- path_to_uri(working_dir)
            self$ClientCapabilities <- capabilities
            self$welcome()
        },

        run = function() {
            # placeholder
        }
    )
)


LanguageClient$set("public", "register_handlers", function() {
    self$request_handlers <- list()
    self$notification_handlers <- list()
})
