# for testing purpose

LanguageClient <- R6::R6Class("LanguageClient",
    inherit = LanguageBase,
    public = list(
        process = NULL,
        rootUri = NULL,
        ClientCapabilities = NULL,
        ServerCapabilities = NULL,
        diagnostics = NULL,

        initialize = function(cmd, args) {
            self$process <- processx::process$new(cmd, args,
                stdin = "|", stdout = "|", supervise = TRUE)
            self$diagnostics <- collections::DictL()
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
            if (!self$process$is_alive() || self$process$poll_io(1)[1] != "ready") return(NULL)
            line <- self$process$read_output_lines(1)
            trimws(line, "right")
        },

        read_char = function(n) {
            self$process$read_output(n)
        },

        read_error = function() {
            paste0(self$process$read_error_lines(), collapse = "\n")
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
