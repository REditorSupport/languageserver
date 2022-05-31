# for testing purpose

LanguageClient <- R6::R6Class("LanguageClient",
    inherit = LanguageBase,
    private = list(
        read_char_buf = raw(0),

        finalize = function() {
            if (!is.null(self$process)) {
                self$process$kill_tree()
            }
            super$finalize()
        }
    ),
    public = list(
        process = NULL,
        rootUri = NULL,
        ClientCapabilities = NULL,
        ServerCapabilities = NULL,
        diagnostics = NULL,

        initialize = function(cmd = NULL, args = NULL) {
            if (!is.null(cmd)) {
                self$process <- processx::process$new(cmd, args,
                    stdin = "|", stdout = "|")
            }
            self$diagnostics <- collections::dict()
            super$initialize()
        },

        check_connection = function() {
            if (!is.null(self$process) && !self$process$is_alive())
                stop("Server is dead.")
        },

        write_text = function(text) {
            self$check_connection()
            self$process$write_input(text)
        },

        read_output_lines = function() {
            self$check_connection()
            if (self$process$is_alive() && self$process$poll_io(1)[1] != "ready") return(NULL)
            self$process$read_output_lines(1)
        },

        read_line = function() {
            self$check_connection()
            buf <- private$read_char_buf
            if (length(buf) > 0 && as.raw(10) %in% buf) {
                first_match <- min(which(buf == charToRaw("\n")))
                line <- buf[seq_len(first_match - 1)]
                if (length(line) > 0 && line[length(line)] == charToRaw("\r")) {
                    line <- line[-length(line)]
                }
                private$read_char_buf <- buf[seq_safe(first_match + 1, length(buf))]
                return(rawToChar(line))
            }
            line <- self$read_output_lines()
            if (length(line) > 0) {
                line <- paste0(rawToChar(buf), line)
                private$read_char_buf <- raw(0)
                trimws(line, "right")
            }
        },

        read_output = function(n) {
            self$process$read_output(n)
        },

        read_char = function(n) {
            self$check_connection()
            if (length(private$read_char_buf) < n) {
                data <- c(private$read_char_buf, charToRaw(self$read_output(n - length(private$read_char_buf))))
            } else {
                data <- private$read_char_buf
            }
            if (length(data) > n) {
                private$read_char_buf <- data[seq_safe(n + 1, length(data))]
                rawToChar(data[seq_len(n)])
            } else {
                private$read_char_buf <- raw(0)
                rawToChar(data)
            }
        },

        read_error = function() {
            self$check_connection()
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
