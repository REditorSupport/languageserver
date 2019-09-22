# for testing purpose

LanguageClient <- R6::R6Class("LanguageClient",
    inherit = LanguageBase,
    public = list(
        process = NULL,
        rootUri = NULL,
        ClientCapabilities = NULL,
        ServerCapabilities = NULL,

        initialize = function(cmd, args) {
            self$process <- processx::process$new(cmd, args,
                stdin = "|", stdout = "|", stderr = "|", supervise = TRUE)
            self$register_handlers()
            self$request_callbacks <- collections::Dict$new()
        },

        finalize = function() {
            self$stop()
        },

        check_connection = function() {
            if (!self$process$is_alive())
                stop("Server is dead.")
        },

        write_text = function(text) {
            self$process$write_input(text)
        },

        read_one_output_line = function() {
            line <- self$process$read_output_lines(1)
            trimws(line, "right")
        },

        read_header = function() {
            if (!self$process$is_alive() || self$process$poll_io(1)[1] != "ready") return(NULL)
            header <- self$read_one_output_line()
            if (length(header) == 0 || !nzchar(header)) return(NULL)
            logger$info("received: ", header)
            matches <- stringr::str_match(header, "Content-Length: ([0-9]+)")
            if (is.na(matches[2]))
                stop(paste0("Unexpected input: ", header))
            as.integer(matches[2])
        },

        read_content = function(nbytes) {
            empty_line <- self$read_one_output_line()
            while (length(empty_line) == 0) {
                empty_line <- self$read_one_output_line()
                Sys.sleep(0.01)
            }
            if (nzchar(empty_line)) {
                stop("Unexpected non-empty line")
            }
            data <- ""
            while (nbytes > 0) {
                newdata <- self$process$read_output(nbytes)
                if (length(newdata) > 0) {
                    nbytes <- nbytes - nchar(newdata, type = "bytes")
                    data <- paste0(data, newdata)
                }
                Sys.sleep(0.01)
            }
            data
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

        stop = function() {
            self$process$kill()
            self$request_callbacks$clear()
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
