get_free_port <- function(port = 6000) {
    while (TRUE) {
        p <- callr::r_bg(
            function(port) {
                inherits(try(
                    socketConnection(port = port, server = TRUE), silent = TRUE),
                "connection")
            },
            list(port = port))
        q <- callr::r_bg(
            function(port) {
                Sys.sleep(0.1)
                inherits(try(
                    socketConnection(port = port, server = FALSE), silent = TRUE),
                "connection")
            },
            list(port = port))
        q$wait()
        p$wait()
        if (q$get_result() && p$get_result()) {
            return(port)
        } else {
            port <- port + 1
        }
    }
}
