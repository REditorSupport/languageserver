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

test_that("TCP input distinguishes an idle socket, a complete message and EOF", {
    skip_on_cran()
    listener <- NULL
    for (port in sample(20000:60000, 10L)) {
        listener <- tryCatch(serverSocket(port), error = function(e) NULL)
        if (!is.null(listener)) break
    }
    skip_if(is.null(listener), "TCP listeners unavailable")
    withr::defer(close(listener))
    server <- LanguageServer$new("127.0.0.1", port)
    withr::defer(server$stop_task_managers())
    peer <- socketAccept(listener, blocking = TRUE, open = "r+b", timeout = 5)
    withr::defer(if (!is.null(peer)) close(peer))

    expect_false(server$wait_input(0))
    expect_identical(server$read_line(), character())
    expect_identical(server$read_raw(32L), raw())
    writeBin(charToRaw("Content-Length: 2\r\n"), peer)
    expect_true(server$wait_input(5))
    expect_identical(server$read_line(), "Content-Length: 2")
    writeBin(charToRaw("Content-Length: 2\r\n\r\n{"), peer)
    expect_true(server$wait_input(5))
    expect_null(server$fetch())
    writeBin(charToRaw("}"), peer)
    expect_identical(server$fetch(blocking = TRUE, timeout = 5), "{}")
    close(peer)
    peer <- NULL
    expect_true(server$wait_input(5))
    expect_identical(server$read_raw(1L), structure(raw(), eof = TRUE))
})
