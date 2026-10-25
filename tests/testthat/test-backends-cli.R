cli_fixture_request <- function(content = character(), timeout = 5) {
    list(backend = list(id = "fixture"), executable = file.path(R.home("bin"), "Rscript"),
        content = content, timeout = timeout, working_directory = tempdir(), env = NULL)
}

test_that("CLI runner streams exact stdin independently from stdout and stderr", {
    script <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "input <- readBin(file('stdin', 'rb'), 'raw', n = 1e7)",
        "cat(rawToChar(input))",
        "cat(strrep('e', 100000), file = stderr())"
    ), script)
    content <- c(paste0('"\U0001f600"', strrep("x", 100000)), "")
    request <- cli_fixture_request(content)
    result <- backend_cli_run(request, c("--vanilla", script))
    expect_identical(result$status, 0L)
    # R's text-mode stdout uses CRLF on Windows; stdin bytes remain unchanged.
    expect_equal(gsub("\r\n", "\n", result$stdout, fixed = TRUE), paste(content, collapse = "\n"))
    expect_equal(nchar(result$stderr), 100000)
})

test_that("CLI runner uses explicit cwd, environment, arguments and timeout", {
    directory <- withr::local_tempdir()
    script <- withr::local_tempfile(fileext = ".R")
    writeLines("cat(getwd(), Sys.getenv('LANGUAGESERVER_CLI_TEST'), commandArgs(TRUE), sep = '\\n')", script)
    request <- cli_fixture_request()
    request$working_directory <- directory
    request$env <- c(LANGUAGESERVER_CLI_TEST = "value")
    result <- backend_cli_run(request, c("--vanilla", script, "argument with spaces"))
    expect_equal(strsplit(gsub("\r\n", "\n", result$stdout, fixed = TRUE), "\n", fixed = TRUE)[[1L]],
        c(normalizePath(directory, winslash = "/"), "value", "argument with spaces"))
    request$timeout <- 0.2
    expect_error(backend_cli_run(request, c("--vanilla", "-e", "Sys.sleep(10)")), "timed out")
})

test_that("CLI descendants stop on cancellation, supersession, shutdown, worker death and timeout", {
    skip_on_cran()
    skip_if(identical(Sys.getenv("R_COVR"), "true"))
    skip_if_not_installed("ps")
    script <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "child <- processx::process$new(file.path(R.home('bin'), 'Rscript'),",
        "    c('--vanilla', '-e', \"writeLines('ready', commandArgs(TRUE)[[1L]]); Sys.sleep(30)\",",
        "        paste0(commandArgs(TRUE)[[1L]], '.child')), cleanup = FALSE)",
        "while (!file.exists(paste0(commandArgs(TRUE)[[1L]], '.child'))) Sys.sleep(0.01)",
        "saveRDS(c(parent = Sys.getpid(), child = child$get_pid()), commandArgs(TRUE)[[1L]])",
        "Sys.sleep(30)"
    ), script)
    for (mode in c("cancel", "supersede", "stop", "worker_death", "timeout")) {
        marker <- withr::local_tempfile()
        withr::defer(unlink(paste0(marker, ".child")))
        request <- cli_fixture_request(timeout = if (mode == "timeout") 1.5 else 30)
        manager <- TaskManager$new(paste("CLI", mode), use_session = TRUE,
            min_idle_sessions = 0, max_running_tasks = 1)
        withr::defer(manager$stop())
        manager$add_task("cli", create_task(package_call(backend_cli_run),
                list(request = request, arguments = c("--vanilla", script, marker)),
                track_descendants = TRUE))
        deadline <- Sys.time() + 10
        while (!file.exists(marker) && Sys.time() < deadline) {
            manager$run_tasks()
            manager$check_tasks()
            Sys.sleep(0.01)
        }
        expect_true(file.exists(marker), info = mode)
        if (!file.exists(marker)) next
        handles <- lapply(readRDS(marker), ps::ps_handle)
        withr::defer(for (handle in handles) try(ps::ps_kill(handle), silent = TRUE))
        # The parent has observed a fully started process tree before failure.
        Sys.sleep(0.05)
        manager$check_tasks()
        switch(mode,
            cancel = manager$cancel("cli"),
            supersede = manager$add_task("cli", create_task(function() list(), list())),
            stop = manager$stop(),
            worker_death = manager$.__enclos_env__$private$running_tasks$get("cli")$
                .__enclos_env__$private$session$kill(grace = 0, close_connections = FALSE),
            timeout = NULL)
        alive <- function(handle) {
            tryCatch(ps::ps_is_running(handle) && !ps::ps_status(handle) %in% c("zombie", "dead"),
                no_such_process = function(e) FALSE)
        }
        deadline <- Sys.time() + 5
        while (any(vapply(handles, alive, logical(1L))) && Sys.time() < deadline) {
            manager$check_tasks()
            Sys.sleep(0.02)
        }
        expect_false(any(vapply(handles, alive, logical(1L))), info = mode)
        manager$stop()
    }
})
