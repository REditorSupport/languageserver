completion_signature_fixture <- function() {
    namespace <- PackageNamespace$new("base")
    private <- namespace$.__enclos_env__$private
    storage <- new.env(parent = baseenv())
    storage$probe <- function(argument = 1) NULL
    private$namespace <- storage
    private$membership <- list2env(list(probe = 4L), parent = emptyenv())
    method_env <- environment(private$function_info)
    method_env$get <- method_env$getNamespace <- method_env$loadNamespace <- function(...) {
        stop("Completion must not evaluate or reload namespace bindings")
    }
    list(namespace = namespace, storage = storage, method_env = method_env)
}

test_that("Completion signatures skip active and unforced bindings after catalog preparation", {
    fixture <- completion_signature_fixture()
    expect_match(fixture$namespace$get_completion_signature("probe"), "argument = 1", fixed = TRUE)
    calls <- 0L
    rm("probe", envir = fixture$storage)
    makeActiveBinding("probe", function() {
        calls <<- calls + 1L
        stop("Active binding executed")
    }, fixture$storage)
    expect_null(fixture$namespace$get_completion_signature("probe"))
    expect_identical(calls, 0L)

    rm("probe", envir = fixture$storage)
    delayedAssign("probe", {
        calls <- calls + 1L
        stop("Deferred function executed")
    }, assign.env = fixture$storage)
    expect_null(fixture$namespace$get_completion_signature("probe"))
    expect_identical(member_binding(fixture$storage, "probe")[[1L]], "deferred")
    expect_identical(calls, 0L)
})

test_that("Completion signature formatting is cached and follows stored function changes", {
    fixture <- completion_signature_fixture()
    calls <- 0L
    fixture$method_env$args <- function(fn) {
        calls <<- calls + 1L
        base::args(fn)
    }
    first <- fixture$namespace$get_completion_signature("probe")
    for (i in seq_len(25L)) expect_identical(fixture$namespace$get_completion_signature("probe"), first)
    expect_identical(calls, 1L)
    fixture$storage$probe <- function(changed = 2) NULL
    expect_match(fixture$namespace$get_completion_signature("probe"), "changed = 2", fixed = TRUE)
    expect_identical(calls, 2L)
    withr::local_options(width = 30L)
    expect_match(fixture$namespace$get_completion_signature("probe"), "changed = 2", fixed = TRUE)
    expect_identical(calls, 3L)
})

test_that("Completion signatures use cached membership without namespace reload", {
    fixture <- completion_signature_fixture()
    private <- fixture$namespace$.__enclos_env__$private
    private$membership$probe <- 2L
    expect_null(fixture$namespace$get_completion_signature("probe"))
    expect_match(fixture$namespace$get_completion_signature("probe", exported_only = FALSE), "argument", fixed = TRUE)
    expect_null(fixture$namespace$get_completion_signature("missing", exported_only = FALSE))
    fixture$namespace$package_name <- "unloaded-completion-fixture"
    expect_null(fixture$namespace$get_completion_signature("probe", exported_only = FALSE))
})

test_that("Inline package completion uses the non-evaluating signature accessor", {
    fixture <- completion_signature_fixture()
    calls <- 0L
    rm("probe", envir = fixture$storage)
    makeActiveBinding("probe", function() {
        calls <<- calls + 1L
        stop("Active completion binding executed")
    }, fixture$storage)
    namespaces <- collections::dict()
    namespaces$set("base", fixture$namespace)
    items <- list(list(label = "probe", kind = CompletionItemKind$Function, detail = "{base}",
            data = list(type = "function", package = "base")))
    document <- Document$new("file:///signature-safety.R", "r", 1L, "probe")
    expect_identical(completion_symbol_info(document$uri, list(namespaces = namespaces),
            document, list(row = 0L, col = 5L), NULL, items), items)
    expect_identical(calls, 0L)
})
