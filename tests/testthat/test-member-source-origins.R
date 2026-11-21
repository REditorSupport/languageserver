test_that("Member origins survive nested loop iterators, bodies and function defaults", {
    source <- c("f <- function(x = list(option = 1)) {",
        "    for (item in list(input = x)) {",
        "        item$updated <- list(result = x)",
        "    }",
        "}")
    uri <- "file:///coordinates.R"
    parsed <- member_parse_recovered(source, uri, row = 10L, col = 3L)
    fn <- parsed[[1L]][[3L]]
    loop <- fn[[3L]][[2L]]
    write <- loop[[4L]][[2L]]
    origin <- function(node) attr(node, "member_origins", exact = TRUE)[["1"]]
    expect_identical(origin(fn[[2L]]$x), list(uri = uri, range = c(10L, 26L, 32L)))
    expect_identical(origin(loop[[3L]]), list(uri = uri, range = c(11L, 22L, 27L)))
    expect_identical(attr(write[[2L]], "member_origin", exact = TRUE),
        list(uri = uri, range = c(12L, 13L, 20L)))
    expect_identical(origin(write[[3L]]), list(uri = uri, range = c(12L, 29L, 35L)))
})

test_that("Recovered coordinates skip inserted names and shift surviving fields back", {
    # Recovery replaced the two original characters with the five characters 'added'.
    expr <- member_parse_recovered("x <- list(added = 0, keep = 1)", "file:///edit.R", 0L,
        edit = list(row = 0L, start = 10L, end = 12L, inserted = 5L))
    tags <- attr(expr[[1L]][[3L]], "member_origins", exact = TRUE)
    expect_null(tags[["1"]])
    expect_identical(tags[["2"]]$range, c(0L, 18L, 22L))
})

test_that("Pipe placeholders preserve the names from the original receiver and call", {
    skip_if(getRversion() < "4.2.0")
    line <- "box <- list(label = 1) |> list(flag = TRUE, data = _)"
    fixture <- provider_fixture(c(line, "box$data$label", "box$flag"))
    lookup <- function(row, name) {
        member_definition_location(fixture$uri, fixture$workspace, fixture$document,
            list(row = row, col = regexpr(name, fixture$document$content[[row + 1L]], fixed = TRUE)[[1L]]))$result
    }
    for (name in c("data", "label", "flag")) {
        col <- regexpr(name, line, fixed = TRUE)[[1L]] - 1L
        expect_equal(lookup(if (name == "flag") 2L else 1L, name),
            list(uri = fixture$uri, range = range(position(0L, col), position(0L, col + nchar(name)))))
    }
})

test_that("Member navigation ignores unavailable or mismatched installed source references", {
    root <- normalizePath(withr::local_tempdir(), winslash = "/")
    withr::local_dir(root)
    writeLines("run <- function(x) x", "source.R")
    ns <- new.env(parent = baseenv())
    sys.source("source.R", envir = ns, keep.source = TRUE)
    original <- member_function_origin(ns$run)
    expect_identical(original$uri, path_to_uri(file.path(root, "source.R")))

    # A short reference, a stale line number, or a column between tokens cannot identify a function.
    ref <- attr(ns$run, "srcref", exact = TRUE)
    with_ref <- function(value) member_function_origin(structure(ns$run, srcref = value))
    expect_null(with_ref(structure(1:4, class = "srcref")))
    expect_null(with_ref(structure(as.integer(ref), class = "srcref")))
    moved <- ref
    moved[[7L]] <- moved[[8L]] <- 2L
    expect_null(with_ref(moved))
    moved <- ref
    moved[[5L]] <- moved[[5L]] + 1L
    expect_null(with_ref(moved))

    # Some package images retain an in-memory or relative srcfile after the file disappears.
    file <- attr(ref, "srcfile")
    file$wd <- "."
    expect_null(with_ref(ref))
    file$wd <- root
    file$lines <- "run <- 42L"
    expect_null(with_ref(ref))
    file$isFile <- FALSE
    expect_null(with_ref(ref))
    source <- file.path(root, "source.R")
    expect_true(file.rename(source, file.path(root, "moved.R")))
    expect_null(member_file_definition(original, list(documents = collections::dict())))
})

test_that("R6 initialization keeps writes made before explicit and conditional returns", {
    index <- member_generic_index("")
    generator <- member_infer(quote(R6::R6Class("Example", public = list(data = NULL,
        initialize = function(early) {
            self$data <- list(initial = 1L)
            if (early) return()
            self$data <- list(final = 2L)
            invisible(self)
        }
    ))), index)
    scope <- list(Example = generator)
    expect_identical(member_infer(quote(Example$new(TRUE)$data$initial), index, scope)$literal, 1L)
    expect_identical(member_infer(quote(Example$new(FALSE)$data$final), index, scope)$literal, 2L)
    expect_null(member_infer(quote(Example$new(early)$data$initial), index, scope)$type)

    fixture <- provider_fixture(c("C <- R6::R6Class('C', active = list(",
            "    value = function() stop('getter must stay inert')))", "C$new()$value"))
    result <- member_definition_location(fixture$uri, fixture$workspace, fixture$document, list(row = 2L, col = 9L))$result
    expect_equal(result, list(uri = fixture$uri, range = range(position(1L, 4L), position(1L, 9L))))
})
