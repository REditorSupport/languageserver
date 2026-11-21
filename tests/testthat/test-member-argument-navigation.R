test_that("Method discovery distinguishes constructor fields from ordinary callback arguments", {
    f <- provider_fixture(c(
        "api <- base::list(run = function(x) x)",
        "frame <- base::data.frame(column = function(x) x)",
        "external <- other::list(hidden = function(x) x)",
        "callback(arg = function(x) x)",
        "list <- function(...) NULL",
        "list(masked = function(x) x)",
        "alias <- rlang::list2(collect = function(x) x)",
        "api$direct <- function(y) y"
    ))
    methods <- member_navigation_methods(f$workspace, f$document)
    names <- vapply(methods, `[[`, "", "name")
    expect_setequal(names, c("direct", "run", "column", "collect"))
    run <- methods[[match("run", names)]]
    expect_identical(member_navigation_method(f$workspace, run$memberDefinition), run)
    expect_identical(run$range, range(position(0L, 18L), position(0L, 37L)))
    expect_null(member_navigation_method(f$workspace, location(f$uri, range(position(0L, 0L), position(0L, 3L)))))
    f <- provider_fixture("list(hidden = function(x) x)")
    f$workspace$get_namespace <- function(...) list(get_definition = function(...) list(uri = "somewhere.R"))
    expect_identical(member_navigation_methods(f$workspace, f$document), list())
})

test_that("Rightward writes retain the declaration token and the parenthesized function", {
    f <- provider_fixture(c("api <- list()", "(function(input) input) -> api$run",
            "api[['copy']] <- (function(source) source)", "api$run(inp = 1)", "api$copy(sou = 2)"))
    doc <- f$document
    index <- member_navigation_syntax(doc$parse_data)
    for (name in c("run", "copy")) {
        declaration <- index[index$kind == "write" & index$name == name, ]
        expect_equal(nrow(declaration), 1L)
        def <- location(f$uri, navigation_range(doc, doc$parse_data$syntax_data, declaration$row))
        fn <- member_navigation_function(doc, def)
        expect_gt(fn, 0L)
        kids <- syntax_children(doc$parse_data$syntax_data, fn)
        names <- doc$parse_data$syntax_data$text[kids]
        expect_true(if (name == "run") "input" %in% names else "source" %in% names)
    }
    for (tag in c("inp", "sou")) {
        rec <- index[index$name == tag, ]
        def <- member_navigation_formal(f$workspace, doc, rec)
        expect_identical(def$uri, f$uri)
        expect_identical(def$range$start$line, if (tag == "inp") 1L else 2L)
    }
})

test_that("Partial tags navigate only to a unique formal before dots", {
    f <- provider_fixture(c("fn <- function(alpha, alphabet, ..., after) NULL",
            "fn(al = 1)", "fn(alpha = 1, al = 2)", "fn(aft = 1)", "fn(after = 1)", "fn(... = 1)",
            "absent(al = 1)"))
    doc <- f$document
    index <- member_navigation_syntax(doc$parse_data)
    at <- function(row, name) {
        rec <- index[index$kind == "tag" & index$name == name & doc$parse_data$syntax_data$line1[index$row] == row, ]
        member_navigation_formal(f$workspace, doc, rec)
    }
    expect_null(at(2L, "al"))
    expect_equal(at(3L, "al")$range, range(position(0L, 22L), position(0L, 30L)))
    expect_null(at(4L, "aft"))
    expect_equal(at(5L, "after")$range, range(position(0L, 37L), position(0L, 42L)))
    expect_null(at(6L, "..."))
    expect_null(at(7L, "al"))
    target <- at(3L, "alpha")
    expect_true(member_navigation_formal_conflict(f$workspace, c("file:///missing.R", f$uri), target,
            "alpha", "alpine"))
    expect_false(member_navigation_formal_conflict(f$workspace, f$uri, target, "alpha", "first"))
})

test_that("Navigation skips stale revisions and missing source locations", {
    f <- provider_fixture(c("api <- list(field = 1L)", "api$field"))
    doc <- f$document
    pos <- list(row = 1L, col = 6L)
    target <- member_navigation_at(f$workspace, doc, pos)$definition
    expect_identical(target$range, range(position(0L, 12L), position(0L, 17L)))
    expect_identical(member_navigation_origin(f$workspace, list(uri = f$uri, range = c(0L, 12L, 17L))),
        location(f$uri, target$range))
    expect_null(member_navigation_origin(f$workspace, list(uri = f$uri, range = c(12L, 17L))))
    expect_null(member_navigation_origin(f$workspace, list(uri = "file:///missing.R", range = target$range)))
    doc$parse_data$version <- 0L
    expect_null(member_navigation_at(f$workspace, doc, pos))
    expect_length(member_navigation_occurrences(f$workspace, target, "field", c("file:///missing.R", f$uri)), 0L)
})

test_that("Rename detects collisions for unannotated field locations and long partial names", {
    f <- provider_fixture(c("api <- list(option = 1L, other = 2L)", "api$option"))
    index <- member_navigation_syntax(f$document$parse_data)
    long_name <- paste0("option", strrep("x", 150L))
    expect_identical(member_navigation_prefix_indices(index, long_name), which(index$name == "option"))
    expect_identical(member_navigation_prefix_indices(index, ""), integer())
    for (i in which(index$name == "option")) {
        ref <- location(f$uri, navigation_range(f$document, f$document$parse_data$syntax_data, index$row[i]))
        conflict <- member_navigation_rename_conflict(f$workspace, f$uri, list(ref), "option", "other")
        expect_identical(conflict, "Another member of this object already has the new name")
        expect_null(member_navigation_rename_conflict(f$workspace, f$uri, list(ref), "option", long_name))
    }
})
