call_hierarchy_test_containers <- function(calls, definitions) {
    .Call("call_hierarchy_containers_c", calls, definitions,
        PACKAGE = "languageserver")
}

call_hierarchy_test_range <- function(coords) {
    range(position(coords[[1L]], coords[[2L]]),
        position(coords[[3L]], coords[[4L]]))
}

test_that("native call containment preserves overlapping ranges and span ties", {
    definitions <- matrix(as.integer(c(
        0, 0, 30, 0,
        2, 1, 8, 9,
        2, 1, 8, 9,
        5, 1, 10, 9,
        5, 2, 5, 20,
        5, 2, 5, 20,
        4, 1000001, 5, 1,
        11, 0, 11, 2000000
    )), ncol = 4L, byrow = TRUE)
    calls <- matrix(as.integer(c(
        5, 2, 5, 9,
        3, 2, 9, 3,
        30, 1, 30, 2,
        2, 1, 8, 9,
        11, 1500000, 11, 1500001,
        5, 0, 5, 1,
        4, 2, 5, 19,
        0, 0, 30, 0
    )), ncol = 4L, byrow = TRUE)
    expect_identical(call_hierarchy_test_containers(calls, definitions),
        c(5L, 1L, 0L, 2L, 8L, 7L, 2L, 1L))
    expect_identical(call_hierarchy_test_containers(calls, definitions[FALSE, ]),
        rep.int(0L, nrow(calls)))
    expect_identical(call_hierarchy_test_containers(calls[FALSE, ], definitions), integer())
    expect_error(call_hierarchy_test_containers(calls[, -1L], definitions),
        "invalid call hierarchy ranges")
    invalid <- calls
    invalid[[1L]] <- NA_integer_
    expect_error(call_hierarchy_test_containers(invalid, definitions),
        "invalid call hierarchy positions")
})

test_that("native call containment agrees with exhaustive range comparisons", {
    withr::local_seed(722L)
    make_ranges <- function(n) {
        start <- sample.int(2000L, n, replace = TRUE)
        end <- start + sample.int(1000L, n, replace = TRUE)
        cbind(start %/% 100L, start %% 100L, end %/% 100L, end %% 100L)
    }
    for (iteration in seq_len(20L)) {
        calls <- make_ranges(40L)
        definitions <- make_ranges(80L)
        expected <- apply(calls, 1L, function(call) {
            contains <- apply(definitions, 1L, function(definition) {
                item_range <- call_hierarchy_test_range(definition)
                indexed_position_in_range(call[[1L]], call[[2L]], item_range) &&
                    indexed_position_in_range(call[[3L]], call[[4L]], item_range)
            })
            matches <- which(contains)
            if (!length(matches)) return(0L)
            spans <- (definitions[matches, 3L] - definitions[matches, 1L]) *
                1000000 + definitions[matches, 4L] - definitions[matches, 2L]
            matches[[which.min(spans)]]
        })
        expect_identical(call_hierarchy_test_containers(calls, definitions), expected)
    }
})

test_that("outgoing call filtering preserves inclusive endpoint semantics", {
    index <- list(
        token = c(rep("SYMBOL_FUNCTION_CALL", 6L), "SYMBOL"),
        line = c(0L, 1L, 1L, 2L, 3L, 1L, 1L),
        col = c(10L, 2L, 3L, 1L, 0L, 3L, 3L),
        end_line = c(1L, 1L, 1L, 2L, 3L, 2L, 1L),
        end_col = c(3L, 3L, 4L, 6L, 1L, 7L, 4L)
    )
    item_range <- range(position(1L, 3L), position(2L, 6L))
    expect_identical(indexed_calls_in_range(index, item_range), c(3L, 4L))
    expected <- which(index$token == "SYMBOL_FUNCTION_CALL" &
        vapply(seq_along(index$token), function(i) {
            indexed_position_in_range(index$line[[i]], index$col[[i]], item_range) &&
                indexed_position_in_range(
                    index$end_line[[i]], index$end_col[[i]], item_range)
        }, logical(1L)))
    expect_identical(indexed_calls_in_range(index, item_range), expected)
})

test_that("batched incoming calls retain recursion filtering and top-level groups", {
    uri <- "file:///batched-call-hierarchy.R"
    content <- c(
        "target <- function() target()",
        "caller <- function() { target(); target() }",
        "another <- function() target()",
        "target(); target()"
    )
    document <- Document$new(uri, version = 1L, content = content)
    parsed <- parse_document(uri, content)
    parsed$version <- 1L
    workspace <- Workspace$new(NULL)
    workspace$documents$set(uri, document)
    workspace$update_parse_data(uri, parsed)
    target <- parsed$definitions$target
    item <- list(name = "target", uri = uri, range = target$range,
        data = list(definitionKey = "global:target",
            definition = list(uri = uri, range = target$range)))
    result <- indexed_incoming_calls(workspace, item)
    expect_length(result, 3L)
    names <- vapply(result, function(entry) entry$from$name, character(1L))
    caller <- result[[match("caller", names)]]
    expect_length(caller$fromRanges, 2L)
    expect_equal(vapply(caller$fromRanges, function(x) x$start$character, integer(1L)),
        c(23L, 33L))
    top <- result[[match("batched-call-hierarchy.R", names)]]
    expect_length(top$fromRanges, 2L)
    expect_equal(top$from$kind, SymbolKind$File)
    expect_equal(top$from$selectionRange, top$fromRanges[[1L]])
    outgoing_item <- list(name = "caller", uri = uri,
        range = parsed$definitions$caller$range,
        data = list(definition = list(uri = uri, range = parsed$definitions$caller$range)))
    outgoing <- indexed_outgoing_calls(workspace, outgoing_item)
    expect_length(outgoing, 1L)
    expect_equal(outgoing[[1L]]$to$name, "target")
    expect_equal(outgoing[[1L]]$fromRanges, caller$fromRanges)
})
