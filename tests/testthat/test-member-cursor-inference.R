test_that("Double-bracket completion accepts literal keys at quote boundaries", {
    cursor <- function(lines, col) {
        doc <- Document$new("file:///key.R", language = "r", version = 1L, content = lines)
        member_bracket_cursor(doc, list(row = length(lines) - 1L, col = col))
    }
    opening <- cursor(c("obj[[ # key follows", "  'name']]"), 2L)
    expect_identical(opening$token, "")
    expect_identical(opening$start, 2L)
    expect_identical(opening$operator_row, 0L)
    expect_identical(cursor('obj[["na\\x6de"]]', 13L)$token, "name")
    expect_null(cursor(c("obj[[prefix +", "'name']]"), 4L))
    expect_null(cursor('obj[[ +"name"]]', 11L))
    expect_null(cursor('obj[[r"(name', 12L))
    expect_null(cursor(c("obj[[", rep("", 129L), "'name']]"), 4L))
})

test_that("Uncertain receivers and bounded recovery do not offer literal-key edits", {
    for (text in c('unknown[["field"]]', 'number <- 1L; number[["field"]]')) {
        f <- provider_fixture(text)
        p <- list(row = 0L, col = as.integer(regexpr("field", text, fixed = TRUE)))
        expect_identical(member_completion(f$uri, f$workspace, f$document, p, FALSE, 100L), list())
    }
    # Skip unrelated giant or already-rewritten lines when recovering a receiver.
    for (name in c(strrep("x", 65536L), ".__languageserver_member_cursor__")) {
        text <- paste0(name, "$")
        doc <- Document$new("file:///limit.R", language = "r", version = 1L, content = text)
        point <- list(row = 0L, col = nchar(text))
        expect_null(member_recover(doc, point, member_cursor(doc, point), list()))
    }
})

test_that("Recovering source references alone preserves a nested default's lexical body", {
    lines <- c("f <- function(arg = function(x) {", "    local <- list(target = 1L)",
        "    local$target", "}) NULL")
    doc <- Document$new("file:///default.R", language = "r", version = 1L, content = lines)
    parsed <- parse(text = lines, keep.source = TRUE)
    # This request has the source expression and ranges but no precomputed function-part table.
    data <- list(items = list(list(expr = parsed[[1L]], start = c(0L, 0L), end = c(3L, 7L))))
    point <- list(row = 2L, col = 12L)
    cursor <- member_cursor(doc, point)
    result <- member_recover_scope(doc, point, cursor, data)
    receiver <- member_cursor_value(result$parsed, result$sentinel, member_generic_index(""), list(),
        member_request_budget(), accessor = cursor$accessor, path = result$path)
    expect_identical(names(receiver$fields), "target")
    expect_identical(receiver$fields$target$literal, 1L)
})

test_that("Cursor inference resolves requested list members and rejects unsupported slots", {
    resolve <- function(expr, name, accessor) {
        member_cursor_value(expr, "cursor", member_generic_index(""), list(), member_request_budget(),
            name = name, accessor = accessor)
    }
    expect_identical(resolve(expression(list(field = 42L)$cursor), "field", "$")$literal, 42L)
    method <- resolve(expression(list(run = function(input) input)$cursor), "run", "$")
    expect_identical(names(method$function_expr[[2L]]), "input")
    expect_identical(resolve(expression(obj@cursor), "field", "@")$reason, "unknown_s4_slot")
    tip <- member_completion_tip(member_value(classes = c("specific", "parent", "family", "general")))
    expect_identical(tip$summary, "specific | parent | family | ...")
})

test_that("Local aliases detach after rebinding and uncertain writes break propagation", {
    f <- provider_fixture(c(
        "box <- new.env()", "alias <- box", "alias$shared <- 2L", "alias <- new.env()",
        "alias$separate <- 3L", "box$shared", "other <- box", "if (cond) other <- new.env()",
        "other$late <- 4L", "box$shared"))
    receiver <- function(row) {
        point <- list(row = row, col = 6L)
        cursor <- member_cursor(f$document, point)
        member_resolve_cursor(f$uri, f$workspace, f$document, point, cursor)$value
    }
    expect_identical(names(receiver(5L)$fields), "shared")
    expect_identical(receiver(5L)$fields$shared$literal, 2L)
    expect_identical(receiver(9L)$reason, "unknown_mutation")
})

test_that("Document binding recursion and cycles stop without obscuring other bindings", {
    content <- c("one <- list(field = 1L)", "two <- one", "three <- two", "three$field")
    f <- provider_fixture(content)
    index <- member_context_index(f$workspace, f$uri, f$document, c(3L, 0L))
    budget <- member_request_budget()
    budget$depth_limit <- 0L
    expect_identical(member_resolve_document("one", index, list(), budget, 1L, character())$reason, "binding_recursion")
    end <- index$document_bindings$one[[1L]]$end
    trail <- paste(c("binding", "one", end), collapse = ":")
    expect_identical(member_resolve_document("one", index, list(), budget, 0L, trail)$reason, "binding_cycle")
    expect_identical(member_resolve_document("two", index, list(), member_request_budget(), 0L, character())$fields$field$literal,
        1L)
})

test_that("Quoted document symbols retain class descriptions on ordinary completion items", {
    f <- provider_fixture(c("Thing <- R6::R6Class('Thing')", "`odd name` <- Thing$new()", "`odd name`"))
    p <- list(row = 2L, col = 8L)
    token <- list(accessor = "", range = list(start = list(col = 0L), end = list(col = 10L)))
    items <- list(list(label = "`odd name`", documentation = list(value = "User documentation")),
        list(label = "`odd name`", data = list(type = "parameter")))
    enriched <- member_class_completion_info(f$uri, f$workspace, f$document, p, token, items)
    expect_match(enriched[[1L]]$documentation$value, "R6 instance of `Thing`", fixed = TRUE)
    expect_match(enriched[[1L]]$documentation$value, "User documentation", fixed = TRUE)
    expect_identical(enriched[[2L]], items[[2L]])

    budget <- member_request_budget()
    budget$exhausted <- TRUE
    stopped <- list(value = member_value(type = "environment"), bindings = list(), index = list(), budget = budget)
    expect_identical(member_class_completion_info(f$uri, f$workspace, f$document, p, token, items, stopped), items)
})

test_that("Literate recovery uses only the current cell for a complete class definition", {
    code <- c("Outside prose", "```{r}", 'C <- R6::R6Class("C", public = list(',
        "  run = function() self$member,", "  member = 1L", "))", "```",
        "```{r}", "unrelated <- function() stop('next cell')", "```")
    doc <- Document$new("file:///class.Rmd", language = "rmd", version = 1L, content = code)
    p <- list(row = 3L, col = 26L)
    cursor <- member_cursor(doc, p)
    result <- member_recover(doc, p, cursor, list())
    expect_false(is.null(result$context))
    expect_true("R6Class" %in% all.names(result$context))
    expect_false("unrelated" %in% all.names(result$context))
    expect_identical(result$start[[1L]], 2L)
})

test_that("Document import context changes only after a library call", {
    lines <- c("first <- 1L", "library(fixture)", "last <- first")
    f <- provider_fixture(lines)
    data <- f$document$parse_data$member_data
    data$imports[[1L]]$cache_prefix <- NULL
    f$document$parse_data$member_data <- data
    snapshot <- member_generic_index("")
    snapshot$roots <- list(api = member_literal(42L))
    f$workspace$member_metadata <- collections::dict()
    f$workspace$member_metadata$set("fixture", snapshot)
    before <- member_context_index(f$workspace, f$uri, f$document, c(0L, 0L))
    after <- member_context_index(f$workspace, f$uri, f$document, c(2L, 0L))
    expect_null(before$attached_roots$api)
    expect_identical(after$attached_roots$api$literal, 42L)
    expect_false(identical(before$document_context_key, after$document_context_key))
})
