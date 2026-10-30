binding_cache_fixture <- function(possible = TRUE) {
    index <- member_generic_index("step <- function(label = NULL) self")
    index$package <- "bindingfixture"
    index$exports <- "seed"
    index$members <- list(box = c(step = "step"))
    seed <- member_value(type = "box", member_package = index$package)
    index$roots$seed <- if (possible) member_value(reason = "open_dispatch", possible_returns = list(seed)) else seed
    metadata <- MemberMetadataCache$new(1024^2)
    metadata$set(index$package, member_index_freeze(index))
    workspace <- list(member_metadata = metadata)
    content <- c("library(bindingfixture)", "object <- seed",
        rep("object <- object$step()", 18L), "object$step()")
    document <- Document$new("file:///binding-reuse.R", language = "r", version = 0L)
    update <- function(lines) {
        document$set_content(document$version + 1L, lines)
        data <- parse_document(document$uri, lines)
        data$version <- document$version
        document$update_parse_data(data)
    }
    update(content)
    resolve <- function(row = document$nline - 1L, col = 7L) {
        point <- list(row = row, col = col)
        member_resolve_cursor(document$uri, workspace, document, point, member_cursor(document, point))
    }
    list(index = index, metadata = metadata, workspace = workspace,
        document = document, content = content, update = update, resolve = resolve)
}

test_that("Long binding summaries survive member and argument edits across providers", {
    for (possible in c(FALSE, TRUE)) {
        fixture <- binding_cache_fixture(possible)
        document <- fixture$document
        cold <- fixture$resolve()
        expect_false(cold$budget$exhausted)
        expect_gt(20000L - cold$budget$remaining, 50L)
        expect_gt(document$member_binding_cache$size(), 0L)
        prefix <- head(fixture$content, -1L)
        for (line in c("object$s", "object$st", "object$step()", "object$step(label = 2)",
                "object$step(\n  label = \"edited\"\n)")) {
            lines <- c(prefix, strsplit(line, "\n", fixed = TRUE)[[1L]])
            fixture$update(lines)
            row <- length(prefix)
            result <- fixture$resolve(row)
            expect_lt(20000L - result$budget$remaining, 10L)
            expect_identical(length(result$value$possible_returns), if (possible) 1L else 0L)
            items <- member_completion(document$uri, fixture$workspace, document, list(row = row, col = 7L), TRUE, 200L)
            expect_identical(vapply(items, `[[`, character(1L), "label"), "step")
            if (startsWith(line, "object$step(")) {
                signature <- signature_reply(1L, document$uri, fixture$workspace, document,
                    list(row = row, col = 12L))$result
                expect_identical(signature$signatures[[1L]]$label, "step(label = NULL)")
                hover <- hover_reply(1L, document$uri, fixture$workspace, document,
                    list(row = row, col = 9L))$result
                expect_match(paste(hover$contents, collapse = "\n"), "step(label = NULL)", fixed = TRUE)
            }
        }
        fixture$update(c(fixture$content, "unrelated <- opaque_source()", "object$step()"))
        result <- fixture$resolve()
        expect_lt(20000L - result$budget$remaining, 10L)
        expect_identical(length(result$value$possible_returns), if (possible) 1L else 0L)
        lines <- fixture$content
        lines[[length(lines) - 1L]] <- 'object <- object$step(label = "new stage")'
        fixture$update(lines)
        result <- fixture$resolve()
        expect_lt(20000L - result$budget$remaining, 20L)
        expect_identical(length(result$value$possible_returns), if (possible) 1L else 0L)
        fixture$update(c(prefix, "object$step(", "  label = "))
        result <- fixture$resolve(length(prefix))
        expect_lt(20000L - result$budget$remaining, 10L)
        signature <- signature_reply(1L, document$uri, fixture$workspace, document,
            list(row = document$nline - 1L, col = 10L))$result
        expect_identical(signature$signatures[[1L]]$label, "step(label = NULL)")
    }
})

test_that("Changed source prefixes and metadata cannot reuse an old binding summary", {
    fixture <- binding_cache_fixture()
    fixture$resolve()
    content <- fixture$content
    content[[2L]] <- "object <- list(other = TRUE)"
    fixture$update(content)
    result <- fixture$resolve()
    expect_length(result$value$possible_returns, 0L)
    expect_gt(20000L - result$budget$remaining, 50L)
    fixture$update(fixture$content)
    expect_length(fixture$resolve()$value$possible_returns, 1L)
    fixture$index$roots$seed <- member_value(type = "list", fields = list(other = member_literal(TRUE)))
    fixture$metadata$set("bindingfixture", member_index_freeze(fixture$index))
    fixture$update(fixture$content)
    result <- fixture$resolve()
    expect_length(result$value$possible_returns, 0L)
    expect_gt(20000L - result$budget$remaining, 50L)
    expect_identical(fixture$document$member_binding_cache$size(), 1L)
})

test_that("Attachment contexts and local bindings remain independent of cached globals", {
    fixture <- binding_cache_fixture()
    masked <- member_generic_index("advance <- function(other = FALSE) self")
    masked$package <- "maskingfixture"
    masked$exports <- "seed"
    masked$members <- list(other = c(step = "advance"))
    masked$roots$seed <- member_value(type = "other", member_package = masked$package)
    fixture$metadata$set(masked$package, member_index_freeze(masked))
    expect_length(fixture$resolve()$value$possible_returns, 1L)
    fixture$update(append(fixture$content, "library(maskingfixture)", after = length(fixture$content) - 1L))
    result <- fixture$resolve()
    expect_identical(result$value$type, "other")
    expect_gt(20000L - result$budget$remaining, 50L)
    fixture$update(c(head(fixture$content, -1L), "check <- function(object) {", "  object$step()", "}"))
    row <- fixture$document$nline - 2L
    result <- fixture$resolve(row, 9L)
    expect_length(result$value$possible_returns, 0L)
    expect_null(result$value$type)
    fixture$update(c(head(fixture$content, -1L), "{", "  object <- list(local = TRUE)", "  object$step()", "}"))
    result <- fixture$resolve(fixture$document$nline - 2L, 9L)
    expect_identical(names(result$value$fields), "local")
})

test_that("Slot and quoted member requests reuse the same unchanged binding", {
    fixture <- binding_cache_fixture()
    seed <- fixture$index$roots$seed$possible_returns[[1L]]
    seed$slot_types <- list(step = "function")
    seed$slots <- list(step = member_value(type = "function", function_key = "step", metadata = "bindingfixture"))
    fixture$index$roots$seed$possible_returns <- list(seed)
    fixture$metadata$set("bindingfixture", member_index_freeze(fixture$index))
    fixture$resolve()
    for (line in c("object@step()", "object$`step`()")) {
        fixture$update(c(head(fixture$content, -1L), line))
        row <- fixture$document$nline - 1L
        col <- if (grepl("`", line, fixed = TRUE)) 10L else 9L
        result <- fixture$resolve(row, col)
        expect_lt(20000L - result$budget$remaining, 10L)
        items <- member_completion(fixture$document$uri, fixture$workspace, fixture$document,
            list(row = row, col = col), TRUE, 200L)
        expect_identical(vapply(items, `[[`, character(1L), "label"), "step")
        opening <- regexpr("(", line, fixed = TRUE)[[1L]]
        signature <- signature_reply(1L, fixture$document$uri, fixture$workspace, fixture$document,
            list(row = row, col = opening))$result
        expect_identical(signature$signatures[[1L]]$label, "step(label = NULL)")
    }
})

test_that("Binding reuse does not capture late-bound source closures or mutable identities", {
    fixture <- binding_cache_fixture(FALSE)
    content <- c("value <- list(first = TRUE)", "factory <- function() value", "factory()$step()")
    fixture$update(content)
    point <- list(row = 2L, col = 10L)
    resolve <- function() {
        member_resolve_cursor(fixture$document$uri, fixture$workspace, fixture$document,
            point, member_cursor(fixture$document, point))
    }
    expect_identical(names(resolve()$value$fields), "first")
    fixture$update(c(content[1:2], "value <- list(second = TRUE)", "factory()$step()"))
    point$row <- 3L
    expect_identical(names(resolve()$value$fields), "second")
    expect_false(member_binding_cacheable(member_value(function_expr = quote(function() value))))
    expect_false(member_binding_cacheable(member_value(type = "environment", r6_class = list(name = "Mutable"))))
    value <- member_value(type = "environment")
    value$r6_identity <- "instance"
    expect_false(member_binding_cacheable(value))
    expect_false(member_binding_cacheable(member_value(type = "list", fields = list(
        deferred = member_local_binding(quote(value), list(value = member_literal(1)))))))
})

test_that("Source fingerprints retain same-line ranges and disable invalid virtual ranges", {
    before <- member_document_index(c("first <- 1; object <- list(α = first)", "object$s"))
    after <- member_document_index(c("first <- 1; object <- list(α = first)", "object$step(label = 1)"))
    expect_identical(before$bindings$object[[1L]]$cache_prefix, after$bindings$object[[1L]]$cache_prefix)
    changed <- member_document_index(c("first <- 2; object <- list(α = first)", "object$s"))
    expect_false(identical(before$bindings$object[[1L]]$cache_prefix, changed$bindings$object[[1L]]$cache_prefix))
    expect_null(member_source_prefix("x <- 1", c(0L, 0L), c(1000L, 0L), ""))
    expect_null(member_source_prefix(c("x <- 1", NA_character_), c(0L, 0L), c(1L, 2L), ""))
})
