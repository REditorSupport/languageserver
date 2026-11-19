member_write_receiver <- function(lines, receiver = "box", local = FALSE, incomplete = FALSE) {
    query <- paste0(receiver, if (incomplete) "$fun(" else "$fun()")
    content <- c(lines, query)
    if (local) content <- c("probe <- function() {", content, "}")
    fixture <- provider_fixture(content)
    point <- list(row = length(lines) + as.integer(local), col = nchar(receiver) + 1L)
    member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document, point,
        member_cursor(fixture$document, point))$value
}

test_that("Direct dollar writes resolve in source order in accepted and recovered scopes", {
    for (local in c(FALSE, TRUE)) {
        for (ctor in c("list()", "new.env()")) {
            lines <- c(paste("box <-", ctor), "box$keep <- 1L", "box$fun <- function(x, y) {",
                "  x + y", "}")
            for (broken in c(FALSE, TRUE)) {
                value <- member_write_receiver(lines, local = local, incomplete = broken)
                expect_identical(names(value$fields$fun$function_expr[[2L]]), c("x", "y"))
                expect_identical(value$fields$keep$literal, 1L)
            }
            replacement <- c(lines, "f <- function(new = 1) new", "box$fun <- f", "f <- 42")
            value <- member_write_receiver(replacement, local = local)
            expect_identical(names(value$fields$fun$function_expr[[2L]]), "new")
            value <- member_write_receiver(c(lines, "box$fun <- NULL"), local = local)
            expect_identical(value$fields$keep$literal, 1L)
            if (ctor == "list()") expect_false("fun" %in% names(value$fields)) else
                expect_identical(value$fields$fun$type, "NULL")
            expect_null(member_write_receiver(c(lines, "box <- list()"), local = local)$fields$fun)
        }
    }
})

test_that("Straight-line environment aliases share writes, list aliases retain their snapshots", {
    for (local in c(FALSE, TRUE)) {
        for (ctor in c("new.env()", "list()")) {
            lines <- c(paste("box <-", ctor), "box$fun <- function(old) old",
                "first <- box", "alias <- first", "alias$fun <- function(new) new")
            expected <- if (ctor == "new.env()") "new" else "old"
            value <- member_write_receiver(lines, local = local)
            expect_identical(names(value$fields$fun$function_expr[[2L]]), expected)
            detached <- c(lines, "alias <- list()", "alias$fun <- function(wrong) wrong")
            value <- member_write_receiver(detached, local = local)
            expect_identical(names(value$fields$fun$function_expr[[2L]]), expected)
        }
    }
})

test_that("List named writes update positional elements and preserve proven siblings", {
    lines <- c("box <- list(3, fun = 'earlier', fun = 8)", "box$fun <- function(x) x")
    value <- member_write_receiver(lines)
    expect_identical(value$fields$fun, value$elements[[2L]])
    expect_identical(value$elements[[1L]]$literal, 3)
    value <- member_write_receiver(c(lines, "box$fun <- NULL"))
    expect_identical(value$fields$fun$literal, 8)
    expect_identical(value$elements[[2L]]$literal, 8)
    value <- member_write_receiver(c("box <- list(keep = 1, fun = function() 0)", "box$fun <- opaque()"))
    expect_identical(names(value$fields), "keep")
    expect_null(member_elements(value))
})

test_that("Unproven writes and conditional replacements do not expose stale functions", {
    for (local in c(FALSE, TRUE)) {
        for (write in c("box[[key]] <- 1", "if (flag) box$fun <- 1", "names(box) <- 'other'")) {
            value <- member_write_receiver(c("box <- list(fun = function(x) x)", write), local = local)
            expect_null(value$fields$fun)
        }
        for (ctor in c("unknown()", "structure(list(), class = 'custom')")) {
            value <- member_write_receiver(c(paste("box <-", ctor), "box$fun <- function(x) x"), local = local)
            expect_null(value$fields$fun)
        }
    }
})

test_that("Source writes reuse flow inference for factory closures and complete long histories", {
    for (ctor in c("list()", "new.env()")) {
        lines <- c("make <- function() {", paste("  out <-", ctor), "  out$fun <- function(x, y) x + y",
            "  out", "}", "box <- make()")
        value <- member_write_receiver(lines)
        expect_identical(names(value$fields$fun$function_expr[[2L]]), c("x", "y"))
    }
    lines <- c("box <- list()", sprintf("box$f%d <- %d", 1:100, 1:100), "box$fun <- function() 0")
    value <- member_write_receiver(lines)
    expect_length(value$fields, 101L)
    expect_identical(value$fields$f100$literal, 100)
})

test_that("Qualified base constructors and masked replacement functions follow lexical lookup", {
    for (local in c(FALSE, TRUE)) {
        for (ctor in c("list", "new.env")) {
            lines <- c(paste(ctor, "<- function(...) opaque()"), sprintf("box <- base::%s()", ctor),
                "box$fun <- function(x) x")
            value <- member_write_receiver(lines, local = local)
            expect_identical(names(value$fields$fun$function_expr[[2L]]), "x")
            masked <- c(lines, "`$<-` <- function(x, name, value) list()", "box$fun <- function(fake) fake")
            expect_null(member_write_receiver(masked, local = local)$fields$fun)
        }
    }
})
