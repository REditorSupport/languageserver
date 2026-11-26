test_that("bracket masking respects the receiver and the evaluation of i, j and controls", {
    lines <- c("v <- 1", "DT <- data.table::data.table(v = 1)",
        "DT[v]", "DT[v > 0]", "DT[, v, with = FALSE]", "DT[, v, by = v]",
        "DT[, v, env = list(v = v)]", "DT[, ..v]", "DT[[v]]",
        "f <- function(unknown) unknown[, v]",
        "d <- data.frame(v = 1)", "data.table::setDT(d)", "d[v > 0]",
        "DT <- matrix(1)", "DT[, v]", "DT <- unknown()", "DT[, v]")
    p <- parse_document("file:///mask.R", lines)
    s <- p$syntax_data
    mask <- p$data_mask
    rows <- mask$column
    expect_equal(s$line1[rows], c(4L, 6L, 6L, 7L, 8L, 10L, 13L, 17L))
    expect_equal(mask$indirect, rep("v", 4L))
    expect_equal(sort(s$line1[mask$indirect_row]), c(8L, 10L, 13L, 17L))
    expect_equal(mask$receiver[c(2L, 4L, 6L, 7L, 8L)], rep(0L, 5L))
    expect_true(all(mask$receiver[c(1L, 3L)] > 0L))
})

test_that("dplyr masks respect pipes, standard evaluation and lexical shadowing", {
    lines <- c("library(dplyr)", "filter(d, v)",
        "d |> dplyr::filter(v > .env$v)", "d %>% dplyr::filter(v > .data$v)",
        "dplyr::filter(d, .data[[v]] > !!v)", "dplyr::select(d, tidyselect::all_of(v))",
        "dplyr::mutate(d, x = tidyselect::all_of(v))", "dplyr::filter(d, {{v}} > 0)",
        "filter <- function(data, expr) expr", "filter(d, v)", "stats::filter(d, v)",
        "dplyr::filter(d, (function(v) v + 1)(1))",
        "dplyr::filter(d, (function() v)())", "dplyr::filter(d, .env[['v']] > .env$'z')",
        "dplyr::rowwise(v, data = d)", "dplyr::distinct(d, v, .keep_all = v)")
    if (getRversion() < "4.1.0") lines[[3L]] <- sub("|>", "%>%", lines[[3L]], fixed = TRUE)
    p <- parse_document("file:///mask.R", lines)
    s <- p$syntax_data
    mask <- p$data_mask
    rows <- mask$column[s$text[mask$column] == "v"]
    expect_equal(s$line1[rows], c(2L, 3L, 4L, 4L, 7L, 13L, 15L, 16L))
    expect_equal(s$line1[mask$env], 3L)
    expect_setequal(mask$indirect, c("v", "z"))
    expect_equal(class_token_text(s$text[mask$indirect_row]), mask$indirect)
})

test_that("nested member access through a masked receiver retains independent identity", {
    lines <- c("dplyr::mutate(d, out = obj$v, f = obj$sub$v(), x = obj[['v']])",
        "dplyr::filter(d, .env$obj$v > .data$obj$v)",
        "base::with(d, obj$v)", "obj$v")
    p <- parse_document("file:///mask.R", lines)
    s <- p$syntax_data
    mask <- p$data_mask
    rows <- mask$column[s$text[mask$column] %in% c("v", "'v'")]
    expect_equal(s$line1[rows], c(1L, 1L, 1L, 2L, 3L))
    expect_equal(s$col1[rows], c(28L, 43L, 57L, 41L, 19L))
    expect_true(all(vapply(rows, function(row) identical(data_mask_receiver(mask, row), 0L), logical(1L))))
    expect_equal(s$text[mask$env], "obj")
})

test_that("unknown bracket dispatch declines partial lexical renames", {
    f <- provider_fixture(c("f <- function(x, i) x[, i]", "g <- function(i) i",
            "obj <- list(field = 1)", "field <- 42", "unknown[, obj$field]"))
    rename <- function(line, char) {
        rename_reply(1L, f$uri, f$workspace, f$document, list(row = line, col = char), "other")
    }
    expect_match(rename(0L, 17L)$error$message, "data.mask")
    expect_length(rename(1L, 14L)$result$changes[[f$uri]], 2L)
    expect_match(rename(2L, 0L)$error$message, "data.mask")
    expect_length(rename(3L, 0L)$result$changes[[f$uri]], 1L)
    mask <- f$document$parse_data$data_mask
    expect_setequal(mask$indirect, c("i", "obj"))

    f <- provider_fixture(c("i <- 1", "dt <- data.table::data.table(i = 1)",
            "m <- unique(cbind(1:3, rbind(1L, 2L, 3L)))", "m[, i]", "dt[, i]"))
    edits <- rename(0L, 0L)$result$changes[[f$uri]]
    expect_equal(vapply(edits, function(edit) edit$range$start$line, numeric(1L)), c(0L, 3L))
})

test_that("unqualified verbs preserve only verified lexical and pronoun bindings", {
    f <- provider_fixture(c("v <- 3", "d <- data.frame(v = 1)", "filter(d, v)",
            "mutate(d, x = .env$v)", "filter(d, .env[['v']] > 0)"))
    rename <- function() {
        rename_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 0L), "outer")
    }
    expect_match(rename()$error$message, "data.mask")
    mask <- f$document$parse_data$data_mask
    s <- f$document$parse_data$syntax_data
    expect_equal(sort(s$line1[mask$indirect_row[mask$indirect == "v"]]), 3:5)
    expect_length(mask$env, 0L)

    f <- provider_fixture(c("v <- 3", "filter <- function(d, x) x", "filter(d, v)", "stats::filter(d, v)"))
    expect_length(rename()$result$changes[[f$uri]], 3L)

    p <- parse_document("file:///mask.R", c("library(tidyverse)", "filter(d, .env$v)",
            "library(MASS)", "select(d, .env$v)", "library(dplyr)", "filter(d, .env$v)"))
    expect_equal(p$syntax_data$line1[p$data_mask$env], 2L)
    rows <- p$data_mask$indirect_row[p$data_mask$indirect == "v"]
    expect_equal(p$syntax_data$line1[rows], c(4L, 6L))
})
