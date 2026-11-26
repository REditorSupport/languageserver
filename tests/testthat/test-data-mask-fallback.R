test_that("a possible caller lookup in a data mask prevents a partial lexical rename", {
    for (use in c("dplyr::filter(df, x > threshold)", "with(df, x > threshold)",
            "base::subset(df, x > threshold)", "DT[, x > threshold]")) {
        f <- provider_fixture(c("threshold <- 2L", "df <- data.frame(x = 1L)",
                "DT <- data.table::data.table(x = 1L)", use))
        f$workspace$get_definition <- function(...) NULL
        at <- list(row = 0L, col = 2L)
        result <- rename_reply(1L, f$uri, f$workspace, f$document, at, "limit")
        expect_match(result$error$message, "data.mask")
        expect_match(prepare_rename_reply(1L, f$uri, f$workspace, f$document, at)$error$message, "data.mask")
    }
})

test_that("fallback checks use the field at the read and respect explicit data pronouns", {
    rename <- function(lines) {
        f <- provider_fixture(c("value <- 2L", lines))
        f$workspace$get_definition <- function(...) NULL
        rename_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 2L), "renamed")
    }
    known <- rename(c("df <- data.frame(value = 1L)", "dplyr::filter(df, value < .env$value)"))
    expect_null(known$error)
    expect_length(known$result$changes[[1L]], 2L)
    explicit <- rename("dplyr::filter(unknown, .data$value < .env$value)")
    expect_null(explicit$error)
    expect_length(explicit$result$changes[[1L]], 2L)
    for (lines in list("dplyr::filter(unknown, value > 1L)",
            c("df <- data.frame(value = 1L)", "df$value <- NULL", "with(df, value)"),
            c("DT <- data.table::data.table(value = 1L)", "alias <- DT", "DT[, value]"))) {
        expect_match(rename(lines)$error$message, "data.mask")
    }
})
