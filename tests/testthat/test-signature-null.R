nullable_signature_capabilities <- list(textDocument = list(signatureHelp = list(
    signatureInformation = list(noActiveParameterSupport = TRUE)
)))

nullable_signature_parameter <- function(code, signature, nullable = TRUE) {
    row <- which(grepl("<cursor>", code, fixed = TRUE))
    col <- regexpr("<cursor>", code[[row]], fixed = TRUE)[[1L]] - 1L
    code[[row]] <- sub("<cursor>", "", code[[row]], fixed = TRUE)
    document <- Document$new("file:///nullable-signature.R", content = code)
    point <- list(row = row - 1L, col = col)
    opening <- document$detect_call(point)$opening
    detect_active_parameter(code, opening$row, opening$col, point$row, point$col,
        signature, no_active_parameter = nullable)
}

test_that("Nullable signature parameters distinguish unmatched arguments", {
    signature <- "f(first, second)"
    for (code in c("f(unknown = <cursor>)", "f(1, 2, <cursor>)")) {
        expect_null(nullable_signature_parameter(code, signature), info = code)
    }
    expect_identical(nullable_signature_parameter("f(unknown = <cursor>)", signature,
            nullable = FALSE), 0L)
    expect_identical(nullable_signature_parameter("f(1, 2, <cursor>)", signature,
            nullable = FALSE), 2L)
    expect_null(nullable_signature_parameter("f(<cursor>)", "f()"))
})

test_that("Nullable signature parameters preserve R argument matching", {
    cases <- list(
        list("f(sec = <cursor>)", "f(first, second)", 1L),
        list("f(al = <cursor>, albert = 2)", "f(alpha, albert)", 0L),
        list("f(al = <cursor>, alpha = 2)", "f(alpha, also)", 1L),
        list("f(alpha = 2, al = <cursor>)", "f(alpha, also)", 1L),
        list("f(second = 1, <cursor>)", "f(first, second)", 0L),
        list("f(<cursor>, first = 1)", "f(first, second)", 1L),
        list("f(unknown = <cursor>)", "f(first, ..., named)", 1L),
        list("f(named = <cursor>)", "f(first, ..., named)", 2L),
        list("f(na = <cursor>)", "f(first, ..., named)", 1L),
        list("f(1, 2, <cursor>)", "f(first, ..., named)", 1L),
        list("f(`odd name` = <cursor>)", "f(first, `odd name`)", 1L),
        list(c("f(second # before equals", " = <cursor>)"), "f(first, second)", 1L)
    )
    for (case in cases) {
        expect_identical(nullable_signature_parameter(case[[1L]], case[[2L]]),
            case[[3L]], info = paste(case[[1L]], collapse = "\n"))
    }
    expect_null(nullable_signature_parameter("f(al = <cursor>)", "f(alpha, also)"))
    expect_null(nullable_signature_parameter("f(al = <cursor>)", "f(alpha, also, ...)"))
    expect_null(nullable_signature_parameter("f(fir = <cursor>, first = 1)", "f(first, second)"))
    expect_identical(nullable_signature_parameter("f(fir = <cursor>, first = 1)",
            "f(first, ...)"), 1L)
})

test_that("Nullable signature parameters preserve pipe argument placement", {
    cases <- list(
        list("x |> f(<cursor>)", "f(first, second)", 1L),
        list("x |> f(sec = <cursor>)", "f(first, second)", 1L),
        list("x |> f(al = <cursor>, alpha = _)", "f(alpha, also)", 1L),
        list("x |> f(<cursor>, second = _)", "f(first, second)", 0L),
        list("x %>% f(second = ., <cursor>)", "f(first, second)", 0L),
        list("x |> f(unknown = <cursor>)", "f(first, ...)", 1L),
        list("x %>% f(1, <cursor>)", "f(first, ...)", 1L)
    )
    for (case in cases) {
        expect_identical(nullable_signature_parameter(case[[1L]], case[[2L]]),
            case[[3L]], info = case[[1L]])
    }
    for (code in c("x |> f(unknown = <cursor>)", "x |> f(1, <cursor>)",
            "x %>% f(unknown = <cursor>)", "x %>% f(1, <cursor>)")) {
        expect_null(nullable_signature_parameter(code, "f(first, second)"), info = code)
    }
})

test_that("Nullable signature parameters decode literal argument tags without evaluation", {
    cases <- list(
        list('f("a\\u006cpha" = <cursor>)', "f(alpha)", 0L),
        list('f("al\\x70ha" = <cursor>)', "f(alpha)", 0L),
        list('f("\\141lpha" = <cursor>)', "f(alpha)", 0L),
        list('f(r"(alpha)" = <cursor>)', "f(alpha)", 0L),
        list('f(R"---[alpha]---" = <cursor>)', "f(alpha)", 0L),
        list('f("al\\u0070" = <cursor>)', "f(alpha, also)", 0L),
        list('f("a\\u006c" = <cursor>, alpha = 1)', "f(alpha, also)", 1L),
        list('f("odd name" = <cursor>)', "f(first, `odd name`)", 1L),
        list('f("line\\nname" = <cursor>)', "f(`line\\nname`)", 0L),
        list("f(`line\\nname` = <cursor>)", "f(`line\\nname`)", 0L),
        list('f("\\u6f22\\u5b57" = <cursor>)', "f(first, `漢字`)", 1L),
        list('x |> f("a\\u006cpha" = <cursor>, second = _)', "f(alpha, second)", 0L),
        list('x %>% f(second = ., r"(alpha)" = <cursor>)', "f(alpha, second)", 0L)
    )
    for (case in cases) {
        expect_identical(nullable_signature_parameter(case[[1L]], case[[2L]]),
            case[[3L]], info = case[[1L]])
    }
    withr::local_options(list(languageserver.literal_tag_test = FALSE))
    code <- 'f("options(languageserver.literal_tag_test = TRUE)" = <cursor>)'
    signature <- "f(`options(languageserver.literal_tag_test = TRUE)`)"
    expect_identical(nullable_signature_parameter(code, signature), 0L)
    expect_false(getOption("languageserver.literal_tag_test"))
})

test_that("Malformed literal tags do not fail signature matching or change legacy behavior", {
    for (code in c('f("\\q" = <cursor>)', 'f("\\uZZZZ" = <cursor>)',
            'f("" = <cursor>)', 'f(r"bad" = <cursor>)')) {
        expect_null(nullable_signature_parameter(code, "f(alpha)"), info = code)
    }
    for (code in c('f("a\\u006cpha" = <cursor>)', 'f(r"(alpha)" = <cursor>)')) {
        expect_identical(nullable_signature_parameter(code, "f(alpha)", nullable = FALSE), 0L,
            info = code)
    }
})

test_that("Ordinary signature look-ahead is bounded after the cursor", {
    for (padding in c(strrep(" ", 200000L), paste0("#", strrep(" ", 200000L)),
            paste0("%", strrep(" ", 200000L)))) {
        content <- c("f(", padding, ", first = 1)")
        # A later named argument reserves the first formal only within the
        # look-ahead budget. Its presence here would reveal unbounded scanning.
        expect_identical(detect_active_parameter(content, 0L, 1L, 0L, 2L,
                "f(first, second)", no_active_parameter = TRUE), 0L)
    }
    content <- c("f(", rep(paste0("#", strrep(" ", 100L)), 1000L), "second = 1)")
    expect_identical(detect_active_parameter(content, 0L, 1L,
            length(content) - 1L, nchar(tail(content, 1L)) - 1L,
            "f(first, second)", no_active_parameter = TRUE), 1L)
})

test_that("Signature replies negotiate and serialize explicit null parameters", {
    content <- c("f <- function(first, second) NULL", "f(unknown = 1)")
    fixture <- provider_fixture(content)
    point <- list(row = 1L, col = nchar(content[[2L]]) - 1L)
    legacy <- signature_reply(1L, fixture$uri, fixture$workspace, fixture$document, point)
    expect_identical(legacy$result$activeParameter, 0L)

    reply <- signature_reply(1L, fixture$uri, fixture$workspace, fixture$document,
        point, nullable_signature_capabilities)
    expect_length(reply$result$signatures, 1L)
    expect_true("activeParameter" %in% names(reply$result))
    expect_null(reply$result$activeParameter)
    expect_match(reply$to_json(), '"activeParameter":null', fixed = TRUE)

    unsupported <- nullable_signature_capabilities
    unsupported$textDocument$signatureHelp$signatureInformation$noActiveParameterSupport <- FALSE
    expect_identical(signature_reply(1L, fixture$uri, fixture$workspace,
            fixture$document, point, unsupported)$result$activeParameter, 0L)
})

test_that("S3 signature replies use negotiated null parameters", {
    document <- Document$new("file:///nullable-s3.R", content = "f(unknown = 1)")
    point <- list(row = 0L, col = nchar(document$content[[1L]]) - 1L)
    result <- list(signature = "f(first)", description = "An S3 method.",
        opening = list(row = 0L, col = 1L), method = list())
    legacy <- s3_signature_reply(1L, list(), document, point, result)
    expect_identical(legacy$result$activeParameter, 0L)
    reply <- s3_signature_reply(1L, list(), document, point, result,
        nullable_signature_capabilities)
    expect_length(reply$result$signatures, 1L)
    expect_true("activeParameter" %in% names(reply$result))
    expect_match(reply$to_json(), '"activeParameter":null', fixed = TRUE)
})

test_that("Member signatures use negotiated null parameters", {
    content <- c("make <- function() {", "self <- new.env(parent = emptyenv())",
        "self$method <- function(first) self", "self", "}", "obj <- make()",
        "obj$method(unknown = 1)")
    fixture <- provider_fixture(content)
    point <- list(row = 6L, col = nchar(content[[7L]]) - 1L)
    legacy <- signature_reply(1L, fixture$uri, fixture$workspace, fixture$document, point)
    expect_identical(legacy$result$activeParameter, 0L)
    reply <- signature_reply(1L, fixture$uri, fixture$workspace, fixture$document,
        point, nullable_signature_capabilities)
    expect_identical(reply$result$signatures[[1L]]$label, "method(first)")
    expect_true("activeParameter" %in% names(reply$result))
    expect_match(reply$to_json(), '"activeParameter":null', fixed = TRUE)
})

test_that("Installed signature help preserves explicit null parameters", {
    skip_on_cran()
    directory <- withr::local_tempdir()
    path <- file.path(directory, "nullable-signature.R")
    content <- c("f <- function(first, second) NULL", "f(unknown = 1)")
    writeLines(content, path)
    client <- language_client(working_dir = directory,
        capabilities = nullable_signature_capabilities)
    did_open(client, path)
    result <- respond_signature(client, path, c(1L, nchar(content[[2L]]) - 1L))
    expect_length(result$signatures, 1L)
    expect_true("activeParameter" %in% names(result))
    expect_null(result$activeParameter)
})
