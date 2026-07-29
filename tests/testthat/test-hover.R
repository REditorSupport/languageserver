test_that("Simple hover works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "    strsplit",
            "fs::path_real"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_hover(temp_file, c(0, 7))
    expect_length(result$contents, 1)
    expect_true(stringi::stri_detect_fixed(result$contents[1], "strsplit"))
    expect_equal(result$range$end$character, 12)

    result <- client %>% respond_hover(temp_file, c(1, 7))
    expect_length(result$contents, 1)
    expect_true(stringi::stri_detect_fixed(result$contents[1], "path_real"))
    expect_equal(result$range$end$character, 13)
})

test_that("function argument hover falls back through sparse documentation", {
    workspace <- new.env(parent = baseenv())
    workspace$get_documentation <- function(...) "plain text"
    workspace$get_signature <- function(...) NULL
    expect_null(function_argument_hover_contents(
        workspace, "target", NULL, "argument"
    ))

    workspace$get_documentation <- function(...) {
        list(arguments = list("..." = "additional arguments"))
    }
    expect_equal(
        function_argument_hover_contents(workspace, "target", NULL, "missing"),
        "additional arguments"
    )

    workspace$get_documentation <- function(...) list(arguments = list())
    expect_null(function_argument_hover_contents(
        workspace, "target", NULL, "missing"
    ))
})

test_that("hover handles package and literal token classes", {
    fixture <- provider_fixture(c(
        "base::mean",
        "missingHoverPackage::fun",
        "object@slot",
        "'text'",
        "# comment",
        "1 + 2"
    ))
    fixture$workspace$get_help <- function(...) NULL
    fixture$workspace$get_documentation <- function(...) NULL
    fixture$workspace$get_signature <- function(...) NULL
    fixture$workspace$get_definition <- function(...) NULL
    fixture$workspace$guess_namespace <- function(...) NULL

    outside <- hover_reply(
        1L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(row = 99L, col = 0L)
    )
    expect_null(outside$result)

    installed <- hover_reply(
        2L, fixture$uri, fixture$workspace, fixture$document,
        list(row = 0L, col = 1L)
    )
    expect_match(
        paste(installed$result$contents, collapse = " "),
        "base",
        ignore.case = TRUE
    )

    missing <- hover_reply(
        3L, fixture$uri, fixture$workspace, fixture$document,
        list(row = 1L, col = 2L)
    )
    expect_match(
        paste(missing$result$contents, collapse = " "),
        "not installed"
    )

    for (point in list(
        list(row = 2L, col = 8L),
        list(row = 3L, col = 2L),
        list(row = 4L, col = 2L),
        list(row = 5L, col = 2L)
    )) {
        expect_null(hover_reply(
            4L, fixture$uri, fixture$workspace, fixture$document, point
        )$result)
    }
})

test_that("hover combines fallback signatures with character and list docs", {
    fixture <- provider_fixture("mystery")
    fixture$workspace$get_help <- function(...) NULL
    fixture$workspace$guess_namespace <- function(...) "workspace"
    fixture$workspace$get_signature <- function(...) "mystery(value)"
    fixture$workspace$get_definition <- function(...) NULL
    point <- list(row = 0L, col = 2L)

    fixture$workspace$get_documentation <- function(...) "character docs"
    character_reply <- hover_reply(
        1L, fixture$uri, fixture$workspace, fixture$document, point
    )
    expect_match(character_reply$result$contents[[1L]], "mystery\\(value\\)")
    expect_equal(character_reply$result$contents[[2L]], "character docs")

    fixture$workspace$get_documentation <- function(...) {
        list(description = "description docs")
    }
    description_reply <- hover_reply(
        2L, fixture$uri, fixture$workspace, fixture$document, point
    )
    expect_equal(description_reply$result$contents[[2L]], "description docs")

    fixture$workspace$get_documentation <- function(...) {
        list(description = "ignored", markdown = "markdown docs")
    }
    markdown_reply <- hover_reply(
        3L, fixture$uri, fixture$workspace, fixture$document, point
    )
    expect_equal(markdown_reply$result$contents[[2L]], "markdown docs")
})

test_that("Hover on user function works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test1 <- function(x, y) x + 1",
            "test1",
            "test2 = function(x, y) x - 1",
            "test2"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_hover(temp_file, c(1, 3))
    expect_length(result$contents, 1)
    expect_equal(result$contents[1], "```r\ntest1(x, y)\n```")
    expect_equal(result$range$end$character, 5)

    result <- client %>% respond_hover(temp_file, c(3, 3))
    expect_length(result$contents, 1)
    expect_equal(result$contents[1], "```r\ntest2(x, y)\n```")
    expect_equal(result$range$end$character, 5)
})

test_that("Hover on user function with multi-lined arguments works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test1 <- function(",
            "  x, # arg 1",
            "  y  # arg 2",
            ") {",
            "    x + y",
            "}",
            "test1",
            "test2 = function(",
            "  x, # arg 1",
            "  y  # arg 2",
            ") {",
            "    x + y",
            "}",
            "test2"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_hover(temp_file, c(6, 3))
    expect_length(result$contents, 1)
    expect_equal(result$contents[1], "```r\ntest1(x, y)\n```")
    expect_equal(result$range$end$character, 5)

    result <- client %>% respond_hover(temp_file, c(13, 3))
    expect_length(result$contents, 1)
    expect_equal(result$contents[1], "```r\ntest2(x, y)\n```")
    expect_equal(result$range$end$character, 5)
})

test_that("Hover on variable works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "var1 <- 1:10",
            "f(var1)",
            "local({",
            "   var2 <- 2:10",
            "   f(var1, var2)",
            "   var1 <- 0",
            "   f(var1, var2)",
            "})"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_hover(temp_file, c(0, 1))
    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 0, character = 4))
    expect_equal(result$contents, "```r\nvar1 <- 1:10\n```")

    result <- client %>% respond_hover(temp_file, c(1, 3))
    expect_equal(result$range$start, list(line = 1, character = 2))
    expect_equal(result$range$end, list(line = 1, character = 6))
    expect_equal(result$contents, "```r\nvar1 <- 1:10\n```")

    result <- client %>% respond_hover(temp_file, c(4, 6))
    expect_equal(result$range$start, list(line = 4, character = 5))
    expect_equal(result$range$end, list(line = 4, character = 9))
    expect_equal(result$contents, "```r\nvar1 <- 1:10\n```")

    result <- client %>% respond_hover(temp_file, c(4, 13))
    expect_equal(result$range$start, list(line = 4, character = 11))
    expect_equal(result$range$end, list(line = 4, character = 15))
    expect_equal(result$contents, "```r\nvar2 <- 2:10\n```")

    result <- client %>% respond_hover(temp_file, c(6, 6))
    expect_equal(result$range$start, list(line = 6, character = 5))
    expect_equal(result$range$end, list(line = 6, character = 9))
    expect_equal(result$contents, "```r\nvar1 <- 0\n```")
})

test_that("Hover on variable with leading tabs works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "index1 <- 1:10",
            "\tindex1 + 1",
            "\t\tindex1 + 2"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_hover(temp_file, c(0, 1))
    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 0, character = 6))
    expect_equal(result$contents, "```r\nindex1 <- 1:10\n```")

    result <- client %>% respond_hover(temp_file, c(1, 4))
    expect_equal(result$range$start, list(line = 1, character = 1))
    expect_equal(result$range$end, list(line = 1, character = 7))
    expect_equal(result$contents, "```r\nindex1 <- 1:10\n```")

    result <- client %>% respond_hover(temp_file, c(2, 7))
    expect_equal(result$range$start, list(line = 2, character = 2))
    expect_equal(result$range$end, list(line = 2, character = 8))
    expect_equal(result$contents, "```r\nindex1 <- 1:10\n```")
})

test_that("Hover on variable works with semi-colons", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "var1 <- 1:10;",
            "f(var1)",
            "local({",
            "   var2 <- 2:10;",
            "   f(var1, var2)",
            "   var1 <- 0;",
            "   f(var1, var2)",
            "})"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_hover(temp_file, c(0, 1))
    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 0, character = 4))
    expect_equal(result$contents, "```r\nvar1 <- 1:10;\n```")

    result <- client %>% respond_hover(temp_file, c(1, 3))
    expect_equal(result$range$start, list(line = 1, character = 2))
    expect_equal(result$range$end, list(line = 1, character = 6))
    expect_equal(result$contents, "```r\nvar1 <- 1:10;\n```")

    result <- client %>% respond_hover(temp_file, c(4, 6))
    expect_equal(result$range$start, list(line = 4, character = 5))
    expect_equal(result$range$end, list(line = 4, character = 9))
    expect_equal(result$contents, "```r\nvar1 <- 1:10;\n```")

    result <- client %>% respond_hover(temp_file, c(4, 13))
    expect_equal(result$range$start, list(line = 4, character = 11))
    expect_equal(result$range$end, list(line = 4, character = 15))
    expect_equal(result$contents, "```r\nvar2 <- 2:10;\n```")

    result <- client %>% respond_hover(temp_file, c(6, 6))
    expect_equal(result$range$start, list(line = 6, character = 5))
    expect_equal(result$range$end, list(line = 6, character = 9))
    expect_equal(result$contents, "```r\nvar1 <- 0;\n```")
})

test_that("Hover works in scope with different assignment operators", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "my_fn <- function(var1) {",
        "  var2 <- 1",
        "  var3 = 2",
        "  3 -> var4",
        "  for (var5 in 1:10) {",
        "    var1 + var2 + var3 + var4 + var5",
        "  }",
        "}",
        "my_fn(1)"
    ), temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_hover(temp_file, c(8, 0))
    expect_equal(result$range$start, list(line = 8, character = 0))
    expect_equal(result$range$end, list(line = 8, character = 5))
    expect_equal(result$contents, "```r\nmy_fn(var1)\n```")

    result <- client %>% respond_hover(temp_file, c(5, 5))
    expect_equal(result$range$start, list(line = 5, character = 4))
    expect_equal(result$range$end, list(line = 5, character = 8))
    expect_equal(result$contents, "```r\nmy_fn <- function(var1) {\n```")

    result <- client %>% respond_hover(temp_file, c(5, 12))
    expect_equal(result$range$start, list(line = 5, character = 11))
    expect_equal(result$range$end, list(line = 5, character = 15))
    expect_equal(result$contents, "```r\nvar2 <- 1\n```")

    result <- client %>% respond_hover(temp_file, c(5, 20))
    expect_equal(result$range$start, list(line = 5, character = 18))
    expect_equal(result$range$end, list(line = 5, character = 22))
    expect_equal(result$contents, "```r\nvar3 = 2\n```")

    result <- client %>% respond_hover(temp_file, c(5, 26))
    expect_equal(result$range$start, list(line = 5, character = 25))
    expect_equal(result$range$end, list(line = 5, character = 29))
    expect_equal(result$contents, "```r\n3 -> var4\n```")

    result <- client %>% respond_hover(temp_file, c(5, 34))
    expect_equal(result$range$start, list(line = 5, character = 32))
    expect_equal(result$range$end, list(line = 5, character = 36))
    expect_equal(result$contents, "```r\nfor (var5 in 1:10) {\n```")
})

test_that("Hover works on both sides of assignment", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "var1 <- 1",
        "var1 <- var1 + 1",
        "var2 = 2",
        "var2 = var2 + 2",
        "3 -> var3",
        "var3 + 3 -> var3"
    ), single_file)

    client %>% did_open(single_file)

    result <- client %>% respond_hover(single_file, c(0, 1))
    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 0, character = 4))
    expect_equal(result$contents, "```r\nvar1 <- 1\n```")

    result <- client %>% respond_hover(single_file, c(1, 1))
    expect_equal(result$range$start, list(line = 1, character = 0))
    expect_equal(result$range$end, list(line = 1, character = 4))
    expect_equal(result$contents, "```r\nvar1 <- var1 + 1\n```")

    result <- client %>% respond_hover(single_file, c(1, 9))
    expect_equal(result$range$start, list(line = 1, character = 8))
    expect_equal(result$range$end, list(line = 1, character = 12))
    expect_equal(result$contents, "```r\nvar1 <- 1\n```")

    result <- client %>% respond_hover(single_file, c(2, 1))
    expect_equal(result$range$start, list(line = 2, character = 0))
    expect_equal(result$range$end, list(line = 2, character = 4))
    expect_equal(result$contents, "```r\nvar2 = 2\n```")

    result <- client %>% respond_hover(single_file, c(3, 1))
    expect_equal(result$range$start, list(line = 3, character = 0))
    expect_equal(result$range$end, list(line = 3, character = 4))
    expect_equal(result$contents, "```r\nvar2 = var2 + 2\n```")

    result <- client %>% respond_hover(single_file, c(3, 8))
    expect_equal(result$range$start, list(line = 3, character = 7))
    expect_equal(result$range$end, list(line = 3, character = 11))
    expect_equal(result$contents, "```r\nvar2 = 2\n```")

    result <- client %>% respond_hover(single_file, c(4, 6))
    expect_equal(result$range$start, list(line = 4, character = 5))
    expect_equal(result$range$end, list(line = 4, character = 9))
    expect_equal(result$contents, "```r\n3 -> var3\n```")

    result <- client %>% respond_hover(single_file, c(5, 1))
    expect_equal(result$range$start, list(line = 5, character = 0))
    expect_equal(result$range$end, list(line = 5, character = 4))
    expect_equal(result$contents, "```r\n3 -> var3\n```")

    result <- client %>% respond_hover(single_file, c(5, 15))
    expect_equal(result$range$start, list(line = 5, character = 12))
    expect_equal(result$range$end, list(line = 5, character = 16))
    expect_equal(result$contents, "```r\nvar3 + 3 -> var3\n```")
})

test_that("Hover on function argument works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "unlist(list(a = 1, b = 2), recursive = FALSE)",
            "x <- list(var1 = 1, var2 = 2)"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_hover(temp_file, c(0, 30))
    expect_equal(result$range$start, list(line = 0, character = 27))
    expect_equal(result$range$end, list(line = 0, character = 36))
    expect_equal(result$contents[[1]], "```r\nunlist(x, recursive = TRUE, use.names = TRUE) \n```")
    expect_true(stringi::stri_detect_fixed(result$contents[[2]], "`recursive` - logical"))
    expect_true(stringi::stri_detect_fixed(
        result$contents[[2]],
        "Should unlisting be applied to list components of `x`"
    ))

    result <- client %>% respond_hover(temp_file, c(1, 12))
    expect_equal(result$range$start, list(line = 1, character = 10))
    expect_equal(result$range$end, list(line = 1, character = 14))
    expect_equal(result$contents, list(
        "```r\nlist(...) \n```",
        "`...` - objects, possibly named."
    ))
})

test_that("Hover on user function with function argument works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test <- function(var1, var2 = function(x) x + 1) {",
            "  var1",
            "  var2",
            "}"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_hover(temp_file, c(1, 3))
    expect_length(result$contents, 1)
    expect_equal(result$contents[1], "```r\ntest <- function(var1, var2 = function(x) x + 1) {\n```")
    expect_equal(result$range$start, list(line = 1, character = 2))
    expect_equal(result$range$end, list(line = 1, character = 6))

    result <- client %>% respond_hover(temp_file, c(2, 3))
    expect_length(result$contents, 1)
    expect_equal(result$contents[1], "```r\ntest <- function(var1, var2 = function(x) x + 1) {\n```")
    expect_equal(result$range$start, list(line = 2, character = 2))
    expect_equal(result$range$end, list(line = 2, character = 6))
})

test_that("Hover works with local function", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "local({",
            "  #' test function",
            "  #' @param var1 a number",
            "  test <- function(var1, var2=1) {",
            "    var1 + var2",
            "  }",
            "  test(var1 = 1, var2 = 2)",
            "})"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_hover(temp_file, c(3, 20))
    expect_equal(result$range$start, list(line = 3, character = 19))
    expect_equal(result$range$end, list(line = 3, character = 23))
    expect_equal(result$contents, "`var1` - a number")

    result <- client %>% respond_hover(temp_file, c(6, 4))
    expect_equal(result$range$start, list(line = 6, character = 2))
    expect_equal(result$range$end, list(line = 6, character = 6))
    expect_equal(result$contents, list(
        "```r\ntest(var1, var2 = 1)\n```",
        "test function  \n\n`@param` `var1` a number  \n"
    ))

    result <- client %>% respond_hover(temp_file, c(6, 9))
    expect_equal(result$range$start, list(line = 6, character = 7))
    expect_equal(result$range$end, list(line = 6, character = 11))
    expect_equal(result$contents, list(
        "```r\ntest(var1, var2 = 1)\n```",
        "`var1` - a number"
    ))
})

test_that("Hover on operator is ignored", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "for (ll in 1:3) {",
            " p = ll",
            " I = array(0:0, dim=c(p,p))",
            "}"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_hover(temp_file, c(2, 22),
        retry_when = function(result) length(result) > 0)
    expect_equal(result, NULL)

    result <- client %>% respond_hover(temp_file, c(2, 23))
    expect_equal(result$range$start, list(line = 2, character = 22))
    expect_equal(result$range$end, list(line = 2, character = 23))
    expect_equal(result$contents, "```r\np = ll\n```")
})

test_that("Hover works across multiple files", {
    skip_on_cran()
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    query_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c("test <- 1"), defn_file)
    writeLines(c("test + 1"), query_file)

    client %>% did_open(defn_file)
    client %>% did_open(query_file)

    result <- client %>% respond_hover(query_file, c(0, 0))

    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 0, character = 4))
    expect_equal(result$contents, "```r\ntest <- 1\n```")
})

test_that("Simple hover works in Rmarkdown", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".Rmd")
    writeLines(
        c(
            "---",
            "title: r markdown",
            "---",
            "Using strsplit",
            "```{r}",
            "    strsplit",
            "fs::path_real",
            "```"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_hover(temp_file, c(5, 7))
    expect_length(result$contents, 1)
    expect_true(stringi::stri_detect_fixed(result$contents[1], "strsplit"))
    expect_equal(result$range$end$character, 12)

    result <- client %>% respond_hover(temp_file, c(6, 7))
    expect_length(result$contents, 1)
    expect_true(stringi::stri_detect_fixed(result$contents[1], "path_real"))
    expect_equal(result$range$end$character, 13)
})

test_that("Hover on user function works in Rmarkdown", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".Rmd")
    writeLines(
        c(
            "---",
            "title: r markdown",
            "---",
            "some text here",
            "```{r}",
            "test1 <- function(x, y) x + 1",
            "test1",
            "```"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_hover(temp_file, c(6, 3))
    expect_length(result$contents, 1)
    expect_equal(result$contents[1], "```r\ntest1(x, y)\n```")
    expect_equal(result$range$end$character, 5)
})

test_that("Hover on variable works in Rmarkdown", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".Rmd")
    writeLines(
        c(
            "---",
            "title: r markdown",
            "---",
            "some text here",
            "```{r}",
            "var1 <- 1:10",
            "f(var1)",
            "local({",
            "   var2 <- 2:10",
            "   f(var1, var2)",
            "   var1 <- 0",
            "   f(var1, var2)",
            "})",
            "```"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_hover(temp_file, c(5, 1))
    expect_equal(result$range$start, list(line = 5, character = 0))
    expect_equal(result$range$end, list(line = 5, character = 4))
    expect_equal(result$contents, "```r\nvar1 <- 1:10\n```")

    result <- client %>% respond_hover(temp_file, c(6, 3))
    expect_equal(result$range$start, list(line = 6, character = 2))
    expect_equal(result$range$end, list(line = 6, character = 6))
    expect_equal(result$contents, "```r\nvar1 <- 1:10\n```")

    result <- client %>% respond_hover(temp_file, c(9, 6))
    expect_equal(result$range$start, list(line = 9, character = 5))
    expect_equal(result$range$end, list(line = 9, character = 9))
    expect_equal(result$contents, "```r\nvar1 <- 1:10\n```")

    result <- client %>% respond_hover(temp_file, c(9, 13))
    expect_equal(result$range$start, list(line = 9, character = 11))
    expect_equal(result$range$end, list(line = 9, character = 15))
    expect_equal(result$contents, "```r\nvar2 <- 2:10\n```")

    result <- client %>% respond_hover(temp_file, c(11, 6))
    expect_equal(result$range$start, list(line = 11, character = 5))
    expect_equal(result$range$end, list(line = 11, character = 9))
    expect_equal(result$contents, "```r\nvar1 <- 0\n```")
})

test_that("Hover on function argument works in Rmarkdown", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".Rmd")
    writeLines(
        c(
            "---",
            "title: r markdown",
            "---",
            "some text here",
            "```{r}",
            "unlist(list(a = 1, b = 2), recursive = FALSE)",
            "x <- list(var1 = 1, var2 = 2)",
            "```"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_hover(temp_file, c(5, 30))
    expect_equal(result$range$start, list(line = 5, character = 27))
    expect_equal(result$range$end, list(line = 5, character = 36))
    expect_equal(result$contents[[1]], "```r\nunlist(x, recursive = TRUE, use.names = TRUE) \n```")
    expect_true(stringi::stri_detect_fixed(result$contents[[2]], "`recursive` - logical"))
    expect_true(stringi::stri_detect_fixed(
        result$contents[[2]],
        "Should unlisting be applied to list components of `x`"
    ))

    result <- client %>% respond_hover(temp_file, c(6, 12))
    expect_equal(result$range$start, list(line = 6, character = 10))
    expect_equal(result$range$end, list(line = 6, character = 14))
    expect_equal(result$contents, list(
        "```r\nlist(...) \n```",
        "`...` - objects, possibly named."
    ))
})
