inherit_syntax_index <- function(lines, uri = "file:///topics.R", row_offset = 0L, col_offset = 0L) {
    parsed <- parse(text = normalize_parse_content(lines), keep.source = TRUE)
    member_roxygen_type_index(lines, utils::getParseData(parsed), row_offset = row_offset,
        col_offset = col_offset, uri = uri)
}

test_that("source topics retain direct descriptions and ordered literal donors", {
    lines <- c("#' @param b Handled by this function.", "#' @param z (`Own`) The direct type.",
        "#' @inheritParams project::base_topic a -c", "#' @inheritParams fallback first:third",
        "#' @inheritParams base_topic", "child <- function(a, b, z) a",
        "#' @param a,b (`pkg::A | B`) Shared.", "#' @param", "#'   c Plain.",
        "base_topic <- function(a, b, c) a")
    index <- inherit_syntax_index(lines)
    topics <- attr(index, "topics", exact = TRUE)
    child <- topics[["6:10"]]
    expect_identical(child$topic, "child")
    expect_identical(child$documented, c("b", "z"))
    expect_identical(child$formals, c("a", "b", "z"))
    expect_identical(index[["6:10"]]$params, list(z = "Own"))
    expect_identical(child$inherit, list(list(topic = "project::base_topic", selector = "a -c"),
            list(topic = "fallback", selector = "first:third"), list(topic = "base_topic", selector = "")))
    donor <- topics[["10:15"]]
    expect_identical(donor$groups, list(c("a", "b"), "c"))
    expect_identical(index[["10:15"]]$params, list(a = c("pkg::A", "B"), b = c("pkg::A", "B")))
    expect_identical(attr(index[[1L]], "roxygen_documented"), c("b", "z"))
    expect_true(attr(index[[1L]], "roxygen_inherits"))
    expect_null(attr(index[[2L]], "roxygen_inherits"))
    expect_identical(attr(index[[1L]], "roxygen_uri"), "file:///topics.R")
    expect_identical(attr(index[[1L]], "roxygen_key"), "6:10")
    expect_identical(sort(unique(unlist(index, use.names = FALSE))), sort(c("Own", "pkg::A", "B")))
})

test_that("named NULL donors and ordinary documentation stay out of the function AST", {
    lines <- c("#' @name common-parameters", "#' @aliases common_data", "#' @param x (`Frame`) Input.", "NULL",
        "#' @rdname shared", "#' @param x Plain.", "first <- function(x) x",
        "#' @describeIn shared Another entry.", "#' @param y Plain.", "second <- function(y) y")
    index <- inherit_syntax_index(lines, uri = "file:///project/R/doc.R")
    expect_length(index, 0L)
    expect_false(attr(index, "has_inheritance"))
    records <- attr(index, "topics")
    expect_identical(vapply(records, `[[`, "", "topic", USE.NAMES = FALSE), c("common-parameters", "first", "second"))
    expect_identical(records[[1L]]$page, "common-parameters")
    expect_null(records[[1L]]$formals)
    expect_identical(records[[1L]]$group_types, list("Frame"))
    expect_identical(records[[2L]]$groups, list("x"))
    expect_identical(records[[2L]]$page, "shared")
    expect_identical(records[[3L]]$page, "shared")
    expect_identical(attr(index, "occurrences")$row, 2L)
    expect_identical(attr(index, "occurrences")$doc_only, TRUE)
    parsed <- parse(text = lines, keep.source = TRUE)
    expect_identical(member_attach_type_hints(parsed, index), parsed)
})

test_that("only top-level declarations publish donor names", {
    lines <- c("#' @noRd", "#' @param x (`Hidden`) Local.", "hidden <- function(x) x",
        "outer <- function() {", "  #' @inheritParams hidden", "  inner <- function(x) x", "}",
        "methods <- list(", "  #' @inheritParams hidden", "  method = function(x) x)",
        "#' @inheritParams dynamic()", "irrelevant <- function(x) x",
        "#' @name explicit", "#' @rdname page", "#' @aliases NULL other-name", "#' @param x Plain.",
        "published <- function(x) x")
    index <- inherit_syntax_index(lines)
    r <- attr(index, "topics")
    expect_null(r[[1L]]$topic)
    expect_null(r[[2L]]$topic)
    expect_null(r[[3L]]$topic)
    expect_identical(r[[2L]]$inherit, list(list(topic = "hidden", selector = "")))
    expect_false(any(vapply(index, function(h) identical(attr(h, "roxygen_key"), "12:15"), logical(1L))))
    expect_identical(r[[length(r)]]$topic, "explicit")
    expect_identical(r[[length(r)]]$page, "page")
})

test_that("inheritance-only functions carry sparse source identity in complete and repaired syntax", {
    lines <- c("#' @inheritParams donor", "f <- function(x) x")
    index <- inherit_syntax_index(lines, uri = NULL)
    expect_length(index, 1L)
    expect_null(attr(index, "occurrences"))
    expect_true(attr(index, "has_inheritance"))
    index <- member_type_hint_uri(index, "file:///here.R")
    fn <- member_attach_type_hints(parse(text = lines, keep.source = TRUE), index)[[1L]][[3L]]
    hints <- attr(fn, "member_type_hints", exact = TRUE)
    expect_identical(hints$params, list())
    expect_identical(attr(hints, "roxygen_uri"), "file:///here.R")
    expect_identical(attr(hints, "roxygen_key"), "2:6")
    expect_null(attr(index, "occurrences"))
    partial <- c("```{r}", "#' @inheritParams wrong", "```", "```{r}",
        "#' @inheritParams donor", "f <- function(x) x$", "```")
    data <- parse_document("file:///blocks.qmd", partial, is_rmarkdown = TRUE)$member_data
    expect_identical(attr(data$type_hints, "topics")[["6:6"]]$inherit, list(list(topic = "donor", selector = "")))
})

test_that("ordinary scripts collect prose donors only when inheritance is relevant", {
    lines <- c("#' @param x Ordinary prose.", "donor <- function(x) x")
    expect_null(member_roxygen_type_index(lines, stop("unneeded syntax"), uri = "file:///script.R"))
    package <- inherit_syntax_index(lines, uri = "file:///repo/R/donor.R")
    expect_length(package, 0L)
    expect_identical(attr(package, "topics")[[1L]]$documented, "x")
    mixed <- inherit_syntax_index(c(lines, "#' @inheritParams donor", "borrow <- function(x) x"))
    expect_identical(attr(mixed, "topics")[[1L]]$topic, "donor")
    expect_true(attr(mixed, "has_inheritance"))
    fake <- c('s <- "begin', "#' @inheritParams donor", "f <- function(x) x", 'end"')
    expect_null(inherit_syntax_index(fake))
})

test_that("fragment-only inheritance records use physical coordinates and verified comments", {
    uri <- "file:///source.R"
    donor <- inherit_syntax_index(c("#' @param x (`FromDonor`)", "donor <- function(x) x"), uri)
    fragment <- c("#' @inheritParams donor", "inner <- function(x) x")
    parse_fragment <- function(verified) {
        member_parse_recovered(fragment, NULL, 10L, 3L, type_hints = donor, hint_comments = verified)[[1L]][[3L]]
    }
    expect_null(attr(parse_fragment(8L), "member_type_hints"))
    hints <- attr(parse_fragment(11L), "member_type_hints")
    expect_identical(attr(hints, "roxygen_key"), "12:10")
    expect_identical(attr(hints, "roxygen_uri"), uri)
    recovered <- attr(hints, "roxygen_record")
    expect_identical(recovered$row, 11L)
    expect_identical(recovered$col, 9L)
    expect_identical(recovered$inherit, list(list(topic = "donor", selector = "")))
})

test_that("title-only primary topic records reveal shared pages", {
    main <- c("#' Main page.", "first <- function() NULL")
    expect_null(member_roxygen_type_index(main, stop("unneeded syntax"), uri = "file:///plain.R"))
    page <- inherit_syntax_index(main, uri = "file:///project/R/main.R")
    expect_length(page, 0L)
    expect_identical(attr(page, "topics")[[1L]]$topic, "first")
    secondary <- c("#' @rdname first", "#' @param x (`Model`) Input.", "second <- function(x) x")
    same_file <- inherit_syntax_index(c(main, secondary, "#' @inheritParams second", "borrow <- function(x) x"))
    records <- attr(same_file, "topics")
    expect_identical(vapply(records, `[[`, "", "page", USE.NAMES = FALSE), c("first", "first", "borrow"))
    expect_false(attr(same_file, "occurrences")$doc_only)
})

test_that("effective types follow Rd groups and repeated descriptions independently of direct hints", {
    lines <- c("#' @param a,b (`Group`) Joint description.", "#' @param a (`Other`) For a alone.",
        "donor <- function(a) NULL", "#' @inheritParams donor", "one <- function(a) NULL",
        "#' @inheritParams donor", "both <- function(a, b) NULL", "#' @param x (`Earlier`) First.",
        "#' @param x Plain replacement.", "plain <- function(x) x", "#' @param x (`Later`) Alternative.",
        "later <- function(x) x", "#' @inheritParams plain", "#' @inheritParams later", "blocked <- function(x) NULL")
    f <- provider_fixture(lines)
    index <- f$document$parse_data$member_data$type_hints
    topics <- attr(index, "topics")
    key <- function(name) names(Filter(function(rec) identical(rec$topic, name), topics))[[1L]]
    effective <- function(name) {
        resolved <- roxygen_effective_params(index[[key(name)]], f$workspace, f$document)
        lapply(resolved, function(hint) if (is.list(hint)) hint$types else hint)
    }
    expect_identical(topics[[key("donor")]]$groups, list(c("a", "b"), "a"))
    expect_identical(topics[[key("donor")]]$group_types, list("Group", "Other"))
    expect_identical(effective("one"), list(a = "Other"))
    expect_identical(effective("both"), list(a = "Group", b = "Group"))
    expect_identical(effective("blocked"), list())
    expect_identical(index[[key("donor")]]$params, list(a = "Other"))
    expect_identical(index[[key("plain")]]$params, list(x = "Earlier"))
})

test_that("stale donor groups mask later donors without supplying a type", {
    lines <- c("#' @param x,y (`Missing`) Neither name is a formal.", "stale <- function(actual) NULL",
        "#' @name common-inputs", "#' @param x,y (`Valid`) A documentation-only group.", "NULL",
        "#' @inheritParams stale", "#' @inheritParams common-inputs", "blocked <- function(x, y) NULL",
        "#' @inheritParams common-inputs", "valid <- function(x, y) NULL")
    f <- provider_fixture(lines)
    index <- f$document$parse_data$member_data$type_hints
    topics <- attr(index, "topics")
    key <- function(name) names(Filter(function(t) identical(t$topic, name), topics))[[1L]]
    expect_identical(topics[[key("stale")]]$groups, list(c("x", "y")))
    expect_null(topics[[key("stale")]]$group_types[[1L]])
    expect_identical(attr(index, "occurrences")$type, "Valid")
    expect_identical(roxygen_effective_params(index[[key("blocked")]], f$workspace, f$document), list())
    valid <- roxygen_effective_params(index[[key("valid")]], f$workspace, f$document)
    expect_identical(lapply(valid, `[[`, "types"), list(x = "Valid", y = "Valid"))
})

test_that("simple package prose records agree with syntax indexing without materializing the syntax tree", {
    uri <- "file:///pkg/R/topics.R"
    lines <- c('text <- r"(begin', "#' @param x Decoy.", 'end)"', "ignored <- function(x) x",
        "#' A topic.", "#' @param x,y One group.", "# ordinary", "#' @param y Another description.",
        "#' @param x,y Final group.", "", "first <- function(x, y = 2L) {", "  # inside the body",
        "  x + y", "}", "{", "  #' A nullary topic.", "  second <- function() NULL", "}",
        "#' An ordinary value.", "v <- 1", "#' After the assignment.", "third <- function(z) z")
    parsed <- parse(text = lines, keep.source = TRUE)
    slow <- member_roxygen_type_index(lines, utils::getParseData(parsed), uri = uri)
    fast <- member_roxygen_type_index(lines, stop("unneeded navigation tree"), uri = uri, parsed = parsed)
    expect_identical(fast, slow)
    records <- attr(fast, "topics")
    expect_identical(vapply(records, `[[`, "", "topic", USE.NAMES = FALSE), c("first", "second", "third"))
    expect_identical(records[[1L]]$groups, list("y", c("x", "y")))
    expect_identical(records[[1L]]$documented, c("x", "y"))
})

test_that("uncommon prose blocks retain complete topic and parameter semantics", {
    for (lines in list(
        c("#' @name shared", "#' @param x Details.", "NULL"),
        c("#' @rdname page", "#' @param", "#' x,y Details.", "f <- function(x, y) NULL"),
        c("#' @param `two words` Details.", "other <- (function(`two words`) NULL)"),
        c("#' Topic.", "#' @noRd", "skip <- function(x) NULL"),
        c("\t#' @param x Details.", "\tf <- function(x) NULL")
    )) {
        parsed <- parse(text = lines, keep.source = TRUE)
        info <- utils::getParseData(parsed)
        expected <- member_roxygen_type_index(lines, info, uri = "file:///pkg/R/topics.R")
        expect_identical(member_roxygen_type_index(lines, info, uri = "file:///pkg/R/topics.R", parsed = parsed), expected)
    }
})
