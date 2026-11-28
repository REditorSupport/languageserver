test_that("leading type spellings distinguish explicit and class-validated alternatives", {
    hint <- function(text) roxygen_type_block(paste("#' @returns", text))$returns
    linked <- function(types, links = types) structure(types, roxygen_class_links = links)
    expect_identical(hint("(`pkg::Box | Panel`) Result."), c("pkg::Box", "Panel"))
    expect_identical(hint("[\\code{pkg::Box | Panel}]\\cr Result."), c("pkg::Box", "Panel"))
    expect_identical(hint("\\[`function`]\\cr A transformer."), "function")
    expect_identical(hint("[\\code{any}]\\cr An object."), linked("any"))
    expect_identical(hint("\\[`listish`] List-like input."), linked("listish"))
    expect_identical(hint("[\\code{atomic | pkg::any}] Input."), linked(c("atomic", "pkg::any"), "atomic"))
    expect_identical(hint("(`expressions`) A nominal class."), "expressions")
    expect_identical(hint("[\\code{any}] | (`any`) Value."), "any")
    expect_identical(hint("[`pkg::Box | Panel`] Result."), linked(c("pkg::Box", "Panel")))
    expect_identical(hint("[`Box`] or [`pkg::Panel`]."), linked(c("Box", "pkg::Panel")))
    expect_identical(hint("[\\code{Box}] | [`pkg::Panel`]: Result."), linked(c("Box", "pkg::Panel"), "pkg::Panel"))
    expect_identical(hint("[`Box`] | (`Box | Panel`), returned."), c("Box", "Panel"))
    expect_identical(hint("An object of class [hms]."), linked("hms"))
    expect_identical(hint("An object of class [`pkg::Box`] or [hms]."), linked(c("pkg::Box", "hms")))
    expect_identical(hint("An object of class `pkg::Box`, generally."), linked("pkg::Box"))
    expect_identical(hint("An object of class \\code{Box} or `Panel`."), linked(c("Box", "Panel")))
    expect_identical(hint("An object of class `cert`, `key` or `pubkey` which holds the data"), linked(c("cert", "key", "pubkey")))
    expect_identical(hint("[`A`], [`B`], or [`C`] results."), linked(c("A", "B", "C")))
    expect_identical(hint("The [`MockShinySession`] object to use as the [reactive domain]."), linked("MockShinySession"))
    expect_identical(hint("A [`Box`] object or a [`Panel`] object."), linked(c("Box", "Panel")))
})

test_that("prose, ordinary links and incomplete alternatives are not type declarations", {
    rejected <- c("[hms] A help link.", "`[Box]` Other notation.", "`Box` is mentioned.", "Uses [`Box`].",
        "An object of class orange.", 'An object of class "Box".', "An object of classes [hms].",
        "A [hms] object.", "The [`MockShinySession`] application.", "An object of class [hms()]",
        "[`mean()`] A help topic.", "[`mean`][alias] A link.", "[`Box`](https://example.org) Linked.",
        "An object of class [hms](target).", "A [`Box`][alias] object.", "[`Box`] (target) Other.", "[`Box`] [alias] Other.",
        "[`Box`]: <https://example.org>", "[`Box`]suffix", "[`Box`].suffix", "[\\code{Box}]\\crate", "[\\code{character(1)}]\\cr Size.",
        "\\[`character(1)`: default] Optional.", "[`A | `] Incomplete.", "[`A`] or B", "[`A`] or", "[`A`] |", "[`A`] || [`B`]",
        "[`A`] or [`B()`]", "[`A`] | [`B`](target)", "[`A`] or [`B`][elsewhere]", "[`A`] or [`B`] or [topic]",
        "[`A`], [`B`],", "[`A`] \\[`B`]", "An object of class `glht`, `confint.glht`, `summary.glht` or [multcomp::cld()]",
        "The [`A`] object or other objects.")
    for (text in rejected) expect_null(roxygen_type_block(paste("#' @return", text)), info = text)
    too_many <- paste0("[`Type", seq_len(max_type_hint_alternatives + 1L), "`]", collapse = " | ")
    expect_null(roxygen_type_block(paste("#' @return", too_many)))
})

test_that("each marker preserves physical type and parameter ranges", {
    lines <- c("\t#' @param a,b", "\t#'   [`pkg::Π`]", "\t#'   or [\\code{Ψ | Ω}] Input.",
        "\t#' @param `😀 count` An object of", "\t#'   class [pkg::Clazz] with a class attribute.",
        "\t#' @returns", "\t#'   The [`Ω`]", "\t#'   object to use.")
    marker <- as.integer(regexpr("#'", lines, fixed = TRUE))
    tags <- roxygen_type_block(substring(lines, marker), c("a", "b", "😀 count"),
        rows = seq_along(lines) - 1L, columns = marker - 1L, collect_groups = TRUE)
    expect_identical(tags$params$a, structure(c("pkg::Π", "Ψ", "Ω"), roxygen_class_links = "pkg::Π"))
    expect_identical(tags$params$b, tags$params$a)
    expect_identical(tags$params[["😀 count"]], structure("pkg::Clazz", roxygen_class_links = "pkg::Clazz"))
    expect_identical(tags$returns, structure("Ω", roxygen_class_links = "Ω"))
    sites <- attr(tags, "occurrences", exact = TRUE)
    expect_identical(sites$type, c("pkg::Π", "Ψ", "Ω", "pkg::Clazz", "Ω"))
    expect_identical(sites$row, c(1L, 2L, 2L, 4L, 6L))
    expect_identical(substr(lines[sites$row + 1L], sites$start + 1L, sites$end), c("Π", "Ψ", "Ω", "Clazz", "Ω"))
    expect_identical(sites$parameter_row, c(0L, 0L, 0L, 3L, NA_integer_))
    expect_identical(substr(lines[4L], sites$parameter_start[[4L]] + 1L,
            sites$parameter_start[[4L]] + nchar("`😀 count`")), "`😀 count`")
})

test_that("ordinary links cannot replace explicit contracts on a repeated parameter or return", {
    blocks <- c("#' @param x,y (`Explicit`) Inputs.", "#' @param x,y [`Topic`] Related topic.",
        "#' @returns [\\code{Explicit}] Output.", "#' @returns An object of class `Prose`.")
    hints <- roxygen_type_block(blocks, collect_groups = TRUE)
    expect_identical(hints$params, list(x = "Explicit", y = "Explicit"))
    expect_identical(hints$returns, "Explicit")
    expect_identical(unname(attr(hints, "group_types")), list("Explicit", "Explicit"))
})
