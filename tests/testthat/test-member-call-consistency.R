member_consistency_probe <- function(code, line, provider, snapshots = list()) {
    cursor <- regexpr("|", line, fixed = TRUE)[[1L]]
    stopifnot(cursor > 0L)
    line <- paste0(substr(line, 1L, cursor - 1L), substring(line, cursor + 1L))
    fixture <- provider_fixture(c(code, line))
    document <- fixture$document
    workspace <- fixture$workspace
    workspace$loaded_packages <- character()
    workspace$imported_objects <- collections::dict()
    workspace$get_namespace <- workspace$guess_namespace <- workspace$get_documentation <-
        workspace$get_signature <- workspace$get_help <- workspace$get_definition <- function(...) NULL
    workspace$member_metadata <- collections::dict()
    for (package in names(snapshots)) workspace$member_metadata$set(package, member_index_thaw(snapshots[[package]]))
    point <- list(row = length(code), col = cursor - 1L)
    uri <- fixture$uri
    if (provider == "hover") return(hover_reply(1L, uri, workspace, document, point)$result$contents)
    if (provider == "signature") return(signature_reply(1L, uri, workspace, document, point)$result$signatures)
    items <- completion_reply(1L, uri, workspace, document, point, list())$result$items
    items <- Filter(function(item) isTRUE(item$data$type %in% c("member", "member_value")), items)
    if (provider == "labels") return(vapply(items, `[[`, character(1L), "label"))
    if (provider == "resolve") return(lapply(items, function(item) {
        completion_item_resolve_reply(1L, workspace, item, list())$result
    }))
    items
}

test_that("member completion matches supplied arguments and the active default", {
    code <- 'obj <- list(draw = function(x, mode=c("fast","slow"), ..., flag=TRUE) NULL)'
    labels <- function(line) member_consistency_probe(code, line, "labels")
    expect_identical(labels("obj$draw(x=1, |)"), c("mode", "flag", "fast", "slow"))
    expect_identical(labels("obj$draw(1, |, flag=FALSE)"), c("mode", "fast", "slow"))
    expect_identical(labels("obj$draw(mo='fast', |)"), c("x", "flag"))
    expect_identical(labels("obj$draw(x=1, mode=|)"), c("fast", "slow"))
    item <- member_consistency_probe(code, 'obj$draw(mode="f|ake")', "items")[[1L]]
    expect_identical(item$label, "fast")
    expect_identical(item$textEdit$newText, "fast")
    expect_equal(item$textEdit$range$end$character - item$textEdit$range$start$character, 4L)
    expect_identical(labels("obj$draw(flag='f|')"), character())
    expect_identical(labels("obj$draw(m|)"), "mode")
    tag <- member_consistency_probe(code, "obj$draw(m|ode = 'fast')", "items")
    tag <- Filter(function(item) item$label == "mode", tag)
    expect_identical(tag[[1L]]$textEdit$newText, "mode")
    expect_equal(tag[[1L]]$textEdit$range$end$character - tag[[1L]]$textEdit$range$start$character, 4L)
    expect_identical(labels("obj$draw(1, |, 'slow')"), c("mode", "flag", "fast", "slow"))
})

test_that("documented callables preserve signatures, spelling and help across extraction", {
    code <- c("#' Draw a picture.", "#' @param mode Choose the speed.",
        "draw <- function(mode=c('fast','slow')) NULL", 'obj <- list("two words"=draw)')
    for (spelling in c('obj$"two words"', "obj$'two words'", 'obj[["two words"]]', "obj$`two words`")) {
        alias <- c(code, paste("f <-", spelling))
        expect_identical(member_consistency_probe(alias, "f(|)", "signature")[[1L]]$label,
            'f(mode = c("fast", "slow"))')
        expect_identical(member_consistency_probe(alias, paste0(spelling, "(|)"), "signature")[[1L]]$documentation$value,
            "Draw a picture.")
        hover <- member_consistency_probe(alias, paste0(spelling, "(m|ode='fast')"), "hover")
        expect_match(paste(hover, collapse = " "), "Choose the speed.", fixed = TRUE)
        items <- member_consistency_probe(alias, "f(m|)", "resolve")
        expect_identical(items[[1L]]$documentation$value, "Choose the speed.")
    }
    item <- member_consistency_probe(code, 'obj$"t|wo words"', "resolve")[[1L]]
    expect_identical(item$textEdit$newText, '"two words"')
    expect_identical(item$documentation$value, "Draw a picture.")
    expect_length(member_consistency_probe(c(code, "f <- obj$'two words'", "f <- 1"), "f(|)", "signature"), 0L)
})

test_that("reference class instances inherit declarations and initialize signatures", {
    code <- c('A <- methods::setRefClass("Parent", fields = c(count="numeric"),',
        "   methods = list(bump=function(step=1) NULL, initialize=function(start=0) NULL))",
        'Child <- setRefClass("Child", contains="Parent", fields=list(label="character"))',
        "object <- Child$new()")
    expect_setequal(member_consistency_probe(code, "object$|", "labels"), c("count", "label", "initialize", "bump"))
    expect_identical(member_consistency_probe(code, "object$bump(|)", "signature")[[1L]]$label, "bump(step = 1)")
    expect_identical(member_consistency_probe(code, "Child$new(|)", "signature")[[1L]]$label, "new(start = 0)")
    expect_identical(member_consistency_probe(code, "Child(|)", "signature")[[1L]]$label, "Child(start = 0)")
    expect_match(member_consistency_probe(code, "object$cou|nt", "hover")[[1L]], "numeric")
    masked <- c("setRefClass <- function(...) NULL", 'Bad <- setRefClass("Bad", fields=c(f="integer"))')
    expect_length(member_consistency_probe(masked, "Bad$new()$|", "labels"), 0L)
    plain <- 'Plain <- setRefClass("Plain", fields=c(count="numeric"))'
    expect_identical(member_consistency_probe(plain, "Plain$new(co|)", "labels"), "count")
})

test_that("S4 constructors, method formals and literal keys use declared slots", {
    code <- c('W <- methods::setClass("W", slots=c(shade="character"))', "w <- W()")
    expect_identical(member_consistency_probe(code, "W(sh|)", "labels"), "shade")
    expect_identical(member_consistency_probe(code, 'methods::new("W", |', "labels"), "shade")
    expect_identical(member_consistency_probe(code, "W(shade='ok', |)", "labels"), character())
    expect_identical(member_consistency_probe(code, 'methods::new("W", |)', "signature")[[1L]]$label, "new(Class, ...)")
    for (access in c("methods::slot(w, 'sh|ade')", "slot(name='sh|ade', object=w)")) {
        item <- member_consistency_probe(code, access, "items")[[1L]]
        expect_identical(item$textEdit$newText, "'shade'")
        expect_match(member_consistency_probe(code, access, "hover")[[1L]], "character")
    }
    expect_length(member_consistency_probe(c(code, "slot <- function(...) NULL"), "slot(w,'sh|')", "labels"), 0L)
    expect_length(member_consistency_probe(code, "methods::slot('sh|', 'shade')", "labels"), 0L)
    method <- 'methods::setMethod("show", "W", function(object) object@sh|ade)'
    expect_match(member_consistency_probe(code, method, "hover")[[1L]], "character")
    method <- 'setMethod("g", signature(y="W"), function(x, y) y@sh|ade)'
    expect_identical(member_consistency_probe(code, method, "labels"), "shade")
    expect_length(member_consistency_probe(code, sub("y@", "x@", method), "labels"), 0L)
    init <- c(code, 'setMethod("initialize", "W", function(.Object, seed, strict=FALSE) .Object)')
    expect_identical(member_consistency_probe(init, "W(|)", "signature")[[1L]]$label, "W(seed, strict = FALSE)")
    expect_identical(member_consistency_probe(init, 'new("W", |)', "labels"), c("seed", "strict"))
})

test_that("S7 literal property calls complete only a proven property key", {
    skip_if_not_installed("S7")
    snapshot <- list(S7 = member_prepare_package("S7"))
    code <- c('Dog <- S7::new_class("Dog", properties=list(name=S7::class_character))', "d <- Dog()")
    expect_identical(member_consistency_probe(code, 'S7::prop(d, "na|")', "labels", snapshot), "name")
    expect_match(member_consistency_probe(code, 'S7::prop(d, "na|me")', "hover", snapshot)[[1L]], "character")
    expect_length(member_consistency_probe(code, 'S7::prop("na|", "anything")', "labels", snapshot), 0L)
})
