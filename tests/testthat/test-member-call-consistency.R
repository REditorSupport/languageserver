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

test_that("S7 constructor arguments use plain insertion until an equals sign is present", {
    skip_if_not_installed("S7", "1.0.0")
    metadata <- list(S7 = member_prepare_package("S7"))
    code <- c("library(S7)", "Range := new_class(",
        "  properties = list(start = class_double, end = class_double),",
        "  validator = function(self) if (self@end < self@start) 'invalid range'",
        ")")
    items <- function(line) member_consistency_probe(code, line, "items", metadata)
    expect_identical(vapply(items("x <- Range(|)"), `[[`, "", "insertText"), c("start = ", "end = "))
    start <- items("x <- Range(st|)")[[1L]]
    end <- items("x <- Range(start = 1, en|)")[[1L]]
    expect_identical(c(start$insertText, end$insertText), c("start = ", "end = "))
    # A JSON null edit makes VS Code discard the entire completion list.
    expect_false("textEdit" %in% names(start))
    expect_false("textEdit" %in% names(end))
    edit <- items("x <- Range(st|art = 1)")[[1L]]$textEdit
    expect_identical(edit, text_edit(range(position(length(code), 11L), position(length(code), 16L)), "start"))
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

test_that("S4 constructor signatures follow the nearest known initializer", {
    code <- c(
        'Top <- methods::setClass("Top", slots = c(value = "numeric"))',
        'Mid <- methods::setClass("Mid", contains = "Top")',
        'Child <- methods::setClass("Child", contains = "Mid")',
        'methods::setMethod("initialize", "Top", function(.Object, top = 7L) .Object)',
        'methods::setMethod("initialize", "Mid", function(.Object, middle = 8L) .Object)'
    )
    signature <- function(lines) member_consistency_probe(lines, "Child(|)", "signature")[[1L]]$label
    expect_identical(signature(code), "Child(middle = 8L)")
    dynamic <- 'methods::setMethod("initialize", choose_class(), function(.Object, dynamic) .Object)'
    expect_identical(signature(c(code, dynamic)), "Child(...)")
    exact <- 'methods::setMethod("initialize", "Child", function(.Object, exact) .Object)'
    expect_identical(signature(c(code, dynamic, exact)), "Child(exact)")
})

test_that("S7 literal property calls complete only a proven property key", {
    skip_if_not_installed("S7")
    snapshot <- list(S7 = member_prepare_package("S7"))
    code <- c('Dog <- S7::new_class("Dog", properties=list(name=S7::class_character))', "d <- Dog()")
    expect_identical(member_consistency_probe(code, 'S7::prop(d, "na|")', "labels", snapshot), "name")
    expect_match(member_consistency_probe(code, 'S7::prop(d, "na|me")', "hover", snapshot)[[1L]], "character")
    expect_length(member_consistency_probe(code, 'S7::prop("na|", "anything")', "labels", snapshot), 0L)
})

test_that("S7 generic signatures and method receivers follow their declarations", {
    skip_if_not_installed("S7")
    snapshot <- list(S7 = member_prepare_package("S7"))
    # Single/multiple dispatch from vignette("S7") and vignette("generics-methods").
    code <- c("library(S7)",
        'Pet <- new_class("Pet", properties = list(name = class_character))',
        'Dog <- new_class("Dog", parent = Pet, properties = list(breed = class_character))',
        'Language <- new_class("Language", properties = list(label = class_character))',
        'speak <- new_generic("speak", c("pet", "language"))',
        "method(speak, list(Dog, Language)) <- function(pet, language) pet@name")
    probe <- function(line, provider, source = code) member_consistency_probe(source, line, provider, snapshot)
    expect_identical(probe("speak(|)", "signature")[[1L]]$label, "speak(pet, language, ...)")
    expect_identical(probe("speak(Dog(), lan|)", "labels"), "language")
    expect_match(probe("spe|ak", "hover")[[1L]], "speak(pet, language, ...)", fixed = TRUE)
    method <- "method(speak, list(Dog, Language)) <- function(pet, language, ...) "
    expect_identical(probe(paste0(method, "pet@|"), "labels"), c("breed", "name"))
    expect_identical(probe(paste0(method, "language@|"), "labels"), "label")
    expect_match(probe(paste0(method, "pet@na|me"), "hover")[[1L]], "character")
    expect_length(probe(paste0(method, "{ pet <- NULL; pet@| }"), "labels"), 0L)
    expect_length(probe(paste0(method, "function(pet) pet@|"), "labels"), 0L)
    masked <- c(code, "`method<-` <- function(generic, signature, value) NULL")
    expect_length(probe(paste0(method, "pet@|"), "labels", masked), 0L)
    expect_length(probe("speak(|)", "signature", c(masked, code[[6L]])), 0L)
    expect_length(probe("speak(Dog(), Language())@|", "labels"), 0L)
    external <- c(code, 'draw <- new_external_generic("optionalpkg", "draw", "item")',
        "method(draw, Pet) <- function(item) item@name")
    expect_identical(probe("method(draw, Dog) <- function(item) item@|", "labels", external), c("breed", "name"))
    expect_length(probe("draw(|)", "signature", external), 0L)
    local <- c("library(S7)", "run <- function() {",
        'Dog <- new_class("Dog", properties = list(breed = class_character))',
        'speak <- new_generic("speak", "dog")',
        "method(speak, Dog) <- function(dog) dog@breed")
    expect_identical(probe("method(speak, list(Dog)) <- function(dog) dog@| }", "labels", local), "breed")
    if (isTRUE(snapshot$S7$s7_capabilities$bind)) {
        # The evolution vignette changes the interface and then renames a generic.
        code <- c(code, 'describe := new_generic("item", function(item, ..., verbose = FALSE) S7_dispatch())',
            "method(describe, Dog) <- function(item, ..., verbose = FALSE) item@name",
            'old_name := deprecated_generic(new = describe, when = "2.0.0")')
        expect_identical(probe("describe(|)", "signature")[[1L]]$label, "describe(item, ..., verbose = FALSE)")
        expect_identical(probe("old_name(|)", "signature")[[1L]]$label, "old_name(item, ..., verbose = FALSE)")
        expect_identical(probe("method(old_name, Dog) <- function(item, ...) item@|", "labels"), c("breed", "name"))

        premature <- c('speak := S7::new_generic("x")', "library(S7)")
        expect_length(probe("speak(|)", "signature", premature), 0L)
        late <- member_generic_index("")
        late$package <- "other"
        late$exports <- ":="
        late$definitions <- list(`:=` = quote(function(...) NULL))
        masks <- c(snapshot, list(other = member_index_freeze(late)))
        shadow <- c("library(S7)", 'before := new_generic("x")', "library(other)", 'after := new_generic("y")')
        expect_identical(member_consistency_probe(shadow, "before(|)", "signature", masks)[[1L]]$label, "before(x, ...)")
        expect_length(member_consistency_probe(shadow, "after(|)", "signature", masks), 0L)
    }
})

test_that("S7 package generics preserve their callable and property interfaces", {
    skip_if_not_installed("S7")
    ns <- new.env(parent = asNamespace("S7"))
    ns$describe <- S7::new_generic("describe", "pet", function(pet, ..., verbose = FALSE) S7::S7_dispatch())
    pkg <- member_index_freeze(member_package_index(member_namespace_input(ns,
                package = "genericsfixture", exports = "describe")))
    snapshots <- list(genericsfixture = pkg, S7 = member_prepare_package("S7"))
    code <- c('Dog <- S7::new_class("Dog", properties = list(name = S7::class_character))',
        "library(genericsfixture)")
    probe <- function(line, provider, source = code) member_consistency_probe(source, line, provider, snapshots)
    expected <- get_signature("describe", member_function_syntax(ns$describe))
    expect_identical(probe("genericsfixture::describe(|)", "signature")[[1L]]$label, expected)
    expect_identical(probe("describe(|)", "signature")[[1L]]$label, expected)
    expect_identical(probe("describe@|", "labels"), sort(names(attr(S7::S7_class(ns$describe), "properties", exact = TRUE))))
    expect_identical(probe("S7::method(describe, Dog) <- function(pet, ...) pet@|", "labels"), "name")
})

test_that("S7 validation and property callbacks see the declared instance types", {
    skip_if_not_installed("S7")
    snapshots <- list(S7 = member_prepare_package("S7"))
    # Range and its dynamic length property from vignette("classes-objects").
    code <- c("library(S7)", 'Range <- new_class("Range", properties = list(start = class_double, end = class_double))')
    probe <- function(line, source = code) member_consistency_probe(source, line, "labels", snapshots)
    expect_identical(probe('Child <- new_class("Child", parent = Range, validator = function(self) self@|)'), c("end", "start"))
    prefix <- 'Child <- new_class("Child", parent = Range, properties = list(window = new_property(Range, '
    expect_identical(probe(paste0(prefix, "getter = function(self) self@|)))")), c("end", "start", "window"))
    expect_identical(probe(paste0(prefix, "setter = function(self, value) value@|)))")), c("end", "start"))
    named_setter <- tryCatch(S7::new_property(setter = function(self, name, value) self), error = function(e) NULL)
    if (!is.null(named_setter)) {
        expect_identical(probe(paste0(prefix, "setter = function(self, name, value) value@|)))")), c("end", "start"))
        expect_length(probe(paste0(prefix, "setter = function(self, name, value) name@|)))")), 0L)
    }
    expect_identical(probe("new_property(Range, validator = function(value) value@|)"), c("end", "start"))
    expect_length(probe("new_property(Range, getter = function(self) self@|)"), 0L)
    expect_length(probe(paste0(prefix, "getter = function(self) (function(self) self@|)(NULL))))")), 0L)
    if (isTRUE(snapshots$S7$s7_capabilities$bind)) {
        expect_identical(probe("Child := new_class(parent = Range, validator = function(self) self@|)"), c("end", "start"))
    }
})
