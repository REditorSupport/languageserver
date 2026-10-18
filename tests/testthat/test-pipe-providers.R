pipe_provider_cursor <- function(content) {
    row <- which(grepl("<cursor>", content, fixed = TRUE))
    col <- regexpr("<cursor>", content[[row]], fixed = TRUE)[[1L]] - 1L
    content[[row]] <- sub("<cursor>", "", content[[row]], fixed = TRUE)
    list(content = content, point = list(row = row - 1L, col = col))
}

test_that("Native pipe member lookup works for complete and incomplete calls", {
    skip_if(getRversion() < "4.1.0", "Native pipes require R 4.1")
    definition <- "obj <- list(run = function(data, option = TRUE) NULL, value = 1)"
    for (call in list("x |> obj$r<cursor>un()", "x |> obj$r<cursor>",
            c("x |>", "  obj$r<cursor>un()"),
            c("x |> obj$", "  r<cursor>un()"),
            "x |> obj$r<cursor>un() |> identity()")) {
        cursor <- pipe_provider_cursor(c(definition, call))
        fixture <- provider_fixture(cursor$content)
        items <- member_completion(fixture$uri, fixture$workspace,
            fixture$document, cursor$point, TRUE, 200L)
        expect_identical(vapply(items, `[[`, character(1L), "label"), "run")
        if (any(grepl("run", gsub("<cursor>", "", call, fixed = TRUE), fixed = TRUE))) {
            hover <- hover_reply(1L, fixture$uri, fixture$workspace,
                fixture$document, cursor$point)$result
            expect_match(paste(hover$contents, collapse = " "), "run\\(data, option = TRUE\\)")
        }
    }
    for (call in c("x |> obj$run(<cursor>)", "x |> obj$run(<cursor>")) {
        cursor <- pipe_provider_cursor(c(definition, call))
        fixture <- provider_fixture(cursor$content)
        signature <- signature_reply(1L, fixture$uri, fixture$workspace,
            fixture$document, cursor$point)$result
        expect_identical(signature$signatures[[1L]]$label, "run(data, option = TRUE)")
        expect_identical(signature$activeParameter, 1L)
    }
    cursor <- pipe_provider_cursor(c(definition, '"x |> obj$r<cursor>"'))
    fixture <- provider_fixture(cursor$content)
    expect_null(member_completion(fixture$uri, fixture$workspace,
            fixture$document, cursor$point, TRUE, 200L))
})

test_that("Class providers match piped class names, inheritance and source ranges", {
    skip_if(getRversion() < "4.2.0", "Native pipe placeholders require R 4.2")
    types <- list(
        S4 = list(fun = "methods::setClass", arg = "Class", parent = 'contains = "Parent"',
            members = 'slots = c(value = "numeric")', supers = find_s4_supertypes, subs = find_s4_subtypes),
        RefClass = list(fun = "methods::setRefClass", arg = "Class", parent = 'contains = "Parent"',
            members = 'fields = list(value = "numeric")', supers = find_refclass_supertypes, subs = find_refclass_subtypes),
        R6 = list(fun = "R6::R6Class", arg = "classname", parent = "inherit = Parent",
            members = "public = list(value = 1)", supers = find_r6_supertypes, subs = find_r6_subtypes)
    )
    for (type in names(types)) {
        spec <- types[[type]]
        for (pipe in c("|>", "%>%")) {
            placeholder <- if (pipe == "|>") "_" else "."
            cases <- c(
                sprintf('"Ch<cursor>ild" %s %s(%s, %s)', pipe, spec$fun, spec$parent, spec$members),
                sprintf('"Ch<cursor>ild" %s %s(%s = %s, %s, %s)',
                    pipe, spec$fun, spec$arg, placeholder, spec$parent, spec$members))
            for (case in cases) {
                cursor <- pipe_provider_cursor(paste0("Child <- ", case))
                fixture <- provider_fixture(cursor$content)
                xdoc <- fixture$document$parse_data$xml_doc
                prepared <- prepare_type_hierarchy_reply(1L, fixture$uri, fixture$workspace,
                    fixture$document, cursor$point)$result
                expect_equal(prepared[[1L]]$name, "Child", info = case)
                expect_equal(prepared[[1L]]$data$classType, type)
                supers <- spec$supers(fixture$document, xdoc, "Child", fixture$uri)
                subs <- spec$subs(fixture$document, xdoc, "Parent", fixture$uri)
                expect_equal(vapply(supers, `[[`, character(1L), "name"), "Parent", info = case)
                expect_equal(vapply(subs, `[[`, character(1L), "name"), "Child", info = case)
                cached <- fixture$document$parse_data$class_data$supertypes[[class_index_key(type, "Child")]]
                expect_equal(vapply(cached, `[[`, character(1L), "name"), "Parent")
                expect_equal(subs[[1L]]$range$start$character,
                    regexpr('"Child"', cursor$content, fixed = TRUE)[[1L]])
                members <- extract_class_members(fixture$document, xdoc, list(name = "Child", type = type))
                expect_true("value" %in% vapply(members, `[[`, character(1L), "name"))
            }
        }
    }
    for (pipe in c("|>", "%>%")) {
        placeholder <- if (pipe == "|>") "_" else "."
        fixture <- provider_fixture(sprintf('"Parent" %s methods::setClass("Child", contains = %s)', pipe, placeholder))
        supers <- find_s4_supertypes(fixture$document, fixture$document$parse_data$xml_doc, "Child", fixture$uri)
        expect_identical(supers[[1L]]$name, "Parent")
        expect_equal(supers[[1L]]$range$start$character, 1L)
    }
})

test_that("Bare magrittr stages are calls and preserve namespace and binding identity", {
    content <- c("target <- function(x) x", "x %>% target", "x %<>% target", "x %T>% target",
        "x %>% stats::median", "x %>% stats:::median.default", "x %$% target",
        "x %>% { target }", "x %>% list(target)", "x %>% .")
    fixture <- provider_fixture(content)
    index <- fixture$document$parse_data$reference_index
    called <- which(index$token == "SYMBOL_FUNCTION_CALL")
    expect_equal(index$name[called], c("target", "target", "target", "median", "median.default", "list"))
    expect_equal(index$line[called], c(1L, 2L, 3L, 4L, 5L, 8L))
    expect_equal(index$call_package[called], c("", "", "", "stats", "stats", ""))
    fixture <- provider_fixture(c("target <- function(x) x", "outer <- function(target) { x %>% target }"))
    index <- fixture$document$parse_data$reference_index
    called <- which(index$token == "SYMBOL_FUNCTION_CALL")
    expect_length(called, 1L)
    expect_true(startsWith(index$definition_key[called], "local:"))
})

test_that("Installed pipe providers return members, class hierarchy and bare calls", {
    skip_on_cran()
    skip_if(getRversion() < "4.2.0", "Native pipe placeholders require R 4.2")
    client <- language_client(working_dir = withr::local_tempdir())
    path <- withr::local_tempfile(fileext = ".R")
    code <- c("obj <- list(run = function(data, option = TRUE) NULL)", "x |> obj$run()",
        'methods::setClass("Parent")', '"Child" |> methods::setClass(contains = "Parent")',
        "target <- function(x) x", "caller <- function(x) x %>% target")
    did_open(client, path, text = code)
    completion <- respond_completion(client, path, c(1L, 10L), retry = FALSE)
    expect_true(any(vapply(completion$items, function(item) identical(item$data$type, "member") && item$label == "run", logical(1L))))
    expect_match(respond_hover(client, path, c(1L, 10L), retry = FALSE)$contents[[1L]], "run\\(data")
    expect_equal(respond_signature(client, path, c(1L, 13L), retry = FALSE)$activeParameter, 1L)
    child <- respond_prepare_type_hierarchy(client, path, c(3L, 3L), retry = FALSE)[[1L]]
    expect_equal(child$name, "Child")
    expect_equal(respond_type_hierarchy_supertypes(client, child, retry = FALSE)[[1L]]$name, "Parent")
    caller <- respond_prepare_call_hierarchy(client, path, c(5L, 1L), retry = FALSE)[[1L]]
    outgoing <- respond_call_hierarchy_outgoing_calls(client, caller, retry = FALSE)
    expect_equal(outgoing[[1L]]$to$name, "target")
    target <- respond_prepare_call_hierarchy(client, path, c(4L, 1L), retry = FALSE)[[1L]]
    incoming <- respond_call_hierarchy_incoming_calls(client, target, retry = FALSE)
    expect_equal(incoming[[1L]]$from$name, "caller")
    lenses <- respond(client, "textDocument/codeLens", list(textDocument = list(uri = path_to_uri(path))), retry = FALSE)
    lens <- Filter(function(item) identical(item$data$symbol, "target"), lenses)[[1L]]
    expect_equal(respond(client, "codeLens/resolve", lens, retry = FALSE)$command$title, "1 call")
})
