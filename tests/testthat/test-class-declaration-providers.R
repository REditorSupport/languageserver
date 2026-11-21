class_provider_fixture <- function(lines, uri = "file:///class-providers.R", workspace = Workspace$new(NULL)) {
    document <- Document$new(uri, version = 1L, content = lines)
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, lines)
    parsed$version <- document$version
    workspace$update_parse_data(uri, parsed)
    list(uri = uri, document = document, workspace = workspace)
}

test_that("class outlines and workspace search agree on direct declarations", {
    lines <- c(
        'C <- R6::R6Class("C", public = list(class = 1, box = list(nested = list(phantom = 2)),',
        '    run = function(x) x), private = list(secret = 0), active = list(label = function() "x"))',
        'methods::setClass("Cat", slots = c(label = "character"))',
        'Dog <- S7::new_class("Dog", properties = list(label = S7::class_character))',
        'methods::setGeneric("talk", function(x) standardGeneric("talk"))',
        'methods::setMethod("talk", "Cat", function(x) x@label)')
    f <- class_provider_fixture(lines)
    outline <- function(tree) {
        document_symbol_reply(1L, f$uri, f$workspace, f$document, list(hierarchicalDocumentSymbolSupport = tree))$result
    }
    full <- outline(TRUE)
    expect_equal(map_chr(full, "name"), c("C", "Cat", "Dog", "talk", "talk"))
    expect_equal(map_int(full, "kind"), c(5L, 5L, 5L, 12L, 6L))
    expect_equal(map_chr(full[[1L]]$children, "name"), c("class", "box", "run", "secret", "label"))
    expect_equal(full[[1L]]$children[[5L]]$kind, SymbolKind$Property)
    expect_equal(full[[1L]]$children[[3L]]$kind, SymbolKind$Method)
    expect_equal(map_chr(full[[3L]]$children, "name"), "label")
    for (items in list(outline(FALSE), workspace_symbol_reply(2L, list(f$workspace), "label")$result)) {
        matches <- Filter(function(x) x$name == "label", items)
        expect_equal(map_chr(matches, "containerName"), c("C", "Cat", "Dog"))
    }
    # Comparison of cold disk and full-parse summaries includes physical UTF-16 columns.
    text <- c('\tC <- R6::R6Class("🐈", public=list(label = 1))')
    path <- tempfile(fileext = ".R")
    writeLines(text, path, useBytes = TRUE)
    cold <- index_shallow_summary(path, text, dirname(path))
    full_parse <- parse_document(path_to_uri(path), text)
    warm <- index_shallow_summary(path, text, dirname(path), parse_data = full_parse)
    expect_equal(cold$class_declarations, warm$class_declarations)
    expect_equal(cold$class_declarations[[1L]]$children[[1L]]$selectionRange$start$character, 36L)
})

test_that("member definition preserves the declaring slot, property and inherited initializer", {
    lines <- c('A <- R6::R6Class("A", public=list(initialize=function(x) {}))',
        'C <- R6::R6Class("C", inherit=A)', "o <- C$new(1)",
        'methods::setClass("Base",slots=c(value="integer"))',
        'methods::setClass("Child",contains="Base")', 's <- methods::new("Child",value=9L)', "s@value")
    f <- class_provider_fixture(lines)
    at <- function(row, name) {
        point <- list(row = row - 1L, col = regexpr(name, f$document$content[[row]], fixed = TRUE)[[1L]])
        member_definition_location(f$uri, f$workspace, f$document, point)$result
    }
    expect_equal(at(3L, "new")$range$start, position(0L, 34L))
    expect_equal(at(7L, "value")$range$start, position(3L, 33L))
    skip_if_not_installed("S7")
    f <- class_provider_fixture(c('P <- S7::new_class("P", properties=list(age=S7::class_integer))',
            'C <- S7::new_class("C", parent=P)', "x <- C(age=1L)", "x@age"))
    f$workspace$member_metadata$set("S7", member_index_thaw(member_prepare_package("S7")))
    expect_equal(at(4L, "age")$range$start, position(0L, 40L))
})

test_that("member semantic tokens mark declaration and use roles", {
    f <- class_provider_fixture(c('C <- R6::R6Class("C", public=list(run=function(x) x))',
            "obj$run(1)", "s@field", "obj$field", "f(field = 1)"))
    data <- f$document$parse_data$semantic_data
    token <- function(line, text) {
        rows <- which(data$lines == line - 1L)
        tokens <- substring(f$document$content[[line]], data$cols[rows] + 1L, data$cols[rows] + data$lengths[rows])
        row <- rows[match(text, tokens)]
        c(type = data$types[[row]], modifiers = data$modifiers[[row]])
    }
    expect_equal(unname(token(1L, "run")), c(SemanticTokenTypes$method, semantic_declaration_modifier))
    expect_equal(unname(token(1L, "C")), c(SemanticTokenTypes$class, semantic_declaration_modifier))
    expect_equal(unname(token(2L, "run")), c(SemanticTokenTypes$method, 0L))
    expect_equal(unname(token(3L, "field")), c(SemanticTokenTypes$property, 0L))
    expect_equal(unname(token(4L, "field")), c(SemanticTokenTypes$property, 0L))
    expect_equal(unname(token(5L, "field")), c(SemanticTokenTypes$parameter, 0L))
})

test_that("type hierarchy follows source bindings and exact class names", {
    parent <- class_provider_fixture(c('Base <- R6::R6Class("Parent")', 'S <- S7::new_class("S")'))
    child <- class_provider_fixture(c('Child <- R6::R6Class("Child", inherit=Base)',
            'T <- S7::new_class("T", parent=S)', "ordinary.name <- 1",
            "print.data.frame <- function(x) 1", 'methods::setMethod("process", "Widget", function(x) x)', "o <- 1"),
        "file:///class-child.R", parent$workspace)
    start <- function(row) {
        prepare_type_hierarchy_reply(1L, child$uri, child$workspace, child$document, list(row = row - 1L, col = 0L))$result
    }
    for (row in 1:2) {
        child_item <- start(row)[[1L]]
        base <- type_hierarchy_supertypes_reply(1L, parent$workspace, child_item)$result[[1L]]
        expect_equal(base$uri, parent$uri)
        expect_equal(base$name, c("Parent", "S")[[row]])
        children <- type_hierarchy_subtypes_reply(1L, parent$workspace, base)$result
        expect_equal(children[[1L]]$uri, child$uri)
    }
    expect_null(start(3L))
    expect_equal(start(4L)[[1L]]$name, "data.frame")
    expect_null(start(6L))
})

test_that("class syntax respects masked helpers and XML fallback token roles", {
    f <- class_provider_fixture(c("R6Class <- function(...) list()", 'Fake <- R6Class("Fake", public=list(ghost=1))',
            'Real <- R6::R6Class("Real", active=list(label=function() 1))', "obj$method()"))
    outline <- document_symbol_reply(1L, f$uri, f$workspace, f$document,
        list(hierarchicalDocumentSymbolSupport = TRUE))$result
    expect_equal(outline[[2L]]$kind, SymbolKind$Field)
    expect_length(outline[[2L]]$children, 0L)
    expect_equal(outline[[3L]]$children[[1L]]$name, "label")
    parsed <- f$document$parse_data
    cached <- extract_semantic_tokens(f$uri, f$workspace, f$document)
    fallback <- list(get_parse_data = function(...) list(xml_doc = parse_data_xml(parsed)))
    expect_equal(extract_semantic_tokens(f$uri, fallback, f$document), cached)
})

test_that("same-named generators retain their individual hierarchy edges", {
    lines <- c('First <- R6::R6Class("First")', 'Second <- R6::R6Class("Second")',
        'A <- R6::R6Class("Node", inherit=First)', 'B <- R6::R6Class("Node", inherit=Second)',
        'C <- R6::R6Class("Node", inherit=First)')
    f <- class_provider_fixture(lines)
    item <- function(row) {
        prepare_type_hierarchy_reply(1L, f$uri, f$workspace, f$document, list(row = row, col = 0L))$result[[1L]]
    }
    for (row in 2:4) {
        parent <- type_hierarchy_supertypes_reply(1L, f$workspace, item(row))$result
        expect_equal(map_chr(parent, "name"), if (row == 3L) "Second" else "First")
    }
    children <- type_hierarchy_subtypes_reply(1L, f$workspace, item(0L))$result
    expect_equal(map_chr(children, "name"), c("Node", "Node"))
    expect_equal(vapply(children, function(x) x$selectionRange$start$line, integer(1L)), c(2L, 4L))
})

test_that("class hierarchy follows source dependencies and unopened reverse dependents", {
    root <- index_normalize_path(withr::local_tempdir())
    w <- Workspace$new(root)
    register <- function(file, code) {
        path <- file.path(root, file)
        writeLines(code, path)
        uri <- path_to_uri(path)
        w$index$update_content(uri, code)
        uri
    }
    base <- register("base.R", 'Base <- R6::R6Class("Parent")')
    child <- register("child.R", c('source("base.R")', 'Child <- R6::R6Class("Child",inherit=Base)'))
    register("unrelated.R", c('Base <- R6::R6Class("Other")', 'Child <- R6::R6Class("Child",inherit=Base)'))
    register("ambiguous.R", c('source("base.R")', 'source("unrelated.R")', 'Both <- R6::R6Class("Both",inherit=Base)'))
    base_item <- class_hierarchy_item(w$index$summaries$get(base)$class_declarations[[1L]])
    children <- find_type_subtypes(w, base_item)
    expect_equal(map_chr(children, "uri"), child)
    expect_equal(map_chr(children, "name"), "Child")
    expect_equal(find_type_supertypes(w, children[[1L]]), list(base_item))
})

test_that("S4 registration return bindings are ordinary variables in outlines and search", {
    f <- class_provider_fixture(c('g <- methods::setGeneric("speak", function(x) standardGeneric("speak"))',
            'm <- methods::setMethod("speak", "numeric", function(x) x)'))
    flat <- document_symbol_reply(1L, f$uri, f$workspace, f$document, list())$result
    expect_equal(map_int(flat[map_chr(flat, "name") %in% c("g", "m")], "kind"),
        rep(get_document_symbol_kind("variable"), 2L))
    expect_equal(map_int(flat[map_chr(flat, "name") == "speak"], "kind"), c(SymbolKind$Function, SymbolKind$Method))
    search <- workspace_symbol_reply(1L, list(f$workspace), "speak")$result
    expect_equal(map_int(search, "kind"), c(SymbolKind$Function, SymbolKind$Method))
})

test_that("quoted and escaped declaration helpers agree between open files and the disk index", {
    code <- c('A <- R6::`R6Class`("A", public = list(run = function() 1))',
        'B <- R6::`R6Cl\\141ss`("B", public = list(value = 2))',
        "`R6Cl\\141ss` <- function(...) base::list()", 'ordinary <- R6Class("Ordinary", public = list(ghost = 1))')
    path <- withr::local_tempfile(fileext = ".R")
    writeLines(code, path)
    f <- class_provider_fixture(code, path_to_uri(path))
    parsed <- f$document$parse_data
    classes <- parsed$class_data$declarations
    expect_equal(map_chr(classes, "name"), c("A", "B"))
    expect_equal(parsed$definitions$ordinary$type, "variable")
    expect_equal(index_shallow_summary(path, code, dirname(path))$class_declarations, classes)
    single <- code[[2L]]
    expect_equal(map_chr(index_shallow_summary(path, single, dirname(path))$class_declarations, "name"), "B")
    data <- parsed$semantic_data
    expect_equal(data$types[data$lines == 1L & data$cols == 0L], SemanticTokenTypes$class)
})

test_that("live URI aliases replace indexed hierarchy declarations until their edit is parsed", {
    root <- index_normalize_path(withr::local_tempdir())
    w <- Workspace$new(root)
    path <- file.path(root, "types.R")
    lines <- c('Base <- R6::R6Class("Base")', 'Child <- R6::R6Class("Child",inherit=Base)')
    writeLines(lines, path)
    uri <- path_to_uri(path)
    w$index$update_content(uri, lines)
    initial <- class_hierarchy_item(class_workspace_declarations(w, uri)[[1L]])
    expect_length(find_type_subtypes(w, initial), 1L)
    alias <- sub("types[.]R$", "%74ypes.R", uri)
    doc <- Document$new(alias, version = 1L, content = c("# edited", lines))
    doc$did_open()
    w$documents$set(alias, doc)
    expect_length(class_workspace_declarations(w, uri), 0L)
    expect_length(find_type_subtypes(w, initial), 0L)
    parsed <- parse_document(alias, doc$content)
    parsed$version <- 1L
    w$update_parse_data(alias, parsed)
    classes <- class_workspace_declarations(w)
    expect_equal(map_chr(classes, "uri"), rep(alias, 2L))
    current <- class_hierarchy_item(classes[[1L]])
    expect_equal(find_type_subtypes(w, current)[[1L]]$uri, alias)
    expect_equal(current$range$start$line, 1L)
})

test_that("an ordinary reassignment masks the old parent generator", {
    f <- class_provider_fixture(c('Base <- R6::R6Class("Base")', "Base <- 1",
            'Child <- R6::R6Class("Child", inherit=Base)'))
    classes <- f$document$parse_data$class_data$declarations
    expect_length(find_type_supertypes(f$workspace, class_hierarchy_item(classes[[2L]])), 0L)
    expect_length(find_type_subtypes(f$workspace, class_hierarchy_item(classes[[1L]])), 0L)
})

test_that("hierarchy preparation uses current source declarations without building XML", {
    f <- class_provider_fixture(c('Class <- R6::R6Class("Node")', "x <- Class$new()", "x"))
    testthat::local_mocked_bindings(parse_data_xml = function(...) stop("unexpected XML rebuild"), .package = "languageserver")
    for (row in c(0L, 2L)) {
        item <- prepare_type_hierarchy_reply(1L, f$uri, f$workspace, f$document, list(row = row, col = 0L))$result
        expect_equal(item[[1L]]$range$start, position(0L, 0L))
    }
    f$document$set_content(2L, "different <- 1")
    expect_null(prepare_type_hierarchy_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 1L))$result)
})

test_that("S4 and reference class hierarchy select registered literal contents across generator aliases", {
    for (helper in c("setClass", "setRefClass")) {
        code <- c(paste0("Generator <- methods::", helper, '(r"--[Par🐈ent]--")'),
            paste0("ChildFactory <- methods::", helper, '("Kid", contains = "Par🐈ent")'))
        f <- class_provider_fixture(code)
        classes <- f$document$parse_data$class_data$declarations
        parent <- class_hierarchy_item(classes[[1L]])
        child <- class_hierarchy_item(classes[[2L]])
        selected_text <- function(item) {
            first <- f$document$from_lsp_position(item$range$start)
            last <- f$document$from_lsp_position(item$range$end)
            substr(code[[first$row + 1L]], first$col + 1L, last$col)
        }
        expect_identical(selected_text(parent), "Par🐈ent")
        expect_identical(selected_text(child), "Kid")
        expect_identical(find_type_supertypes(f$workspace, child), list(parent))
        expect_identical(find_type_subtypes(f$workspace, parent), list(child))
    }
})
