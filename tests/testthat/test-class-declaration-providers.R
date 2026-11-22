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

test_that("S7 vignette bindings, dispatch registrations and inherited properties keep their source locations", {
    skip_if_not_installed("S7", "1.0.0")
    root <- index_normalize_path(withr::local_tempdir())
    workspace <- Workspace$new(root)
    fixture <- function(name, code) {
        path <- file.path(root, name)
        writeLines(code, path)
        f <- class_provider_fixture(code, path_to_uri(path), workspace)
        workspace$index$update_content(f$uri, code, parse_data = f$document$parse_data)
        f
    }
    parent <- fixture("pet.R", c("library(S7)", "Pet := new_class(properties = list(name = class_character))"))
    child <- fixture("dog.R", c('source("pet.R")', "library(S7)", "Dog := new_class(Pet)",
            'speak := new_generic("x")', "method(speak, list(Dog)) <- function(x) x@name",
            'dog <- Dog(name = "Rex")', "speak(dog)", "dog@name"))
    workspace$member_metadata$set("S7", member_index_thaw(member_prepare_package("S7")))
    point <- function(line, name) list(row = line - 1L, col = regexpr(name, child$document$content[[line]], fixed = TRUE)[[1L]])
    outline <- document_symbol_reply(1L, child$uri, workspace, child$document,
        list(hierarchicalDocumentSymbolSupport = TRUE))$result
    expect_equal(map_chr(outline, "name"), c("Dog", "speak", "speak", "dog"))
    expect_equal(map_int(outline, "kind"), c(SymbolKind$Class, SymbolKind$Function, SymbolKind$Method, SymbolKind$Field))
    search <- workspace_symbol_reply(1L, list(workspace), "speak")$result
    expect_equal(map_int(search, "kind"), c(SymbolKind$Function, SymbolKind$Method))
    expect_null(workspace$get_namespace(WORKSPACE, uri = child$uri)$get_formals("speak"))
    current <- class_source_records(child$document$parse_data)
    shallow <- index_shallow_summary(path_from_uri(child$uri), child$document$content, root)
    expect_equal(shallow$class_declarations, current)
    expect_equal(shallow$definitions, child$document$parse_data$definitions)
    expect_equal(find_type_supertypes(workspace, class_hierarchy_item(current[[1L]]))[[1L]]$uri, parent$uri)
    expect_equal(type_definition_reply(1L, child$uri, workspace, child$document, point(8L, "dog"))$result$range,
        current[[1L]]$selectionRange)
    refs <- function(line, name) references_reply(1L, child$uri, workspace, child$document, point(line, name))$result
    expect_equal(map_int(lapply(refs(7L, "speak"), function(x) x$range$start), "line"), c(3L, 4L, 6L))
    property <- parent$document$parse_data$class_data$declarations[[1L]]$children[[1L]]
    expect_equal(definition_reply(1L, child$uri, workspace, child$document, point(8L, "name"))$result,
        list(uri = parent$uri, range = property$selectionRange))
    edits <- rename_reply(1L, child$uri, workspace, child$document, point(8L, "name"), "label")$result$changes
    expect_equal(edits[[parent$uri]][[1L]]$range, property$selectionRange)
    expect_equal(map_int(lapply(edits[[child$uri]], function(x) x$range$start), "line"), c(4L, 5L, 7L))
    expect_equal(map_chr(edits[[child$uri]], "newText"), rep("label", 3L))
})

test_that("S7 named declarations verify helper ownership and preserve ordinary reassignments", {
    skip_if_not_installed("S7", "1.0.0")
    code <- c('Ordinary <- new_class("Ordinary")', "NoImport := S7::new_class()", "library(S7, quietly = TRUE)",
        "local({ Private := new_class() })", 'Duplicate := new_class(name = "Other")',
        'g <- S7::new_generic("g", "x")', "method(g, class_character) <- NULL",
        "method(g, class_character)", "x := new_class()", "x <- 1L",
        "ordinary <- 1L", "ordinary := new_class()", "`method<-` <- function(...) NULL",
        "method(g, class_character) <- function(x) x", "`:=` <- function(...) NULL",
        "Unrelated := S7::new_class()", "obj[entry := S7::new_class()]")
    f <- class_provider_fixture(code)
    records <- f$document$parse_data$class_data$declarations
    expect_equal(map_chr(records, "name"), c("g", "x", "ordinary"))
    expect_equal(f$document$parse_data$definitions$x$type, "integer")
    expect_equal(f$document$parse_data$definitions$ordinary$type, "S7")
    expect_equal(map_chr(document_symbol_reply(1L, f$uri, f$workspace, f$document,
                list(hierarchicalDocumentSymbolSupport = TRUE))$result, "name"),
        c("Ordinary", "g", "x", "ordinary", "method<-", ":="))
})

test_that("the shallow package index respects S7 NAMESPACE imports and exclusions", {
    skip_if_not_installed("S7", "1.0.0")
    package <- index_normalize_path(withr::local_tempdir())
    dir.create(file.path(package, "R"))
    writeLines("Package: shapes", file.path(package, "DESCRIPTION"))
    namespace <- file.path(package, "NAMESPACE")
    writeLines(c("import(S7)", 'import(rlang, except = ":=")'), namespace)
    path <- file.path(package, "R", "api.R")
    code <- c("Shape := new_class()", 'area := new_generic("x")', "method(area, Shape) <- function(x) 0")
    writeLines(code, path)
    summary <- index_shallow_summary(path, code, package)
    expect_equal(map_chr(summary$class_declarations, "name"), c("Shape", "area", "area"))
    expect_true(index_source_candidates_current(summary))
    writeLines('importFrom(S7, new_class, new_generic, "method<-")', namespace)
    expect_false(index_source_candidates_current(summary))
    latest <- index_shallow_summary(path, code, package)
    expect_equal(map_chr(latest$class_declarations, "type"), "S7Method")
    writeLines('importFrom(S7, ":=", new_class, new_generic)', namespace)
    expect_equal(map_chr(index_shallow_summary(path, code, package)$class_declarations, "name"), c("Shape", "area"))
})

test_that("S7 evolution examples retain class properties and generic outlines", {
    skip_if_not_installed("S7", "1.0.0")
    code <- c("library(S7)", 'Retired := deprecated_class(properties = list(size = class_double), when = "2.0.0")',
        'Old := deprecated_class(new = Retired, when = "2.0.0", alias = TRUE)',
        'measure := new_generic("x")', 'old_measure := deprecated_generic(new = measure, when = "1.1.0")')
    f <- class_provider_fixture(code)
    d <- f$document$parse_data$class_data$declarations
    expect_equal(map_chr(d, "name"), c("Retired", "Old", "measure", "old_measure"))
    expect_equal(map_chr(d[[1L]]$children, "name"), "size")
    expect_equal(map_int(d, "kind"), c(SymbolKind$Class, SymbolKind$Function, SymbolKind$Function, SymbolKind$Function))
    expect_equal(d[[2L]]$type, "S7Alias")
})

test_that("source named binding follows the installed S7 export", {
    skip_if_not_installed("S7")
    has_bind <- ":=" %in% getNamespaceExports("S7")
    f <- class_provider_fixture(c("library(S7)", "Named := new_class()"))
    expect_identical("Named" %in% names(f$document$parse_data$definitions), has_bind)
})

test_that("unconditional brace blocks preserve S7 attachment order in both indexes", {
    skip_if_not_installed("S7", "1.0.0")
    root <- withr::local_tempdir()
    path <- file.path(root, "blocks.R")
    lines <- c(
        "if (enabled) { library(S7) }", "f <- function() { library(S7) }",
        "Before := new_class()",
        "{", "  library(S7)", "  Point := new_class()", "}",
        "if (enabled) { detach('package:S7') }", "After := new_class()",
        "{ { detach('package:S7') } }", "Detached := S7::new_class()",
        "{ base::require(S7, quietly = TRUE) }", "Restored := new_class()",
        "{ attach(list(`:=` = function(...) NULL)); Masked := S7::new_class() }"
    )
    writeLines(lines, path)
    full <- parse_document(path_to_uri(path), lines)
    shallow <- index_shallow_summary(path, lines, root)
    expected <- c("Point", "After", "Restored")
    for (records in list(full$class_data$declarations, shallow$class_declarations)) {
        expect_identical(map_chr(records, "name"), expected)
        expect_identical(map_chr(records, "type"), rep("S7", length(expected)))
    }
    for (definitions in list(full$definitions, shallow$definitions)) {
        expect_true(all(expected %in% names(definitions)))
        expect_false(any(c("Before", "Detached", "Masked") %in% names(definitions)))
    }
})

test_that("S7's observed attachment exclusions retain named declarations in either attachment order", {
    skip_if_not_installed("S7", "1.0.0")
    skip_if_not_installed("rlang")
    collect <- function(lines, rules) {
        parsed <- parse(text = lines, keep.source = TRUE)
        syntax <- syntax_parse_data(utils::getParseData(parsed))
        class_source_declarations(syntax, lines, "file:///mixed.R", attachment_exclusions = rules)
    }
    rules <- list(rlang = ":=")
    docs <- c("library(S7)", "library(rlang)", "Dog := new_class()")
    expect_length(collect(docs, NULL), 0L)
    classes <- collect(docs, rules)
    expect_equal(map_chr(classes, "name"), "Dog")
    expect_true(attr(classes, "attachment_rules"))
    expect_equal(map_chr(collect(docs[c(2L, 1L, 3L)], rules), "name"), "Dog")
    expect_length(collect(docs[-1L], rules), 0L)
    expect_length(collect(c(docs[1:2], "`:=` <- function(...) NULL", docs[[3L]]), rules), 0L)
    nested <- c(docs[[1L]], "{ { library(rlang) } }", docs[[3L]])
    expect_length(collect(nested, NULL), 0L)
    expect_equal(map_chr(collect(nested, rules), "name"), "Dog")
})

test_that("S7 property navigation links direct and named-list updates to their receiver", {
    skip_if_not_installed("S7", "1.0.0")
    lines <- c("library(S7)", "Point := new_class(properties = list(x = class_double))",
        "p <- Point()", "set_props(p, x = 2)", "S7::set_props(p, base::list(x = 3))",
        "Other := new_class(properties = list(x = class_double))",
        "set_props(Other(), x = 4)", "set_props <- function(...) NULL", "set_props(p, x = 5)", "p@x")
    f <- class_provider_fixture(lines)
    f$workspace$member_metadata$set("S7", member_index_thaw(member_prepare_package("S7")))
    point <- function(i) list(row = i - 1L, col = regexpr("x", lines[[i]], fixed = TRUE)[[1L]])
    declaration <- f$document$parse_data$class_data$declarations[[1L]]$children[[1L]]$selectionRange
    for (i in 4:5) {
        definition <- definition_reply(1L, f$uri, f$workspace, f$document, point(i))$result
        expect_equal(definition$range, declaration)
    }
    edits <- rename_reply(1L, f$uri, f$workspace, f$document, point(10L), "height")$result$changes[[f$uri]]
    expect_equal(map_int(lapply(edits, function(x) x$range$start), "line"), c(1L, 3L, 4L, 9L))
    expect_match(rename_reply(1L, f$uri, f$workspace, f$document, point(10L), ".check")$error$message,
        "would match a set_props", fixed = TRUE)
})

test_that("the S7 compatibility hierarchy distinguishes registry names from generator bindings", {
    skip_if_not_installed("S7", "1.0.0")
    parent <- class_provider_fixture(c('Point <- methods::setClass("RegisteredPoint", slots = c(x = "numeric"))',
            'RegisteredPoint <- S7::new_class("Unrelated")'))
    child <- class_provider_fixture(c("library(S7)",
            "Colored := new_class(Point)", 'Scaled := new_class(parent = methods::getClass("RegisteredPoint"))',
            'methods::setClass("Replica", contains = S4_contains(Colored))',
            'methods::setClass("OtherReplica", contains = S7::S4_contains(Colored))',
            'S4_contains <- function(x) "Other"',
            'methods::setClass("Masked", contains = S4_contains(Colored))',
            'methods::setClass("Dynamic", contains = S7::S4_contains(choose()))',
            'Unknown := new_class(parent = methods::getClass("RegisteredPoint", where = elsewhere))'),
        "file:///cross-system-classes.R", parent$workspace)
    nodes <- lapply(child$document$parse_data$class_data$declarations, class_hierarchy_item)
    base <- class_hierarchy_item(parent$document$parse_data$class_data$declarations[[1L]])
    up <- function(item) find_type_supertypes(child$workspace, item)
    down <- function(item) find_type_subtypes(child$workspace, item)
    expect_equal(up(nodes[[1L]]), list(base))
    expect_equal(up(nodes[[2L]]), list(base))
    expect_equal(down(base), nodes[1:2])
    expect_equal(up(nodes[[3L]]), nodes[1L])
    expect_equal(up(nodes[[4L]]), nodes[1L])
    expect_equal(down(nodes[[1L]]), nodes[3:4])
    for (node in nodes[5:7]) expect_length(up(node), 0L)
})

test_that("S7 names arbitrary factories as ordinary workspace bindings", {
    skip_if_not_installed("S7", "1.0.0")
    root <- index_normalize_path(withr::local_tempdir())
    dir.create(file.path(root, "R"))
    writeLines(c("Package: factoryfixture", "Version: 0.0.1", "Imports: S7"), file.path(root, "DESCRIPTION"))
    writeLines("import(S7)", file.path(root, "NAMESPACE"))
    workspace <- Workspace$new(root)
    workspace$member_metadata$set("S7", member_index_thaw(member_prepare_package("S7")))
    path <- file.path(root, "R", "factory.R")
    code <- c("make <- function(name) list(label = name)", "widget := make()",
        "{ boxed := list() }", "object[hidden := make()]", "if (flag) { conditional := make() }",
        'duplicate := make(name = "explicit")', "atomic := 1", "widget_again := make()", "widget_again <- 1L",
        "`:=` <- function(lhs, rhs) NULL", "masked := make()")
    writeLines(code, path)
    source <- class_provider_fixture(code, path_to_uri(path), workspace)
    cached <- index_shallow_summary(path, code, root)
    live <- source$document$parse_data$definitions
    expect_equal(cached$definitions, live)
    expect_equal(live$widget$type, "variable")
    expect_equal(live$boxed$type, "variable")
    expect_equal(live$widget_again$type, "integer")
    expect_false(any(c("hidden", "conditional", "duplicate", "atomic", "masked") %in% names(live)))
    workspace$index$set_summary(cached)
    peer <- file.path(root, "R", "use.R")
    use <- c("widget", "widget$label")
    writeLines(use, peer)
    user <- class_provider_fixture(use, path_to_uri(peer), workspace)
    symbol <- workspace_symbol_reply(1L, list(workspace), "widget")$result
    symbol <- Filter(function(x) x$name == "widget", symbol)
    expect_equal(symbol[[1L]]$location$range, live$widget$range)
    expect_equal(symbol[[1L]]$kind, get_document_symbol_kind("variable"))
    outline <- document_symbol_reply(1L, source$uri, workspace, source$document, list(hierarchicalDocumentSymbolSupport = TRUE))$result
    expect_true("widget" %in% map_chr(outline, "name"))
    target <- definition_reply(1L, user$uri, workspace, user$document, list(row = 0L, col = 2L))$result
    expect_equal(target$uri, source$uri)
    expect_equal(target$range, live$widget$range)
    items <- member_completion(user$uri, workspace, user$document, list(row = 1L, col = 7L), TRUE, 200L)
    expect_identical(map_chr(items, "label"), "label")
})
