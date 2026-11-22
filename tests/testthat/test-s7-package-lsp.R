test_that("S7 method completion loads the package's unopened class and generic over LSP", {
    skip_on_cran()
    skip_if_not_installed("S7")
    root <- index_normalize_path(withr::local_tempdir())
    dir.create(file.path(root, "R"))
    writeLines(c("Package: petfixture", "Version: 0.0.1", "Imports: S7",
            "Collate: 'classes.R' 'generics.R' 'methods.R'"), file.path(root, "DESCRIPTION"))
    writeLines('importFrom(S7, new_class, class_character, new_generic, "method<-")', file.path(root, "NAMESPACE"))
    writeLines('Pet <- new_class("Pet", properties = list(name = class_character))', file.path(root, "R", "classes.R"))
    writeLines('describe <- new_generic("describe", "pet")', file.path(root, "R", "generics.R"))
    method <- "method(describe, Pet) <- function(pet) pet@name"
    path <- file.path(root, "R", "methods.R")
    writeLines(c(method, "describe(Pet())"), path)

    client <- language_client(working_dir = root,
        lib_paths = unique(dirname(find.package(c("languageserver", "S7")))))
    did_open(client, path)
    items <- respond_completion(client, path, c(0L, nchar(method) - 4L),
        retry_when = function(result) !"name" %in% vapply(result$items, `[[`, character(1L), "label"))$items
    expect_identical(vapply(items, `[[`, character(1L), "label"), "name")
    signature <- respond_signature(client, path, c(1L, 9L),
        retry_when = function(result) !length(result$signatures))
    expect_identical(signature$signatures[[1L]]$label, "describe(pet, ...)")
})

test_that("package completion exposes and documents the installed S7 hooks", {
    skip_on_cran()
    skip_if_not_installed("S7", "1.0.0")
    root <- withr::local_tempdir()
    file <- file.path(root, "zzz.R")
    writeLines(c(".onLoad <- function(...) S7::S7_on_load()",
        ".onUnload <- function(...) S7::S7_on_unload()", "S7::S7_on_build()"), file)
    client <- language_client(working_dir = root,
        lib_paths = unique(dirname(find.package(c("languageserver", "S7")))))
    did_open(client, file)
    hooks <- getNamespaceExports("S7")
    hooks <- hooks[startsWith(hooks, "S7_on_")]
    items <- respond_completion(client, file, c(2L, 10L), retry_when = function(result) {
        !all(hooks %in% vapply(result$items, `[[`, "", "label"))
    })$items
    labels <- vapply(items, `[[`, "", "label")
    expect_true(all(hooks %in% labels))
    resolved <- respond_completion_item_resolve(client, items[[match("S7_on_build", labels)]])
    expect_match(resolved$documentation$value, "S7_on_build", fixed = TRUE)
})
