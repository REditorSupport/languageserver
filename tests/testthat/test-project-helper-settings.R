test_that("project helper rules combine matching caller globs in configuration order", {
    root <- withr::local_tempdir()
    uri <- function(path) path_to_uri(file.path(root, path))
    caller <- uri("tests/unit/sub/test-one.r")
    settings <- list("./tests/**/test-*.R" = c("support/b.R", "./support/a.R"),
        "tests/unit/**/*.R" = c("support/a.R", "tests/unit/sub/test-one.r", "support/c.R"))
    expected <- c(uri("support/b.R"), uri("support/a.R"), uri("support/c.R"))
    rules <- project_helper_rules(root, settings)
    paths <- project_helper_paths(rules, root, caller)
    expect_identical(names(paths), expected)
    expect_identical(unname(paths[[1L]]), file.path(root, "support", "b.R"))
    expect_identical(project_helper_paths(rules, root, uri("R/ordinary.R")), character())
    json <- jsonlite::toJSON(settings, auto_unbox = TRUE)
    from_client <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    expect_identical(project_helper_rules(root, from_client), rules)
})

test_that("project helper configuration accepts only relative callers and literal helper paths", {
    root <- withr::local_tempdir()
    caller <- path_to_uri(file.path(root, "test.R"))
    for (invalid in list(NULL, FALSE, "helper.R", list("helper.R"), list("*.R" = list(file = "x.R")))) {
        expect_identical(project_helper_rules(root, invalid), list())
    }
    settings <- list("../*.R" = "bad.R", "/test.R" = "bad.R", "*.R" = c("../bad.R",
            "D:\\bad.R", "~/bad.R", "/bad.R", "file:///bad.R", "unknown-*.R", "unknown?.R",
            "a/../../bad.R", NA_character_, "", "./ok.R"))
    expect_identical(names(project_helper_paths(project_helper_rules(root, settings), root, caller)),
        path_to_uri(file.path(root, "ok.R")))
    expect_identical(project_helper_paths(project_helper_rules(root, settings), root,
            path_to_uri(file.path(dirname(root), "test.R"))), character())
})

test_that("project helpers resolve the current targets of workspace symlinks", {
    skip_on_os("windows")
    root <- withr::local_tempdir()
    outside <- withr::local_tempdir()
    target <- file.path(root, "support")
    dir.create(target)
    link <- file.path(root, "linked")
    if (!file.symlink(target, link)) skip("Cannot create a symlink")
    caller <- path_to_uri(file.path(root, "test.R"))
    rules <- project_helper_rules(root, list("**/*.R" = "linked/new/helper.R"))
    expect_identical(names(project_helper_paths(rules, root, caller)), path_to_uri(file.path(link, "new/helper.R")))
    unlink(link)
    expect_true(file.symlink(outside, link))
    expect_identical(project_helper_paths(rules, root, caller), character())
})

test_that("project helper rules respect file globs and excluded ancestor directories", {
    root <- withr::local_tempdir()
    include <- index_glob_regex("**/*.R")
    exclude <- vapply(c("**/ignored.R", "support/private/", "vendor/**"), index_glob_regex, "")
    settings <- list("test.R" = c("support/tool.r", "support/ignored.R", "support/note.txt",
        "support/private/sub/hide.R", "vendor/a.R", "support/public/b.R", "root.R"))
    rules <- project_helper_rules(root, settings, include, exclude)
    paths <- project_helper_paths(rules, root, path_to_uri(file.path(root, "test.R")))
    expect_identical(unname(paths), file.path(root, c("support/tool.r", "support/public/b.R", "root.R")))
})
