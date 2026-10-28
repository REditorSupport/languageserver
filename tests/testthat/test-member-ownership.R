ownership_member_fixture <- function(code, snapshots) {
    content <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    document <- Document$new("file:///member-ownership.R", language = "r", version = 1L, content = content)
    data <- parse_document(document$uri, content)
    data$version <- 1L
    document$update_parse_data(data)
    metadata <- collections::dict()
    for (package in names(snapshots)) metadata$set(package, member_index_thaw(snapshots[[package]]))
    list(document = document, workspace = list(member_metadata = metadata),
        point = list(row = length(content) - 1L, col = nchar(tail(content, 1L))))
}

ownership_member_items <- function(code, snapshots) {
    fixture <- ownership_member_fixture(code, snapshots)
    member_completion(fixture$document$uri, fixture$workspace, fixture$document, fixture$point, FALSE, 200L)
}

ownership_member_labels <- function(code, snapshots) {
    vapply(ownership_member_items(code, snapshots), `[[`, character(1L), "label")
}

test_that("Static receivers retain namespace ownership when root names collide", {
    make_snapshot <- function(package, name, fn) {
        ns <- new.env(parent = baseenv())
        ns$api <- new.env(parent = emptyenv())
        class(ns$api) <- "ownership_commands"
        ns$api[[name]] <- TRUE
        ns$api$shared <- list(done = TRUE)
        ns$api$run <- fn
        ns$bundle <- list(api = ns$api)
        ns$private <- ns$api
        member_index_freeze(member_package_index(member_namespace_input(
            ns, package = package, exports = c("api", "bundle")
        )))
    }
    snapshots <- list(
        firstfixture = make_snapshot("firstfixture", "first", function(first = 1) list(first_result = TRUE)),
        secondfixture = make_snapshot("secondfixture", "second", function(second = 2) list(second_result = TRUE))
    )
    for (order in list(names(snapshots), rev(names(snapshots)))) {
        current <- snapshots[order]
        for (root in c("secondfixture::api", "secondfixture::bundle$api", "secondfixture:::private")) {
            expect_identical(ownership_member_labels(paste0(root, "$"), current), c("run", "second", "shared"))
            expect_identical(ownership_member_labels(paste0(root, "$run()$"), current), "second_result")
            item <- ownership_member_items(paste0(root, "$ru"), current)[[1L]]
            expect_identical(item$detail, "run(second = 2)")
            expect_identical(item$data$package, "secondfixture")
        }
        expect_identical(ownership_member_labels(paste0(
            "list(unused = firstfixture::api, selected = secondfixture::api)$selected$run()$"
        ), current), "second_result")
        expect_identical(ownership_member_labels(paste0(
            "choose <- function(flag) if (flag) firstfixture::api else secondfixture::api\nchoose(unknown)$"
        ), current), c("run", "shared"))
        expect_identical(ownership_member_labels(paste0(
            "choose <- function(flag) if (flag) firstfixture::api else secondfixture::api\nchoose(unknown)$shared$"
        ), current), "done")
    }
})

test_that("Subset dispatch stays conservative across alternate namespace classes", {
    ns <- new.env(parent = baseenv())
    ns$first <- structure(list(raw = list(wrong = TRUE)), class = "ownership_first")
    ns$second <- structure(list(raw = list(wrong = TRUE)), class = "ownership_second")
    ns$`[[.ownership_first` <- function(x, i) list(first = TRUE)
    ns$`[[.ownership_second` <- function(x, i) list(second = TRUE)
    snapshots <- list(ownershipfixture = member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "ownershipfixture", exports = c("first", "second")
    ))))
    expect_length(ownership_member_labels(paste0(
        "choose <- function(flag) if (flag) ownershipfixture::first else ownershipfixture::second\n",
        "choose(unknown)[[1L]]$"
    ), snapshots), 0L)
    expect_identical(ownership_member_labels("ownershipfixture::first[[1L]]$", snapshots), "first")
})

test_that("Callbacks created by namespace methods retain their defining package", {
    make_snapshot <- function(package, label) {
        ns <- new.env(parent = baseenv())
        ns$api <- list(run = function() list(callback = function() helper()))
        ns$helper <- if (label == "first") function() list(first = TRUE) else function() list(second = TRUE)
        member_index_freeze(member_package_index(member_namespace_input(ns,
                    package = package, exports = "api")))
    }
    snapshots <- list(firstfixture = make_snapshot("firstfixture", "first"),
        secondfixture = make_snapshot("secondfixture", "second"))
    expect_identical(ownership_member_labels("secondfixture::api$run()$callback()$", snapshots), "second")
})
