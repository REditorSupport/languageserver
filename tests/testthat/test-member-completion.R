member_fixture <- function(code, snapshots = list(), point = NULL, language = "r") {
    content <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    uri <- if (language == "r") "file:///members.R" else "file:///members.qmd"
    document <- Document$new(uri, language = language, version = 1L, content = content)
    document$parse_data <- parse_document(uri, content, is_rmarkdown = document$is_rmarkdown)
    metadata <- collections::dict()
    for (package in names(snapshots)) metadata$set(package, member_index_thaw(snapshots[[package]]))
    workspace <- list(member_metadata = metadata)
    if (is.null(point)) point <- list(row = length(content) - 1L, col = nchar(tail(content, 1L)))
    list(document = document, workspace = workspace, point = point)
}

member_items <- function(code, snapshots = list(), point = NULL, language = "r", limit = 200L) {
    fixture <- member_fixture(code, snapshots, point, language)
    member_completion(
        fixture$document$uri, fixture$workspace, fixture$document,
        fixture$point, TRUE, limit
    )
}

member_labels <- function(...) vapply(member_items(...), `[[`, character(1L), "label")

test_that("Static inference stops at node, depth and time limits", {
    index <- member_generic_index("")
    budget <- new.env(parent = emptyenv())
    budget$remaining <- 0L
    budget$exhausted <- FALSE
    expect_identical(member_infer(quote(list(alpha = 1)), index, budget = budget)$reason, "budget")
    expect_true(budget$exhausted)

    budget$remaining <- 20000L
    budget$exhausted <- FALSE
    expect_identical(member_infer(quote(list(alpha = 1)), index, depth = 65L, budget = budget)$reason, "budget")
    expect_true(budget$exhausted)

    budget$exhausted <- FALSE
    budget$deadline <- proc.time()[[3L]] - 1
    expect_identical(member_infer(quote(list(alpha = 1)), index, budget = budget)$reason, "budget")
    expect_true(budget$exhausted)
})

test_that("Static inference notices deadlines that expire during traversal", {
    calls <- 0L
    clock <- function() {
        calls <<- calls + 1L
        c(0, 0, if (calls == 1L) 0 else 2)
    }
    stub(member_infer, "proc.time", clock, depth = 2L)
    budget <- new.env(parent = emptyenv())
    budget$remaining <- 20000L
    budget$exhausted <- FALSE
    budget$deadline <- 1
    expr <- as.call(c(list(as.name("list")), rep(list(1L), 256L)))
    value <- member_infer(expr, member_generic_index(""), budget = budget)
    expect_true(budget$exhausted)
    expect_identical(tail(value$elements, 1L)[[1L]]$reason, "budget")
})

test_that("Static members propagate through source factories and aliases", {
    expect_identical(member_labels("x <- list(alpha=1,beta=2)\ny <- x\ny$"), c("alpha", "beta"))
    expect_identical(member_labels(paste0(
        "factory <- function(x) list(step=function() list(finish=x))\n",
        "factory({stop(\"argument\")})$step()$"
    )), "finish")
    expect_identical(member_labels(paste0(
        "factory <- function(x) {self <- new.env(); self$filter <- function(p) self;",
        " self$collect <- function() list(value=x); self}\n",
        "factory(unresolved)$filter(pred)$"
    )), c("collect", "filter"))
    expect_identical(member_labels("x <- list(a=1)\nx <- list(b=1)\nx$"), "b")
    expect_identical(member_labels("x <- list(a=1)\ny <- x\nx <- list(b=1)\ny$"), "a")
    expect_identical(member_labels("f <- function(x) x\nf(list(a=1))$"), "a")
    expect_identical(member_labels("f <- function(flag) {if(flag) return(list(a=1,b=1)); list(a=2,c=1)}\nf(unknown)$"), "a")
    expect_length(member_labels("f <- function(flag) {if(flag) return(opaque()); list(a=1)}\nf(unknown)$"), 0L)
    expect_length(member_labels("f <- function(x) {out <- list(a=1); for (out in x) NULL; out}\nf(unknown)$"), 0L)
    expect_length(member_labels("f <- function() f()\nf()$"), 0L)
})

test_that("Cursor recovery respects quoting, comments, Unicode, and replacement ranges", {
    expect_identical(member_labels("bad <- )\nx <- list(alpha=1)\nx$al"), "alpha")
    expect_identical(member_labels("x <- list(alpha=1)\nx $  al"), "alpha")
    expect_identical(member_labels("list(\n alpha=list(beta=1)\n)$alpha$"), "beta")
    expect_length(member_labels("# x$"), 0L)
    expect_length(member_labels("\"list(a=1)$\""), 0L)
    code <- "x <- list(`a b`=1, beta=2)\nx$`a"
    item <- member_items(code)[[1L]]
    expect_identical(item$label, "a b")
    expect_identical(item$textEdit$newText, "`a b`")
    expect_identical(item$textEdit$range$start$character, 2L)
    code <- "x <- list(alpha=1)\n\"\U0001f680\"; x$alXYZ"
    item <- member_items(code, point = list(row = 1L, col = 9L))[[1L]]
    expect_identical(item$textEdit$range$start$character, 8L)
    expect_identical(item$textEdit$range$end$character, 13L)
    expect_identical(member_labels("```{r}\nx <- list(alpha=1)\nx$\n```",
        point = list(row = 2L, col = 2L), language = "quarto"
    ), "alpha")
    expect_length(member_labels("x$\n```{python}\nx$\n```",
        point = list(row = 2L, col = 2L), language = "quarto"
    ), 0L)
    items <- member_items("list(a=1,b=1,c=1)$", limit = 2L)
    expect_length(items, 2L)
    expect_true(isTRUE(attr(items, "truncated")))
})

test_that("Lexical shadowing and source position prevent invented members", {
    expect_length(member_labels("list <- function(...) unknown()\nlist(a=1)$"), 0L)
    expect_length(member_labels("new.env <- function(...) unknown()\nnew.env()$"), 0L)
    expect_length(member_labels("x$\nx <- list(a=1)", point = list(row = 0L, col = 2L)), 0L)
    expect_length(member_labels("x <- list(a=1)\nf <- function(x) { x$\n}", point = list(row = 1L, col = 22L)), 0L)
    expect_identical(member_labels("f <- function() {x <- list(a=1); x$\n}", point = list(row = 0L, col = 44L)), "a")
    expect_length(member_labels("x <- list(a=1)\nx$a <- opaque()\nx$"), 0L)
})

test_that("Package selection ignores bindings after an incomplete member access", {
    index <- member_generic_index("factory <- function() opaque()")
    index$package <- "fixture"
    index$exports <- "factory"
    index$roots$factory <- member_value(function_key = "factory")
    index$constructor_types$factory <- "box"
    index$members$box <- c(collect = NA_character_)
    snapshot <- member_index_freeze(index)
    expect_identical(member_labels(
        "library(fixture)\nx <- factory()\nx$\ny <- NULL",
        list(fixture = snapshot), point = list(row = 2L, col = 2L)
    ), "collect")
})

test_that("Member completion inserts parseable non-syntactic names", {
    labels <- c("Mazda RX4", "1st", "if", "NA_real_", "a`b", "a\\b", "a\nb", "plain")
    for (label in labels) {
        code <- paste0("x <- list(", encodeString(label, quote = "`"), "=1)\nx$")
        item <- member_items(code)[[1L]]
        expect_identical(item$label, label)
        parsed <- parse(text = paste0("x$", item$textEdit$newText))[[1L]]
        expect_identical(as.character(parsed[[3L]]), label)
    }
    code <- "x <- data.frame(`Mazda RX4`=1, check.names=FALSE)\nx$Ma"
    fixture <- member_fixture(code)
    reply <- completion_reply(1L, fixture$document$uri, fixture$workspace,
        fixture$document, fixture$point, list())
    item <- reply$result$items[[1L]]
    expect_identical(item$textEdit$newText, "`Mazda RX4`")
    expect_identical(item$textEdit$range$start$character, 2L)
    expect_identical(item$textEdit$range$end$character, 4L)
})

test_that("Member snippets preserve R escapes in quoted method names", {
    label <- "run\\$task}`"
    code <- paste0("x <- list(", encodeString(label, quote = "`"), "=function() 1)\nx$")
    item <- member_items(code)[[1L]]
    # These are the literal characters a snippet client inserts before ().
    inserted <- sub("($0)", "()", item$textEdit$newText, fixed = TRUE)
    inserted <- gsub("\\\\([\\\\$}])", "\\1", inserted, perl = TRUE)
    parsed <- parse(text = paste0("x$", inserted))[[1L]]
    expect_identical(as.character(parsed[[1L]][[3L]]), label)
    expect_identical(item$insertTextFormat, InsertTextFormat$Snippet)

    fixture <- member_fixture(paste0(code, "()"), point = list(row = 1L, col = 2L))
    plain <- member_completion(fixture$document$uri, fixture$workspace, fixture$document,
        fixture$point, TRUE, 200L)[[1L]]
    expect_identical(plain$textEdit$newText, encodeString(label, quote = "`"))
    expect_identical(plain$insertTextFormat, InsertTextFormat$PlainText)
})

test_that("R6 fluent APIs expose public inheritance without initialization", {
    code <- paste0(
        "Parent <- R6::R6Class(\"Parent\", public=list(base=function() self, ",
        "field=1))\nChild <- R6::R6Class(\"Child\", inherit=Parent, ",
        "public=list(initialize=function() stop(\"must not initialize\"),",
        "base=function() list(done=1), child=function() self),",
        "private=list(secret=1), active=list(danger=function() stop(\"getter\")))\n"
    )
    expect_identical(
        member_labels(paste0(code, "Child$new()$child()$")),
        c("base", "child", "danger", "field", "initialize")
    )
    expect_identical(member_labels(paste0(code, "Child$new()$base()$")), "done")
    expect_length(member_labels(paste0(code, "Child$new()$danger$")), 0L)
})

test_that("R6 initialization exposes finite copied methods without executing code", {
    marker <- withr::local_tempfile()
    declarations <- paste0(
        "commands <- function(send) list(GET=function(key) send(key), ",
        "SET=function(key, value, option=NULL) send(key, value))\n",
        "Client <- R6::R6Class(\"Client\", lock_objects=FALSE, public=list(initialize=function(send) {",
        sprintf("writeLines(\"ran\", %s);", encodeString(marker, quote = "\"")),
        "methods <- commands(send); for (name in names(methods)) self[[name]] <- methods[[name]]",
        "}, ready=function() self))\n"
    )
    expect_identical(member_labels(paste0(declarations, "Client$new(unknown)$")),
        c("GET", "SET", "initialize", "ready"))
    expect_identical(member_labels(paste0(declarations, "Client$new(unknown)$ready()$")),
        c("GET", "SET", "initialize", "ready"))
    expect_false(file.exists(marker))

    for (body in c("for (name in names(unknown)) self[[name]] <- unknown[[name]]",
            "methods <- list(run=function() NULL); for (name in unknown) self[[name]] <- methods[[name]]",
            "methods <- list(run=function() NULL); for (name in names(methods)) self[[name]] <- opaque()")) {
        code <- sprintf("Client <- R6::R6Class(\"Client\", lock_objects=FALSE, public=list(initialize=function() {%s}, ready=1))\nClient$new()$", body)
        expect_identical(member_labels(code), c("initialize", "ready"))
    }
    code <- paste0("Client <- R6::R6Class(\"Client\", lock_objects=FALSE, public=list(initialize=function(flag) {",
        "if (flag) {methods <- list(run=function() NULL); for (name in names(methods)) self[[name]] <- methods[[name]]}",
        "}, ready=1))\n")
    expect_identical(member_labels(paste0(code, "Client$new(unknown)$")), c("initialize", "ready"))
    expect_identical(member_labels(paste0(code, "Client$new(FALSE)$")), c("initialize", "ready"))
    expect_identical(member_labels(paste0(code, "Client$new(TRUE)$")), c("initialize", "ready", "run"))
})

test_that("Internal package R6 generators are available to exported factories", {
    marker <- withr::local_tempfile()
    ns <- new.env(parent = emptyenv())
    ns$commands <- function() list(extra = function(key) NULL)
    ns$Internal <- R6::R6Class("Internal", lock_objects = FALSE, public = list(
        initialize = function() {
            writeLines("ran", marker)
            methods <- commands()
            for (name in names(methods)) self[[name]] <- methods[[name]]
        },
        run = function(value = 1) self
    ))
    ns$client <- function() Internal$new()
    input <- member_namespace_input(ns, package = "fixture", exports = c("client", "Internal"))
    index <- member_package_index(input)
    index$package_roots$Internal <- member_r6_runtime_shape(ns$Internal, index)
    index$namespace_roots$`fixture::Internal` <- index$package_roots$Internal
    snapshot <- member_index_freeze(index)
    expect_identical(member_labels("fixture::client()$", list(fixture = snapshot)),
        c("clone", "extra", "initialize", "run"))
    expect_identical(member_labels("fixture::Internal$new()$", list(fixture = snapshot)),
        c("clone", "extra", "initialize", "run"))
    expect_false(file.exists(marker))
})

test_that("R6 initialization respects locking and ignores initializer return values", {
    for (locking in c("", ", lock_objects=TRUE", ", lock_objects=unknown")) {
        for (body in c("self$extra <- 1",
                "methods <- list(extra=function() NULL); for (name in names(methods)) self[[name]] <- methods[[name]]")) {
            code <- sprintf('Client <- R6::R6Class("Client"%s, public=list(initialize=function() {%s}, ready=1))\nClient$new()$', locking, body)
            expect_identical(member_labels(code), c("initialize", "ready"))
        }
    }
    for (body in c("self$extra <- 1; return(private)",
            "self$extra <- 1; return(invisible(NULL))", "self$extra <- 1; return(list(secret=1))",
            "self$extra <- 1; self <- private; return(self)", "return({self$extra <- 1; NULL})")) {
        code <- sprintf('Client <- R6::R6Class("Client", lock_objects=FALSE, public=list(initialize=function() {%s}), private=list(secret=1))\nClient$new()$', body)
        expect_identical(member_labels(code), c("extra", "initialize"), info = body)
    }
    for (body in c("self$run <- function(value=2) NULL; return(private)",
            "alias <- self; alias$run <- function(value=2) NULL; return(NULL)")) {
        code <- sprintf('Client <- R6::R6Class("Client", public=list(run=NULL, initialize=function() {%s}), private=list(secret=1))\nClient$new()$ru', body)
        expect_identical(member_items(code)[[1L]]$detail, "run(value = 2)")
    }
    code <- paste0('Client <- R6::R6Class("Client", lock_objects=FALSE, public=list(initialize=function(flag) {',
        "self$common <- 1; if(flag) {self$early <- 1; return(private)}; self$late <- 1; return(NULL)",
        "}), private=list(secret=1))\nClient$new(unknown)$")
    expect_identical(member_labels(code), c("common", "initialize"))
    code <- paste0('Client <- R6::R6Class("Client", lock_objects=FALSE, public=list(initialize=function() {',
        "value <- if(TRUE) 42 else 0; self$answer <- value; return(private)",
        "}), private=list(secret=1))\nClient$new()$answer")
    index <- member_generic_index(code)
    bindings <- list(Client = member_infer(index$definitions$Client, index))
    expect_identical(member_infer(quote(Client$new()$answer), index, bindings)$literal, 42)

    # Runtime generator metadata must preserve the actual locking setting.
    for (locked in c(TRUE, FALSE)) {
        generator <- R6::R6Class("Client", lock_objects = locked, public = list(
            initialize = function() self$extra <- 1))
        index <- member_generic_index("")
        index$roots$Client <- member_r6_runtime_shape(generator, index)
        labels <- member_labels("library(fixture)\nClient$new()$", list(fixture = member_index_freeze(index)))
        expect_identical("extra" %in% labels, !locked)
    }
})

test_that("R6 assignments preserve declared methods and active bindings", {
    for (locked in c(TRUE, FALSE)) {
        for (body in c("self$run <- function(replacement=2) list(fake=1)",
                'self[["run"]] <- function(replacement=2) list(fake=1)',
                "alias <- self; alias$run <- function(replacement=2) list(fake=1)",
                "methods <- list(run=function(replacement=2) list(fake=1)); for (name in names(methods)) self[[name]] <- methods[[name]]")) {
            code <- sprintf('Client <- R6::R6Class("Client", lock_objects=%s, public=list(run=function(original=1) list(real=1), initialize=function() {%s}))\n', locked, body)
            expect_identical(member_items(paste0(code, "Client$new()$ru"))[[1L]]$detail, "run(original = 1)", info = body)
            expect_identical(member_labels(paste0(code, "Client$new()$run()$")), "real", info = body)
        }
        for (body in c("self$x <- list(fake=1)", 'self[["x"]] <- list(fake=1)',
                "alias <- self; alias$x <- list(fake=1)",
                "values <- list(x=list(fake=1)); for (name in names(values)) self[[name]] <- values[[name]]")) {
            code <- sprintf('Client <- R6::R6Class("Client", lock_objects=%s, public=list(initialize=function() {%s}), active=list(x=function(value) {if (missing(value)) NULL else invisible(NULL)}))\n', locked, body)
            expect_length(member_labels(paste0(code, "Client$new()$x$")), 0L)
        }
        # Data bindings stay writable after being assigned a function.
        for (body in c("self$run <- function(first=1) NULL; self$run <- function(second=2) NULL",
                "methods <- list(run=function(first=1) NULL); for (name in names(methods)) self[[name]] <- methods[[name]]; self$run <- function(second=2) NULL")) {
            code <- sprintf('Client <- R6::R6Class("Client", lock_objects=%s, public=list(run=NULL, initialize=function() {%s}))\nClient$new()$ru', locked, body)
            expect_identical(member_items(code)[[1L]]$detail, "run(second = 2)", info = body)
        }
    }
})

test_that("R6 inherited and private bindings retain their assignment semantics", {
    parent <- 'Parent <- R6::R6Class("Parent", public=list(run=function(original=1) list(real=1)), private=list(hidden=function() list(real=1)), active=list(x=function(value) NULL))\n'
    child <- paste0('Child <- R6::R6Class("Child", inherit=Parent, lock_objects=FALSE, public=list(initialize=function(flag) {',
        "if (flag) self$data <- 1 else self$data <- 2;",
        "self$run <- function(replacement=2) list(fake=1); self$x <- list(fake=1)",
        "}))\n")
    code <- paste0(parent, child)
    expect_identical(member_items(paste0(code, "Child$new(unknown)$ru"))[[1L]]$detail, "run(original = 1)")
    expect_length(member_labels(paste0(code, "Child$new(unknown)$x$")), 0L)
    # Check the private method within the flow that attempts to replace it.
    for (body in c("private$hidden <- function() list(fake=1)",
            "methods <- list(hidden=function() list(fake=1)); for (name in names(methods)) private[[name]] <- methods[[name]]")) {
        code <- paste0(parent, 'Child <- R6::R6Class("Child", inherit=Parent, lock_objects=FALSE, public=list(read=function() {',
            body, "; private$hidden()", "}))\n")
        expect_identical(member_labels(paste0(code, "Child$new()$read()$")), "real")
    }
})

test_that("Runtime R6 metadata preserves method and active binding kinds", {
    marker <- withr::local_tempfile()
    for (locked in c(TRUE, FALSE)) {
        generator <- R6::R6Class("Client", lock_objects = locked, public = list(
            run = function(original = 1) list(real = 1),
            initialize = function() {
                self$run <- function(replacement = 2) list(fake = 1)
                self$x <- list(fake = 1)
            }
        ), active = list(x = function(value) {
            writeLines("ran", marker)
            if (missing(value)) NULL else invisible(NULL)
        }))
        index <- member_generic_index("")
        index$roots$Client <- member_r6_runtime_shape(generator, index)
        snapshot <- list(fixture = member_index_freeze(index))
        code <- "library(fixture)\nClient$new()$"
        expect_identical(member_items(paste0(code, "ru"), snapshot)[[1L]]$detail, "run(original = 1)")
        expect_length(member_labels(paste0(code, "x$"), snapshot), 0L)
    }
    expect_false(file.exists(marker))
})

test_that("Installed Redux members resolve without a Redis connection", {
    skip_if_not_installed("redux")
    marker <- withr::local_tempfile()
    snapshot <- member_prepare_package("redux")
    expect_false("R6_redis_api" %in% names(snapshot$roots))
    expect_length(member_labels("redux::R6_redis_api$new()$", list(redux = snapshot)), 0L)
    for (code in c('redis <- redux::hiredis(host = "127.0.0.1")',
            "library(redux)\nredis <- hiredis(host = unknown)",
            sprintf("redis <- redux::hiredis(host = {writeLines(\"ran\", %s); stop(\"host\")})",
                encodeString(marker, quote = "\"")))) {
        labels <- member_labels(paste0(code, "\nredis$"), list(redux = snapshot), limit = 300L)
        expect_true(all(c("GET", "SET", "PING", "pipeline", "subscribe", "config", "reconnect") %in% labels))
        expect_false(any(c(".pipeline", ".subscribe", ".version") %in% labels))
        items <- member_items(paste0(code, "\nredis$GE"), list(redux = snapshot))
        item <- Filter(function(item) identical(item$label, "GET"), items)[[1L]]
        expect_identical(item$detail, "GET(key)")
        expect_identical(item$kind, CompletionItemKind$Method)
    }
    expect_false(file.exists(marker))
})

test_that("Generated closures retain referenced lexical inputs only", {
    index <- member_generic_index("")
    env <- list(used = member_literal(42), unused = member_value(type = "environment", fields = list()))
    value <- member_infer(quote(function(x = used) list(run = function() x)), index, env)
    expect_identical(names(value$closure), "used")
    expect_identical(member_infer(quote(f()$run()), index, list(f = value))$literal, 42)
    value <- member_infer(quote(function() function(x = used) x), index, env)
    expect_identical(names(value$closure), "used")
    expect_identical(member_infer(quote(f()()), index, list(f = value))$literal, 42)
})

test_that("Runtime metadata skips deferred values and active getters", {
    marker <- tempfile()
    on.exit(unlink(marker))
    env <- new.env(parent = emptyenv())
    env$factory <- function(x) list(step = function() list(finish = x))
    makeActiveBinding("unsafe", function() {
        writeLines("getter", marker)
        stop("getter")
    }, env)
    delayedAssign("deferred",
        {
            writeLines("promise", marker)
            stop("promise")
        },
        assign.env = env
    )
    snapshot <- member_snapshot(env)
    shape <- member_snapshot_shape(snapshot)
    expect_identical(sort(names(shape$fields)), c("deferred", "factory", "unsafe"))
    expect_null(shape$fields$deferred$type)
    expect_null(shape$fields$unsafe$type)
    expect_false(file.exists(marker))
    expect_identical(member_binding(env, "deferred")[[1L]], "deferred")
})

test_that("Receiver roots take precedence over unrelated exports regardless of cache order", {
    unrelated <- member_generic_index("")
    unrelated$package <- "unrelated"
    unrelated$exports <- c("scan", "keep", "finish", "path")
    receiver <- member_generic_index("scan <- function(path) NULL\nkeep <- function(value) self\nfinish <- function() NULL")
    receiver$package <- "receiverfixture"
    receiver$exports <- "api"
    receiver$roots <- receiver$package_roots <- list(api = member_value(type = "api"))
    receiver$constructor_types <- list(scan = "query")
    receiver$members <- list(api = c(scan = "scan"), query = c(keep = "keep", finish = "finish"))
    snapshots <- lapply(list(unrelated = unrelated, receiverfixture = receiver), member_index_freeze)
    fixture <- member_fixture("library(receiverfixture)\nq <- api$scan(path)\nq1 <- q$keep(value)\nq1$")
    for (order in list(names(snapshots), rev(names(snapshots)))) {
        metadata <- collections::ordered_dict()
        for (package in order) metadata$set(package, as.list(member_index_thaw(snapshots[[package]])))
        fixture$workspace$member_metadata <- metadata
        resolved <- member_resolve_cursor(
            fixture$document$uri, fixture$workspace, fixture$document, fixture$point,
            member_cursor(fixture$document, fixture$point)
        )
        expect_identical(resolved$index$package, "receiverfixture")
        expect_identical(resolved$value$type, "query")
        items <- member_completion(fixture$document$uri, fixture$workspace, fixture$document,
            fixture$point, TRUE, 200L)
        expect_setequal(vapply(items, `[[`, character(1L), "label"), c("keep", "finish"))
    }
})

test_that("Lazy namespace indexes preserve attached and qualified calls without unrelated restores", {
    receiver <- member_generic_index("scan <- function(value) NULL")
    receiver$package <- "receiverfixture"
    receiver$exports <- "api"
    receiver$roots <- receiver$package_roots <- list(api = member_value(type = "api"))
    receiver$constructor_types <- list(scan = "query")
    receiver$members <- list(api = c(scan = "scan"))
    helper <- member_generic_index("finish <- function(value) list(done = value)")
    helper$package <- "helperfixture"
    helper$exports <- "finish"
    unrelated <- member_generic_index("")
    unrelated$package <- "unrelated"
    for (index in list(receiver, helper, unrelated)) index$padding <- raw(30000L)
    snapshots <- lapply(list(receiverfixture = receiver, helperfixture = helper, unrelated = unrelated), member_index_freeze)
    budget <- max(vapply(snapshots, function(x) as.numeric(object.size(x)), numeric(1L))) + 1024
    cache <- MemberMetadataCache$new(budget)
    for (package in names(snapshots)) cache$set(package, snapshots[[package]])
    fetched <- character()
    metadata <- list(keys = cache$keys, catalog = cache$catalog, get = function(package) {
        fetched <<- c(fetched, package)
        cache$get(package)
    })
    for (call in c("finish(api$scan(x))", "helperfixture::finish(api$scan(x))")) {
        fetched <- character()
        fixture <- member_fixture(paste("library(receiverfixture)", "library(helperfixture)", paste0(call, "$"), sep = "\n"))
        fixture$workspace$member_metadata <- metadata
        items <- member_completion(fixture$document$uri, fixture$workspace, fixture$document, fixture$point, TRUE, 200L)
        expect_identical(vapply(items, `[[`, character(1L), "label"), "done")
        expect_setequal(fetched, c("receiverfixture", "helperfixture"))
    }
    fetched <- character()
    fixture <- member_fixture("library(receiverfixture)\napi$scan(x)$")
    fixture$workspace$member_metadata <- metadata
    resolved <- member_resolve_cursor(fixture$document$uri, fixture$workspace, fixture$document,
        fixture$point, member_cursor(fixture$document, fixture$point))
    expect_identical(resolved$value$type, "query")
    expect_identical(fetched, "receiverfixture")
})

test_that("Installed Polars metadata resolves all original query positions without execution", {
    skip_if_not_installed("polars")
    snapshot <- member_prepare_package("polars")
    query <- paste0(
        "q <- pl$scan_csv(csv_file, infer_schema_files=10)$filter(",
        "pl$col(\"Sepal.Length\") > 5)$group_by(\"Species\", .maintain_order=TRUE)",
        "$agg(pl$all()$sum())"
    )
    positions <- gregexpr("$", query, fixed = TRUE)[[1L]]
    expected <- c("scan_csv", "filter", "col", "group_by", "agg", "all", "sum")
    for (i in seq_along(positions)) {
        code <- paste0("library(polars)\n", substr(query, 1L, positions[[i]]))
        expect_true(expected[[i]] %in% member_labels(code, list(polars = snapshot)), info = code)
    }
    expect_true("collect" %in% member_labels(paste0("library(polars)\n", query, "\nq$"), list(polars = snapshot)))
    expect_true("numeric" %in% member_labels("polars::cs$", list(polars = snapshot)))
    expect_length(member_labels("pl$", list(polars = snapshot)), 0L)
    expect_length(member_labels("library(polars)\npl <- unknown\npl$", list(polars = snapshot)), 0L)
    expect_length(member_labels("library(polars)\nf <- function(pl) {pl$\n}",
        list(polars = snapshot),
        point = list(row = 1L, col = 23L)
    ), 0L)
    marker <- tempfile()
    on.exit(unlink(marker))
    code <- sprintf(
        "library(polars)\npl$scan_csv({writeLines(\"ran\",%s); stop(\"input\")})$filter(stop(\"predicate\"))$",
        encodeString(marker, quote = "\"")
    )
    expect_true("collect" %in% member_labels(code, list(polars = snapshot)))
    expect_false(file.exists(marker))
    changed <- snapshot
    rewrite <- function(node) {
        if (missing(node)) {
            return(node)
        }
        if (is.symbol(node) && identical(as.character(node), ".savvy_wrap_PlRLazyFrame")) {
            return(as.name(".savvy_wrap_PlRDataFrame"))
        }
        if (is.call(node)) {
            return(as.call(lapply(as.list(node), rewrite)))
        }
        node
    }
    changed$definitions$PlRLazyFrame_filter <- rewrite(changed$definitions$PlRLazyFrame_filter)
    idx <- member_index_thaw(changed)
    expect_identical(member_infer(quote(pl$scan_csv(x)$filter(y)), idx, idx$roots)$type, "polars_data_frame")
    expect_identical(unserialize(serialize(snapshot, NULL)), snapshot)
})

test_that("Polars query assignments retain LazyFrame members within the request budget", {
    skip_if_not_installed("polars")
    snapshot <- member_prepare_package("polars")
    lines <- c(
        "library(polars)", "",
        "csv_file <- tempfile(fileext = \".csv\")",
        "write.csv(iris, csv_file, row.names = FALSE)", "",
        "q <- pl$scan_csv(csv_file, infer_schema_files = 10)", "",
        "q1 <- q$filter(pl$col(\"Sepal.Length\") > 5)", "q1", "",
        "q2 <- q1$group_by(\"Species\")$agg(pl$all()$sum())", "q2"
    )
    for (row in c(8L, 11L)) {
        edited <- lines
        name <- edited[[row + 1L]]
        edited[[row + 1L]] <- paste0(name, "$")
        fixture <- member_fixture(
            paste(edited, collapse = "\n"), list(polars = snapshot),
            point = list(row = row, col = 3L)
        )
        resolved <- member_resolve_cursor(
            fixture$document$uri, fixture$workspace, fixture$document, fixture$point,
            member_cursor(fixture$document, fixture$point)
        )
        expect_identical(resolved$value$type, "polars_lazy_frame", info = name)
        expect_false(resolved$budget$exhausted, info = name)
        items <- member_completion(
            fixture$document$uri, fixture$workspace, fixture$document, fixture$point,
            TRUE, 200L
        )
        labels <- vapply(items, `[[`, character(1L), "label")
        expect_true(all(c("collect", "filter", "group_by") %in% labels), info = name)
        expect_true(all(vapply(items, function(item) identical(item$data$type, "member"), logical(1L))))
        expect_false(any(c("fileext", "infer_schema_files", "row.names") %in% labels))
    }
})

test_that("Reassigned collected Polars queries retain their current DataFrame members", {
    skip_if_not_installed("polars")
    snapshot <- member_prepare_package("polars")
    lines <- c(
        "library(polars)", "",
        "csv_file <- tempfile(fileext = \".csv\")",
        "write.csv(iris, csv_file, row.names = FALSE)", "",
        "q <- pl$scan_csv(csv_file, infer_schema_files = 10)", "",
        "q1 <- q$filter(pl$col(\"Sepal.Length\") > 5)", "q1 # working here", "",
        "q1 <- q$filter(pl$col(\"Sepal.Length\") > 5)$group_by(\"Species\")$agg(pl$all()$median())$collect()",
        "q1 # collected", "",
        "q2 <- q1$group_by(\"Species\")$agg(pl$all()$sum())", "q2 # collected"
    )
    for (row in c(8L, 11L, 14L)) {
        edited <- lines
        edited[[row + 1L]] <- paste0(substr(edited[[row + 1L]], 1L, 2L), "$", substring(edited[[row + 1L]], 3L))
        fixture <- member_fixture(paste(edited, collapse = "\n"), list(polars = snapshot),
            point = list(row = row, col = 3L))
        resolved <- member_resolve_cursor(
            fixture$document$uri, fixture$workspace, fixture$document, fixture$point,
            member_cursor(fixture$document, fixture$point)
        )
        expect_identical(resolved$value$type, if (row == 8L) "polars_lazy_frame" else "polars_data_frame")
        expect_false(resolved$budget$exhausted)
        labels <- names(member_members(resolved$value, resolved$index))
        expect_true(all(c("filter", "group_by") %in% labels))
        expect_identical("collect" %in% labels, row == 8L)
        expect_identical("lazy" %in% labels, row != 8L)
    }
})

test_that("LSP completion returns member identity and preserves calls", {
    fixture <- member_fixture("factory <- function() list(run=function(arg=1) list(done=1))\nfactory()$ru()")
    fixture$point <- list(row = 1L, col = 12L)
    reply <- completion_reply(
        1L, fixture$document$uri, fixture$workspace, fixture$document,
        fixture$point, list(completionItem = list(snippetSupport = TRUE))
    )
    item <- reply$result$items[[1L]]
    expect_identical(item$label, "run")
    expect_identical(item$textEdit$newText, "run")
    expect_identical(item$data$type, "member")
    expect_match(item$data$signature, "arg")
    resolved <- completion_item_resolve_reply(
        2L, fixture$workspace, item,
        list(completionItem = list(labelDetailsSupport = TRUE))
    )
    expect_match(resolved$result$labelDetails$detail, "arg")
    expect_true("$" %in% unlist(CompletionOptions$triggerCharacters))
})

test_that("Package metadata fingerprints follow actual API changes", {
    env <- new.env(parent = emptyenv())
    env$one <- function(x) list(a = x)
    first <- digest::digest(member_snapshot(env), algo = "sha256")
    env$two <- function(x, y = 1) list(b = y)
    second <- digest::digest(member_snapshot(env), algo = "sha256")
    expect_false(identical(first, second))
    env$one <- function(x) list(replaced = x)
    expect_false(identical(second, digest::digest(member_snapshot(env), algo = "sha256")))
})

test_that("Unknown branch writes and unrecognized dollar dispatch stay conservative", {
    expect_length(member_labels("x <- list(a=1)\nif (unknown) x <- opaque()\nx$"), 0L)
    expect_length(member_labels("x <- structure(list(a=list(b=1)),class=\"custom\")\nx$a$"), 0L)
})

test_that("Literal custom namespace registration is document local", {
    skip_if_not_installed("polars")
    snapshot <- member_prepare_package("polars")
    code <- paste(c(
        "library(polars)",
        "shortcuts <- function(s) {self <- new.env(); self$`_s` <- s; self$square <- function() self$`_s`*self$`_s`; class(self) <- c(\"custom_namespace\",\"polars_object\"); self}",
        "pl$api$register_series_namespace(\"math\", shortcuts)",
        "s <- as_polars_series(1:3)", "s$math$square()$"
    ), collapse = "\n")
    expect_true("rename" %in% member_labels(code, list(polars = snapshot)))
    expect_false("math" %in% member_labels("library(polars)\nas_polars_series(1:3)$", list(polars = snapshot)))
})

test_that("Installed Polars namespaces preserve distinct result and signature identities", {
    skip_if_not_installed("polars")
    snapshot <- member_prepare_package("polars")
    index <- member_index_thaw(snapshot)
    expected <- c(
        "pl$col(\"a\")$str$to_uppercase()" = "polars_expr",
        "pl$Series(\"a\",1:3)$str$to_uppercase()" = "polars_series",
        "pl$DataFrame(a=1:3)$lazy()" = "polars_lazy_frame",
        "pl$when(pl$col(\"a\")>0)$then(1)$otherwise(0)" = "polars_expr"
    )
    for (code in names(expected)) {
        expect_identical(
            member_infer(parse(text = code)[[1L]], index, index$roots)$type,
            unname(expected[[code]]),
            info = code
        )
    }
    items <- member_items("library(polars)\npl$all()$su", list(polars = snapshot))
    item <- items[[which(vapply(items, `[[`, character(1L), "label") == "sum")]]
    expect_identical(item$data$function_id, "expr__sum")
    expect_identical(item$data$package, "polars")
})

test_that("Deferred values, dispatch hooks and defaults are never evaluated", {
    marker <- tempfile()
    on.exit(unlink(marker))
    code <- sprintf(paste0(
        "f <- function(x={writeLines(\"ran\",%s);stop(\"default\")}) list(done=1)\n",
        "f()$"
    ), encodeString(marker, quote = "\""))
    expect_identical(member_labels(code), "done")
    expect_false(file.exists(marker))
    code <- paste0(
        "`$.custom` <- function(x,name) stop(\"dispatch\")\n",
        "`.DollarNames.custom` <- function(x,pattern) stop(\"hook\")\n",
        "x <- structure(list(a=1),class=\"custom\")\nx$"
    )
    expect_length(member_labels(code), 0L)
})

test_that("Local branch writes and later builtin shadowing respect position", {
    expect_length(member_labels("f <- function() {x <- list(a=1); if(flag) x <- opaque(); x$\n}",
        point = list(row = 0L, col = 72L)
    ), 0L)
    expect_identical(member_labels("x <- list(a=1)\nx$\nlist <- function(...) unknown()",
        point = list(row = 1L, col = 2L)
    ), "a")
})

test_that("Invalid empty binding names cannot break document parsing", {
    content <- c("\"\" <- 2", "assign(\"\", 3)", "d1 <- 1")
    parsed <- parse_document("file:///invalid.R", content)
    expect_true("d1" %in% names(parsed$definitions))
    expect_true("d1" %in% names(parsed$member_data$bindings))
})

test_that("Metadata resolution survives edits with unchanged package requests", {
    uri <- "file:///resolution.R"
    document <- Document$new(uri, version = 2L, content = "library(fixture)")
    document$requested_packages <- list(packages = "fixture", namespace_packages = character())
    docs <- collections::dict()
    docs$set(uri, document)
    metadata <- collections::dict()
    workspace <- list(
        documents = docs, member_metadata = metadata,
        load_packages = function(...) NULL, update_loaded_packages = function(...) NULL
    )
    self <- list(get_workspace = function(...) workspace)
    snapshot <- member_index_freeze(member_generic_index(""))
    resolve_callback(self, uri, 1L, list(packages = "fixture", members = list(fixture = snapshot), requested = document$requested_packages))
    expect_true(metadata$has("fixture"))
    metadata$clear()
    resolve_callback(self, uri, 1L, list(packages = "other", members = list(other = snapshot),
            requested = list(packages = "other", namespace_packages = character())))
    expect_false(metadata$has("other"))
})

test_that("Package extraction follows registries and factories with unrelated names", {
    marker <- tempfile()
    on.exit(unlink(marker))
    code <- paste(c(
        "api <- new.env(parent=emptyenv())",
        "commands <- new.env(parent=emptyenv())",
        "tools <- new.env(parent=emptyenv())",
        "machine <- new.env(parent=emptyenv())",
        "native_box <- function(pointer) {box <- new.env(); box$ptr <- pointer; box$advance <- make_step(pointer); class(box) <- \"raw_box\"; box}",
        "make_step <- function(pointer) function(delta=1) native_box(.Call(\"never\",pointer,delta))",
        "adapt <- function(x) UseMethod(\"adapt\")",
        "adapt.raw_box <- function(x) {object <- new.env(); object$raw <- x; lapply(names(tools), function(label) makeActiveBinding(label,function() tools[[label]](object),object)); class(object) <- \"public_box\"; object}",
        "toolbox <- function(x) {object <- new.env(); object$raw <- x$raw; class(object) <- \"box_tools\"; object}",
        "`$.public_box` <- function(x,name) {method_names <- names(commands); if(name %in% method_names) {f <- commands[[name]]; current <- x; environment(f) <- environment(); f} else NextMethod()}",
        "`$.box_tools` <- function(x,name) {method_names <- names(commands); if(name %in% method_names) {f <- commands[[name]]; current <- x; environment(f) <- environment(); f} else NextMethod()}",
        "step <- function(delta=1) adapt(current$raw$advance(delta))",
        "commands$step <- step",
        "tools$tools <- toolbox",
        "machine$create <- function(input) native_box(.Call(\"never\",input))",
        "api$start <- function(input) adapt(machine$create(input))",
        "api$settings <- commands"
    ), collapse = "\n")
    scope <- new.env(parent = baseenv())
    # Execute only the trusted package fixture's declarations, like package
    # loading. Constructors and methods are never run by metadata preparation.
    eval(parse(text = code), scope)
    input <- member_namespace_input(scope, package = "fluentfixture", exports = c("api", "adapt"))
    snapshot <- member_index_freeze(member_package_index(input))
    expect_true("step" %in% member_labels("library(fluentfixture)\napi$start(stop(\"input\"))$", list(fluentfixture = snapshot)))
    expect_true("step" %in% member_labels("library(fluentfixture)\napi$start(x)$step()$tools$", list(fluentfixture = snapshot)))
    expect_true("step" %in% member_labels("library(fluentfixture)\napi$settings$", list(fluentfixture = snapshot)))
    index <- member_index_thaw(snapshot)
    expect_identical(member_infer(quote(api$start(x)$step(2)$tools$step()), index, index$roots)$type, "public_box")
    scope$commands$finish <- function() list(completed = TRUE)
    updated <- member_index_freeze(member_package_index(member_namespace_input(scope,
        package = "fluentfixture", exports = c("api", "adapt")
    )))
    expect_true("finish" %in% member_labels("library(fluentfixture)\napi$start(x)$", list(fluentfixture = updated)))
    expect_false(identical(snapshot$generation, updated$generation))
    expect_false(file.exists(marker))
})

test_that("Prepared returns preserve argument-dependent results and metadata changes", {
    marker <- tempfile()
    on.exit(unlink(marker))
    code <- paste(c(
        "api <- new.env(parent=emptyenv())",
        "native_box <- function(pointer) {box <- new.env(); box$step <- make_step(pointer); class(box) <- \"raw_box\"; box}",
        "make_step <- function(pointer) function() native_box(.Call(\"never\",pointer))",
        "adapt <- function(x) UseMethod(\"adapt\")",
        "adapt.raw_box <- function(x) {box <- new.env(); box$raw <- x; class(box) <- \"public_box\"; box}",
        sprintf("fixed <- function(first={writeLines(\"ran\",%s)}, second=1, third=1, fourth=1, fifth=1, sixth=1, seventh=1, eighth=1, ninth=1, tenth=1) adapt(native_box(.Call(\"never\")))", deparse(marker)),
        "varying <- function(first, second=1, third=1, fourth=1, fifth=1, sixth=1, seventh=1, eighth=1, ninth=1, tenth=1) if(first) adapt(native_box(NULL)) else list(other=1)",
        "api$fixed <- fixed", "api$varying <- varying"
    ), collapse = "\n")
    scope <- new.env(parent = baseenv())
    eval(parse(text = code), scope)
    index <- member_package_index(member_namespace_input(scope,
            package = "returnfixture", exports = "api"))
    snapshot <- member_index_freeze(index)
    expect_identical(member_infer(quote(api$fixed()), index, index$roots)$type, "public_box")
    expect_identical(member_infer(quote(api$fixed(stop("unused input"))), index, index$roots)$type, "public_box")
    expect_identical(member_infer(quote(api$fixed(fir = 1)), index, index$roots)$type, "public_box")
    expect_identical(member_infer(quote(api$fixed(first = 1, first = 2)), index, index$roots)$reason, "argument_matching")
    expect_identical(member_infer(quote(api$fixed(unknown = 1)), index, index$roots)$reason, "argument_matching")
    expect_identical(member_infer(quote(api$varying(FALSE)), index, index$roots)$type, "list")
    expect_identical(member_infer(quote(api$varying(TRUE)), index, index$roots)$type, "public_box")
    expect_identical(sort(member_infer(quote(api$varying(flag)), index, index$roots)$type), c("list", "public_box"))
    # A snapshot edited before loading cannot retain a prepared result derived
    # from an old method or one of its helpers.
    snapshot$definitions$fixed[[3L]] <- quote(list(updated = TRUE))
    changed <- member_index_thaw(snapshot)
    expect_identical(member_infer(quote(api$fixed()), changed, changed$roots)$type, "list")
    expect_false(file.exists(marker))
})

test_that("Delegation extraction derives captures and result bodies", {
    code <- paste(c(
        "api <- new.env(parent=emptyenv()); commands <- new.env(parent=emptyenv())",
        "original <- function(amount=1) list(ignored=amount)",
        "commands$run <- original",
        "api$start <- function(input) {object <- new.env(); object$payload <- input; class(object) <- \"delegating_box\"; object}",
        "build_delegate <- function(method,object) {saved <- object$payload; generated <- function() list(result=saved); formals(generated) <- formals(method); generated}",
        "`$.delegating_box` <- function(x,name) {allowed <- names(commands); if(name %in% allowed) {selected <- commands[[name]]; build_delegate(selected,x)} else NextMethod()}"
    ), collapse = "\n")
    scope <- new.env(parent = baseenv())
    eval(parse(text = code), scope)
    index <- member_package_index(member_namespace_input(scope, package = "delegationfixture", exports = "api"))
    value <- member_infer(quote(api$start(list(nested = 1))$run()$result), index, index$roots)
    expect_identical(names(value$fields), "nested")
    snapshot <- member_index_freeze(index)
    items <- member_items("library(delegationfixture)\napi$start(x)$ru", list(delegationfixture = snapshot))
    expect_match(items[[1L]]$data$signature, "amount")
    expect_identical(items[[1L]]$data$function_id, "original")
})

test_that("Installed S7 descriptors expose declarations without calling constructors", {
    skip_if_not_installed("S7")
    marker <- tempfile()
    on.exit(unlink(marker))
    scope <- new.env(parent = baseenv())
    scope$api <- new.env(parent = emptyenv())
    scope$Widget <- S7::new_class("Widget",
        properties = list(size = S7::class_numeric),
        constructor = function(...) {
            writeLines("ran", marker)
            stop("constructor")
            S7::new_object(S7::S7_object(), size = 1)
        }
    )
    scope$api$Widget <- scope$Widget
    index <- member_package_index(member_namespace_input(scope, package = "s7fixture", exports = "api"))
    value <- member_infer(quote(api$Widget()), index, index$roots)
    expect_identical(value$type, "Widget")
    expect_true("size" %in% names(member_members(value, index)))
    expect_false(file.exists(marker))
})
