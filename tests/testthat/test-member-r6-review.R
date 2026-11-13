r6_review_snapshot <- function(object) {
    ns <- new.env(parent = baseenv())
    ns$object <- object
    member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "r6reviewfixture", exports = "object"
    )))
}

r6_review_labels <- function(code, snapshot) {
    content <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    uri <- "file:///r6-review.R"
    document <- Document$new(uri, language = "r", version = 1L, content = content)
    data <- parse_document(uri, content)
    data$version <- 1L
    document$update_parse_data(data)
    metadata <- collections::dict()
    metadata$set("r6reviewfixture", member_index_thaw(snapshot))
    workspace <- list(member_metadata = metadata)
    point <- list(row = length(content) - 1L, col = nchar(tail(content, 1L)))
    items <- member_completion(uri, workspace, document, point, TRUE, 200L)
    vapply(items, `[[`, character(1L), "label")
}

test_that("Stored R6 function fields keep their original lexical bindings", {
    marker <- withr::local_tempfile()
    Class <- R6::R6Class("StoredCallbacks", public = list(
        callback = NULL, unrelated = NULL, mimic = NULL,
        run = function() self,
        read = function() private$first()
    ), private = list(payload = list(done = TRUE),
        first = function() private$second(), second = function() private$payload))
    object <- Class$new()
    object$callback <- local({
        self <- 42L
        function() list(value = self)
    })
    object$unrelated <- function() {
        writeLines("method", marker)
        private$payload
    }
    object$mimic <- local({
        self <- object
        private <- NULL
        function() {
            self
            private$payload
        }
    })
    snapshot <- r6_review_snapshot(object)
    index <- member_index_thaw(snapshot)
    value <- member_infer(str2lang("r6reviewfixture::object$callback()$value"), index, index$roots)
    expect_identical(value$literal, 42L)
    expect_identical(r6_review_labels("r6reviewfixture::object$callback()$", snapshot), "value")
    expect_length(r6_review_labels("r6reviewfixture::object$unrelated()$", snapshot), 0L)
    expect_length(r6_review_labels("r6reviewfixture::object$mimic()$", snapshot), 0L)
    expect_identical(r6_review_labels("r6reviewfixture::object$read()$", snapshot), "done")
    expect_identical(r6_review_labels("r6reviewfixture::object$run()$", snapshot),
        c("callback", "clone", "mimic", "read", "run", "unrelated"))
    expect_false(file.exists(marker))
})

test_that("Stored inherited R6 methods retain each lexical super environment", {
    Grand <- R6::R6Class("StoredGrand", public = list(run = function() list(grand = TRUE)))
    Parent <- R6::R6Class("StoredParent", inherit = Grand, public = list(run = function() super$run()))
    Child <- R6::R6Class("StoredChild", inherit = Parent, public = list(run = function() super$run()))
    snapshot <- r6_review_snapshot(Child$new())
    expect_identical(r6_review_labels("r6reviewfixture::object$run()$", snapshot), "grand")
})

test_that("Stored non-portable R6 instances conceal their internal bindings", {
    Class <- R6::R6Class("StoredNonPortable", portable = FALSE,
        public = list(read = function() private$payload),
        private = list(payload = list(done = TRUE)))
    snapshot <- r6_review_snapshot(Class$new())
    expect_identical(r6_review_labels("r6reviewfixture::object$", snapshot), c("clone", "read"))
    expect_identical(r6_review_labels("r6reviewfixture::object$read()$", snapshot), "done")
    expect_length(r6_review_labels("r6reviewfixture::object$private$", snapshot), 0L)
})

test_that("Stored R6 locked data bindings survive invalid replacement attempts", {
    Class <- R6::R6Class("StoredLockedData", public = list(payload = list(done = TRUE)))
    object <- Class$new()
    lockBinding("payload", object)
    snapshot <- r6_review_snapshot(object)
    expect_identical(r6_review_labels(paste0(
        "f <- function() { object <- r6reviewfixture::object\nobject$payload <- unknown()\nobject$payload$"
    ), snapshot), "done")
})

test_that("Stored R6 method contexts preserve private method binding locks", {
    Class <- R6::R6Class("StoredPrivateLocks", public = list(read = function() {
        private$helper <- function() list(fake = TRUE)
        private$helper()
    }), private = list(helper = function() list(real = TRUE)))
    snapshot <- r6_review_snapshot(Class$new())
    expect_identical(r6_review_labels("r6reviewfixture::object$read()$", snapshot), "real")
})

# The tests below cover the storage of R6 method contexts. A method does not
# hold its `private` and `super` environments; they are bound from the
# receiver. Expected labels are the results of the previous by-value storage.

r6_review_package_snapshot <- function(bindings) {
    ns <- list2env(bindings, parent = baseenv())
    input <- member_namespace_input(ns, package = "r6reviewfixture", exports = names(bindings))
    index <- member_package_index(input)
    if (exists("member_prepare_r6_roots", envir = asNamespace("languageserver"), inherits = FALSE)) {
        member_prepare_r6_roots(index, input, ns)
    } else {
        for (name in names(index$package_roots)) {
            value <- bindings[[name]]
            if (is.environment(value) && inherits(value, "R6ClassGenerator")) {
                index$package_roots[name] <- list(member_r6_runtime_shape(value, index))
            }
        }
    }
    index$roots <- member_public_roots(index, input)
    for (name in names(index$roots)) {
        index$namespace_roots[paste("r6reviewfixture", name, sep = "::")] <- list(index$roots[[name]])
    }
    member_index_freeze(index)
}

# Completion items at the "@@" marker, or at the end of the code.
r6_review_cursor_items <- function(code, snapshot = NULL) {
    content <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    row <- grep("@@", content, fixed = TRUE)
    if (length(row)) {
        col <- regexpr("@@", content[[row]], fixed = TRUE)[[1L]] - 1L
        content[[row]] <- sub("@@", "", content[[row]], fixed = TRUE)
        point <- list(row = row - 1L, col = col)
    } else {
        point <- list(row = length(content) - 1L, col = nchar(tail(content, 1L)))
    }
    uri <- "file:///r6-review.R"
    document <- Document$new(uri, language = "r", version = 1L, content = content)
    data <- parse_document(uri, content)
    data$version <- 1L
    document$update_parse_data(data)
    metadata <- collections::dict()
    if (!is.null(snapshot)) metadata$set("r6reviewfixture", member_index_thaw(snapshot))
    workspace <- list(member_metadata = metadata)
    member_completion(uri, workspace, document, point, TRUE, 200L)
}

# Completion labels at the "@@" marker, or at the end of the code.
r6_review_cursor_labels <- function(code, snapshot = NULL) {
    items <- r6_review_cursor_items(code, snapshot)
    sort(vapply(items, `[[`, character(1L), "label"))
}

# Every method value inside a shape, however deeply it is nested.
r6_review_methods <- function(value) {
    found <- list()
    walk <- function(x) {
        if (!is.list(x)) return(NULL)
        if (!is.null(x$function_expr)) found[[length(found) + 1L]] <<- x
        for (child in x) walk(child)
        NULL
    }
    walk(value)
    found
}

r6_review_family <- '
A <- R6::R6Class("A", public = list(
    x = NULL, v = NULL, w = NULL, flag = NULL,
    initialize = function(x = 1) { self$x <- x },
    run = function() private$h1(),
    direct = function() private$h2(),
    viaP = function() private$sec(),
    dflt = function() private$sec(),
    setflag = function(f) { if (f) self$flag <- 1 else self$flag <- "a"; self$viaP() },
    setpriv = function(f) { if (f) private$state <- 1; private$h1() },
    plain = function() list(plain = 1)
), private = list(
    state = NULL,
    h1 = function() private$h2(),
    h2 = function() list(deep = 1),
    sec = function() list(a_sec = 1)
))
B <- R6::R6Class("B", inherit = A, public = list(
    up = function() super$viaP(),
    own = function() private$sec()
), private = list(sec = function() list(b_sec = 1)))
C <- R6::R6Class("C", inherit = B, public = list(
    initialize = function(x = 1) { self$x <- x; self$v <- super$dflt(); self$w <- private$viah() },
    up = function() super$up()
), private = list(sec = function() list(c_sec = 1), viah = function() private$sec()))
'

test_that("R6 class shapes keep no private or super context inside their methods", {
    Root <- R6::R6Class("ShapeRoot", public = list(
        read = function() private$get(), who = function() list(root = TRUE)
    ), private = list(get = function() list(payload = TRUE), state = NULL))
    Middle <- R6::R6Class("ShapeMiddle", inherit = Root, public = list(
        read = function() super$read(), who = function() super$who()
    ), private = list(get = function() super$get()))
    Leaf <- R6::R6Class("ShapeLeaf", inherit = Middle, public = list(
        read = function() super$read(), who = function() super$who()
    ))
    index <- member_generic_index("")
    shape <- member_r6_runtime_shape(Leaf, index)
    instance <- shape$fields$new$result_shape
    methods <- r6_review_methods(shape)
    expect_gt(length(methods), 8L)
    for (method in methods) {
        expect_null(method$closure$private)
        expect_null(method$closure$super)
    }
    expect_null(instance$r6_super)
    expect_identical(instance$r6_level, 3L)
    expect_length(instance$r6_layers, 2L)
    expect_identical(names(instance$r6_layers[[1L]]$public), c("read", "who", "clone"))
    expect_identical(names(instance$r6_layers[[1L]]$private), c("state", "get"))
    expect_identical(names(instance$r6_layers[[2L]]$public), c("read", "who", "clone"))
    # "Mentions super" is decided once, when the shape is built.
    expect_true(instance$fields$read$r6_uses_super)
    expect_null(instance$r6_layers[[1L]]$public$read$r6_uses_super)
    snapshot <- r6_review_package_snapshot(list(Root = Root, Middle = Middle, Leaf = Leaf))
    expect_identical(r6_review_cursor_labels("r6reviewfixture::Leaf$new()$read()$", snapshot), "payload")
    expect_identical(r6_review_cursor_labels("r6reviewfixture::Leaf$new()$who()$", snapshot), "root")
})

test_that("A bound R6 method carries its contexts and no level bookkeeping", {
    index <- member_generic_index("")
    index$r6_attached <- TRUE
    bindings <- list()
    for (expr in str2expression(r6_review_family)) {
        bindings[as.character(expr[[2L]])] <- list(member_infer(expr[[3L]], index, bindings))
    }
    instance <- bindings$C$fields$new$result_shape
    expect_identical(instance$fields$up$r6_owner_level, 3L)
    expect_identical(instance$fields$viaP$r6_owner_level, 1L)
    bound <- member_access(instance, "up", index, list())
    expect_null(bound$r6_owner_level)
    expect_null(bound$r6_uses_super)
    expect_identical(names(bound$closure$private$fields), names(instance$r6_private$fields))
    expect_identical(bound$closure$super$r6_class$name, "B")
    expect_identical(sort(names(bound$closure$super$fields)),
        sort(c("initialize", "run", "direct", "viaP", "dflt", "setflag", "setpriv", "plain", "up", "own",
                "h1", "h2", "sec")))
    expect_identical(bound$closure$super$fields$sec$r6_owner$name, "B")
    # An inherited method gets the super of its declaring class, not of the receiver's class.
    inherited <- member_access(instance, "viaP", index, list())
    expect_null(inherited$closure$super)
    expect_identical(member_r6_super(instance$r6_layers, 2L)$r6_class$name, "A")
    expect_null(member_r6_super(instance$r6_layers, 1L)$fields)
})

test_that("Joined same-class R6 receivers keep the results of the declaring class's contexts", {
    # Pre-existing behaviour, unchanged by the storage change: a receiver
    # joined from two branches has no private table of its own, so a method
    # runs with the private table of the class that declares it. The R6
    # runtime would give `deep` for run() and `b_sec` for viaP() and up().
    join <- function(class, call) {
        r6_review_cursor_labels(paste0(r6_review_family,
                sprintf('o <- if (runif(1) > 0.5) %s$new(1) else %s$new("a")\no$%s$', class, class, call)))
    }
    expect_identical(join("A", "run()"), character())
    expect_identical(join("A", "direct()"), "deep")
    expect_identical(join("B", "viaP()"), "a_sec")
    expect_identical(join("B", "own()"), "b_sec")
    expect_identical(join("B", "up()"), "a_sec")
    expect_identical(join("B", "plain()"), "plain")
    expect_identical(r6_review_cursor_labels(paste0(r6_review_family,
                "o <- if (runif(1) > 0.5) A$new(1) else A$new(1)\no$run()$")), "deep")
    expect_identical(r6_review_cursor_labels(paste0(r6_review_family,
                'o <- if (runif(1) > 0.5) B$new(1) else B$new("a")\nf <- o$up\nf()$')), "a_sec")
})

test_that("Joined R6 receivers of different classes keep the results of shared methods", {
    # Pre-existing behaviour, unchanged by the storage change. A method both
    # classes inherit from the same ancestor runs with that ancestor's private
    # table: `a_sec` and `b_sec` below, where the R6 runtime gives `b_sec` or
    # `c_sec` depending on the branch.
    join <- function(call) {
        r6_review_cursor_labels(paste0(r6_review_family,
                sprintf("o <- if (runif(1) > 0.5) C$new(1) else B$new(1)\no$%s$", call)))
    }
    expect_identical(join("viaP()"), "a_sec")
    expect_identical(join("own()"), "b_sec")
    expect_identical(join("plain()"), "plain")
    expect_identical(join("run()"), character())
    # Overridden in one branch: not the same method any more.
    expect_identical(join("up()"), character())
    expect_identical(r6_review_cursor_labels(paste0(r6_review_family,
                "o <- if (runif(1) > 0.5) C$new(1) else B$new(1)\no$")),
        c("dflt", "direct", "flag", "initialize", "own", "plain", "run", "setflag", "setpriv", "up", "v", "viaP", "w", "x"))
})

test_that("R6 initializers and branch writes bind the contexts of the declaring class", {
    # Pre-existing behaviour, unchanged by the storage change: `super` and
    # `private` are not linked to the instance while initialize() is analysed,
    # so `super$dflt()` reads the parent's private table (`a_sec`; the R6
    # runtime gives `c_sec`).
    labels <- function(code) r6_review_cursor_labels(paste0(r6_review_family, code))
    expect_identical(labels("C$new()$v$"), "a_sec")
    expect_identical(labels("C$new()$w$"), "c_sec")
    expect_identical(labels("C$new()$up()$"), "c_sec")
    expect_identical(labels("C$new()$viaP()$"), "c_sec")
    expect_identical(labels("B$new()$setflag(runif(1) > 0.5)$"), "a_sec")
    expect_identical(labels("A$new()$setpriv(runif(1) > 0.5)$"), "deep")
    expect_identical(labels("C$new()$setpriv(runif(1) > 0.5)$"), "deep")
})

test_that("R6 super hides ancestor methods behind data fields as before", {
    code <- '
A <- R6::R6Class("A", public = list(
    m = function() list(a_m = 1), d = function() list(a_d = 1), keep = function() list(a_keep = 1)
), private = list(pm = function() list(a_pm = 1), pd = function() list(a_pd = 1)))
B <- R6::R6Class("B", inherit = A, public = list(d = list(data = 1)), private = list(pd = NULL))
C <- R6::R6Class("C", inherit = B, public = list(
    d = function() super$d(), m = function() super$m(), viapm = function() super$pm(), viapd = function() super$pd(),
    lst = function() super$keep()
))
D <- R6::R6Class("D", inherit = C, public = list(d = function() super$d(), viapd = function() super$pd()))
'
    labels <- function(call) r6_review_cursor_labels(paste0(code, call))
    expect_identical(labels("C$new()$m()$"), "a_m")
    expect_identical(labels("C$new()$viapm()$"), "a_pm")
    expect_identical(labels("D$new()$lst()$"), "a_keep")
    # A data field of the parent hides the grandparent's method (pre-existing).
    expect_identical(labels("C$new()$d()$"), character())
    expect_identical(labels("C$new()$viapd()$"), character())
    expect_identical(labels("D$new()$d()$"), character())
})

test_that("Outer super bindings and bare non-portable calls stay as conservative as before", {
    # A class without `inherit` still hides a file-level `super` object.
    outer_super_code <- 'super <- list(run = function() list(leak = TRUE))
A <- R6::R6Class("A", public = list(
  run = function() super$run()
))
'
    expect_identical(r6_review_cursor_labels(paste0(outer_super_code, "A$new()$")), "run")
    expect_identical(r6_review_cursor_labels(paste0(outer_super_code, "A$new()$run()$")), character())
    # A bare call of a non-portable method has contexts but no receiver.
    code <- 'P <- R6::R6Class("P", portable = FALSE, public = list(
  run = function() private$pp,
  who = function() list(pw = TRUE)
), private = list(pp = list(p = TRUE), ph = function() list(phv = TRUE)))
K <- R6::R6Class("K", inherit = P, portable = FALSE, public = list(
  field = list(f = TRUE),
  probe = function() {
    %s()$@@
  },
  who = function() list(kw = TRUE)
), private = list(payload = list(done = TRUE), helper = function() private$payload,
  viaself = function() self$field, viasuper = function() super$run()))'
    expect_identical(r6_review_cursor_labels(sprintf(code, "viaself")), character())
    expect_identical(r6_review_cursor_labels(sprintf(code, "helper")), "done")
    expect_identical(r6_review_cursor_labels(sprintf(code, "viasuper")), "p")
})

test_that("Bound R6 methods stored in another instance keep their own class contexts", {
    code <- '
Tool <- R6::R6Class("Tool", public = list(run = function() private$t(), up = function() list(tool = 1)),
    private = list(t = function() list(tool_t = 1)))
Sub <- R6::R6Class("Sub", inherit = Tool, public = list(up = function() super$up()),
    private = list(t = function() list(sub_t = 1)))
Holder <- R6::R6Class("Holder", public = list(
    cb = NULL, cb2 = NULL, tool = NULL,
    initialize = function() { self$tool <- Sub$new(); self$cb <- self$tool$run; self$cb2 <- self$tool$up },
    callcb = function() self$cb(), callcb2 = function() self$cb2(), viatool = function() self$tool$run()
), private = list(t = function() list(holder_t = 1)))
'
    labels <- function(call) r6_review_cursor_labels(paste0(code, call))
    expect_identical(labels("Holder$new()$viatool()$"), "sub_t")
    expect_identical(labels("Holder$new()$callcb2()$"), "tool")
    # Pre-existing: the holder becomes the receiver, so its private table is used.
    expect_identical(labels("Holder$new()$callcb()$"), "holder_t")
})

test_that("member_shape_key distinguishes receivers that differ in an inherited level", {
    # End-to-end completion: Reduce() fixpoint must not treat two receivers as
    # interchangeable when only an inherited level's method differs.
    reduce_code <- '
Root1 <- R6::R6Class("Root1", public = list(m = function() list(r1 = 1)))
Root2 <- R6::R6Class("Root2", public = list(m = function() list(r2 = 1)))
make_leaf <- function(R) R6::R6Class("Leaf", inherit = R, public = list(m = function() super$m()))
L1 <- make_leaf(Root1)
L2 <- make_leaf(Root2)
'
    expect_identical(
        r6_review_cursor_labels(paste0(
            reduce_code,
            "Reduce(function(acc, x) L1$new(), unknown_xs, init = L1$new())$m()$"
        )),
        "r1"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            reduce_code,
            "Reduce(function(acc, x) L2$new(), unknown_xs, init = L1$new())$m()$"
        )),
        character()
    )
    make <- function(payload) {
        Root <- R6::R6Class("KeyRoot", public = list(read = payload), private = list(state = NULL))
        R6::R6Class("KeyLeaf", inherit = Root, public = list(read = function() super$read()))
    }
    index <- member_generic_index("")
    first <- member_r6_runtime_shape(make(function() list(a = 1)), index)$fields$new$result_shape
    again <- member_r6_runtime_shape(make(function() list(a = 1)), index)$fields$new$result_shape
    expect_identical(member_shape_key(first), member_shape_key(again))
    # Same visible members; the root declares one more private member.
    Root <- R6::R6Class("KeyRoot", public = list(read = function() list(a = 1)),
        private = list(state = NULL, extra = function() NULL))
    other <- member_r6_runtime_shape(
        R6::R6Class("KeyLeaf", inherit = Root, public = list(read = function() super$read())),
        index)$fields$new$result_shape
    other$r6_private <- first$r6_private
    expect_identical(names(other$fields), names(first$fields))
    expect_false(identical(member_shape_key(first), member_shape_key(other)))
    root_only <- member_r6_runtime_shape(R6::R6Class("KeyRootOnly"), index)$fields$new$result_shape
    expect_identical(member_shape_key(root_only)$r6_super$r6_role, "super")
    # Overridden ancestor private data literals are absent from both leaf
    # `r6_private` and `r6_super`, whereas non-overridden ones remain in
    # `r6_private` (matching by-value `r6_private` + `r6_super` keying).
    ov_code <- '
P1 <- R6::R6Class("P", private = list(secret = 1L))
P2 <- R6::R6Class("P", private = list(secret = 2L))
C1 <- R6::R6Class("C", inherit = P1, public = list(ans = function() list(from_c1 = TRUE)), private = list(secret = 99L))
C2 <- R6::R6Class("C", inherit = P2, public = list(ans = function() list(from_c2 = TRUE)), private = list(secret = 99L))
N1 <- R6::R6Class("C", inherit = P1, public = list(ans = function() list(from_n1 = TRUE)))
N2 <- R6::R6Class("C", inherit = P2, public = list(ans = function() list(from_n2 = TRUE)))
'
    expect_identical(
        r6_review_cursor_labels(paste0(
            ov_code,
            "Reduce(function(acc, el) C2$new(), unknown_xs, init = C1$new())$ans()$"
        )),
        "from_c1"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            ov_code,
            "Reduce(function(acc, el) N2$new(), unknown_xs, init = N1$new())$ans()$"
        )),
        character()
    )
    # Internal level numbers, masked grandparents, and carried layers on bound
    # contexts do not split Reduce() fixed points when visible surfaces match.
    depth_code <- '
G <- R6::R6Class("P", public = list(gm = function() list(from_g = 1)))
P_direct <- R6::R6Class("P", public = list(gm = function() list(from_p = 1)))
P_sub <- R6::R6Class("P", inherit = G)
D1 <- R6::R6Class("Leaf", inherit = P_direct, public = list(ans = function() list(from_d1 = 1)))
D2 <- R6::R6Class("Leaf", inherit = P_sub, public = list(ans = function() list(from_d2 = 1)))
R0 <- R6::R6Class("R0")
R1 <- R6::R6Class("R1", inherit = R0)
make_p <- function(v, P) R6::R6Class("P", inherit = P, public = list(v = v, gp = function() private), private = list(s = list(ok_s = 1)))
ja <- if (cnd) make_p(1L, R0)$new() else make_p(2L, R0)$new()
jb <- if (cnd) make_p(1L, R1)$new() else make_p(2L, R1)$new()
'
    expect_identical(
        r6_review_cursor_labels(paste0(
            depth_code,
            "Reduce(function(acc, x) D2$new(), unknown_xs, init = D1$new())$ans()$"
        )),
        "from_d1"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            depth_code,
            "Reduce(function(acc, x) jb$gp(), unknown_xs, init = ja$gp())$s$"
        )),
        "ok_s"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            'G1 <- R6::R6Class("G", public = list(gm = function() list(g1 = 1)))\n',
            'G2 <- R6::R6Class("G", public = list(gm = function() list(g2 = 1)))\n',
            "make_chain <- function(G) {\n",
            '    M <- R6::R6Class("M", inherit = G, public = list(gm = function() super$gm()))\n',
            '    R6::R6Class("Leaf", inherit = M, public = list(top = function() super$gm()))\n',
            "}\n",
            "Reduce(function(acc, x) make_chain(G2)$new(), unknown_xs, init = make_chain(G1)$new())$top()$"
        )),
        "g1"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            'A <- R6::R6Class("A", public = list(\n',
            "    get_priv = function() private,\n",
            "    probe = function(xs) Reduce(function(acc, el) self$get_priv(), xs, init = private)$secret$@@\n",
            "), private = list(secret = list(ok_sec = 1)))"
        )),
        "ok_sec"
    )
    # Stored-instance `r6_visited` trails and `r6_context_ids`/`r6_contexts`
    # mirror method closures omitted by `member_shape_key()`, while an actual
    # cycle-cut return value still differs in `type` and `fields`.
    PVis <- R6::R6Class("PVis", cloneable = FALSE,
        public = list(pm = function() private$x(), get_pp = function() private),
        private = list(x = function() list(ok_x = 1L)))
    KVis <- R6::R6Class("KVis", cloneable = FALSE, inherit = PVis,
        public = list(get_sup = function() super, via_priv_sup = function() private$h()),
        private = list(h = function() super))
    KM1 <- R6::R6Class("KM", cloneable = FALSE, public = list(m = function() {
        private
        list(from_m1 = 1L)
    }), private = list(p = 1L))
    KM2 <- R6::R6Class("KM", cloneable = FALSE,
        public = list(m = function() list(from_m2 = 1L)),
        private = list(p = 1L))
    vis_snap <- r6_review_package_snapshot(list(
        obj_vis = KVis$new(),
        holders = list(m1 = KM1$new(), m2 = KM2$new())
    ))
    expect_identical(
        r6_review_cursor_labels(
            "Reduce(function(acc, x) r6reviewfixture::obj_vis$via_priv_sup(), unknown_xs, init = r6reviewfixture::obj_vis$get_sup())$pm()$",
            vis_snap
        ),
        "ok_x"
    )
    expect_identical(
        r6_review_cursor_labels(
            "Reduce(function(acc, x) r6reviewfixture::obj_vis$via_priv_sup()$get_pp(), unknown_xs, init = r6reviewfixture::obj_vis$get_sup()$get_pp())$x()$",
            vis_snap
        ),
        character()
    )
    expect_identical(
        r6_review_cursor_labels(
            "Reduce(function(acc, el) r6reviewfixture::holders$m2, unknown_xs, init = r6reviewfixture::holders$m1)$m()$",
            vis_snap
        ),
        "from_m1"
    )
})

test_that("member_r6_join drops same-syntax methods whose private or super contexts differ", {
    # Same class name and identical method AST (from one factory), different private tables.
    priv_code <- '
make <- function(v) R6::R6Class("A", public = list(m = function() private$p), private = list(p = v))
'
    same_join <- "o <- if (cond) make(list(same = 1))$new() else make(list(same = 1))$new()\n"
    diff_join <- "o <- if (cond) make(list(only1 = 1))$new() else make(list(only2 = 1))$new()\n"
    expect_identical(r6_review_cursor_labels(paste0(priv_code, same_join, "o$m()$")), "same")
    expect_identical(r6_review_cursor_labels(paste0(priv_code, diff_join, "o$m()$")), character())
    same_items <- r6_review_cursor_items(paste0(priv_code, same_join, "o$"))
    diff_items <- r6_review_cursor_items(paste0(priv_code, diff_join, "o$"))
    same_m <- same_items[[match("m", vapply(same_items, `[[`, character(1L), "label"))]]
    diff_m <- diff_items[[match("m", vapply(diff_items, `[[`, character(1L), "label"))]]
    expect_identical(same_m$kind, CompletionItemKind$Method)
    expect_identical(same_m$detail, "m()")
    expect_identical(diff_m$kind, CompletionItemKind$Field)
    expect_identical(diff_m$detail, "[static member]")
    # Same subclass declaration, different direct `super` tables.
    sup_code <- '
P1 <- R6::R6Class("P", public = list(pm = function() list(p1 = 1)))
P2 <- R6::R6Class("P", public = list(pm = function() list(p2 = 1)))
make_sub <- function(P) R6::R6Class("B", inherit = P, public = list(up = function() super$pm()))
'
    sup_same <- "o <- if (cond) make_sub(P1)$new() else make_sub(P1)$new()\no$up()$"
    sup_diff <- "o <- if (cond) make_sub(P1)$new() else make_sub(P2)$new()\no$up()$"
    expect_identical(r6_review_cursor_labels(paste0(sup_code, sup_same)), "p1")
    expect_identical(r6_review_cursor_labels(paste0(sup_code, sup_diff)), character())
    # Same immediate parent syntax (`M`), differing only in a deeper overridden ancestor (`G`).
    deep_code <- '
G1 <- R6::R6Class("G", public = list(gm = function() list(g1 = 1)))
G2 <- R6::R6Class("G", public = list(gm = function() list(g2 = 1)))
make_chain <- function(G) {
  M <- R6::R6Class("M", inherit = G, public = list(gm = function() super$gm()))
  R6::R6Class("Leaf", inherit = M, public = list(top = function() super$gm()))
}
'
    deep_same <- "o <- if (cond) make_chain(G1)$new() else make_chain(G1)$new()\no$top()$"
    deep_diff <- "o <- if (cond) make_chain(G1)$new() else make_chain(G2)$new()\no$top()$"
    expect_identical(r6_review_cursor_labels(paste0(deep_code, deep_same)), "g1")
    expect_identical(r6_review_cursor_labels(paste0(deep_code, deep_diff)), character())
})

test_that("Stored R6 instances keep each method context once and scale linearly in width", {
    make <- function(n) {
        public <- lapply(seq_len(n), function(i) eval(parse(text = sprintf("function() private$p%d()", i))))
        names(public) <- paste0("m", seq_len(n))
        private <- lapply(seq_len(n), function(i) eval(parse(text = sprintf("function() list(v%d = 1)", i))))
        names(private) <- paste0("p", seq_len(n))
        R6::R6Class("StoredWide", public = public, private = private)$new()
    }
    snapshot <- r6_review_snapshot(make(40L))
    expect_lt(as.numeric(utils::object.size(snapshot)), 16 * 1024^2)
    expect_identical(r6_review_labels("r6reviewfixture::object$m7()$", snapshot), "v7")
    expect_identical(r6_review_labels("r6reviewfixture::object$m40()$", snapshot), "v40")
    object <- snapshot$roots$object
    expect_length(object$r6_contexts, 1L)
    priv_ids <- unique(vapply(paste0("m", 1:40), function(name) {
        method <- object$fields[[name]]
        expect_null(method$closure$private)
        method$r6_context_ids[["private"]]
    }, character(1L)))
    expect_length(priv_ids, 1L)
    expect_true(priv_ids[[1L]] %in% names(object$r6_contexts))
    expect_identical(object$r6_contexts[[priv_ids[[1L]]]]$r6_role, "private")
    # Private methods referencing `private` must not inline per-method cycle
    # placeholders carrying n-element `r6_bindings` (quadratic width regression).
    make_priv_chain <- function(n) {
        pub <- stats::setNames(lapply(seq_len(n), function(i) {
            eval(parse(text = sprintf("function() private$p%d()", i)))
        }), paste0("m", seq_len(n)))
        prv <- stats::setNames(lapply(seq_len(n), function(i) {
            eval(parse(text = sprintf("function() private$p%d()", i %% n + 1L)))
        }), paste0("p", seq_len(n)))
        R6::R6Class("StoredPrivChain", public = pub, private = prv, cloneable = FALSE)$new()
    }
    shape_100 <- member_snapshot_shape(member_snapshot(make_priv_chain(100L)))
    shape_200 <- member_snapshot_shape(member_snapshot(make_priv_chain(200L)))
    for (pm in shape_200$r6_contexts[[1L]]$fields) {
        expect_null(pm$closure$private)
    }
    ratio <- as.numeric(utils::object.size(shape_200)) / as.numeric(utils::object.size(shape_100))
    expect_lt(ratio, 2.2)
    # Two equal instances decode to equal values: identifiers are local.
    ns <- new.env(parent = baseenv())
    ns$first <- make(3L)
    ns$second <- make(3L)
    shape <- member_snapshot_shape(member_snapshot(ns), "r6reviewfixture")
    expect_identical(shape$fields$first, shape$fields$second)
})

test_that("Stored instances of deep R6 classes fit the budget and keep super chains", {
    # Four levels with 6 super-calling methods per level exceed the 32 MB
    # budget with the previous code when an instance is snapshotted, and stay small here.
    env <- new.env(parent = baseenv())
    eval(parse(text = '
C1 <- R6::R6Class("C1", public = list(
    m1 = function() private$get(), m2 = function() list(v2 = TRUE),
    m3 = function() list(v3 = TRUE), m4 = function() list(v4 = TRUE),
    m5 = function() list(v5 = TRUE), m6 = function() list(v6 = TRUE),
    run = function() self$m2()
), private = list(get = function() list(root_get = TRUE)))
C2 <- R6::R6Class("C2", inherit = C1, public = list(
    m1 = function() super$m1(), m2 = function() list(from_c2 = TRUE),
    m3 = function() super$m3(), m4 = function() super$m4(),
    m5 = function() super$m5(), m6 = function() super$m6()
))
C3 <- R6::R6Class("C3", inherit = C2, public = list(
    m1 = function() super$m1(), m2 = function() super$m2(),
    m3 = function() super$m3(), m4 = function() super$m4(),
    m5 = function() super$m5(), m6 = function() super$m6()
))
C4 <- R6::R6Class("C4", inherit = C3, public = list(
    m1 = function() super$m1(), m2 = function() super$m2(),
    m3 = function() super$m3(), m4 = function() super$m4(),
    m5 = function() super$m5(), m6 = function() super$m6()
), private = list(get = function() list(leaf = TRUE)))
', keep.source = FALSE), env)
    snapshot <- NULL
    expect_no_error(snapshot <- r6_review_snapshot(env$C4$new()))
    expect_lt(as.numeric(utils::object.size(snapshot)), 16 * 1024^2)
    expect_identical(r6_review_labels("r6reviewfixture::object$m2()$", snapshot), "from_c2")
    expect_identical(r6_review_labels("r6reviewfixture::object$run()$", snapshot), "from_c2")
    # Pre-existing snapshot depth limit (max_depth = 4): private environment at
    # the root of a 4-level stored instance is beyond max_depth, giving nothing.
    expect_identical(r6_review_labels("r6reviewfixture::object$m1()$", snapshot), character())
    expect_true(all(c("m1", "m2", "m6", "run") %in% r6_review_labels("r6reviewfixture::object$", snapshot)))
    # Three levels, a private method calling super (cut at the outer private cycle).
    G <- R6::R6Class("G", public = list(read = function() private$helper(), ok = function() list(g_ok = TRUE)),
        private = list(helper = function() list(g = TRUE)))
    H <- R6::R6Class("H", inherit = G, private = list(helper = function() super$helper()))
    I <- R6::R6Class("I", inherit = H, private = list(helper = function() super$helper()))
    expect_no_error(snapshot <- r6_review_snapshot(I$new()))
    expect_lt(as.numeric(utils::object.size(snapshot)), 16 * 1024^2)
    expect_identical(r6_review_labels("r6reviewfixture::object$ok()$", snapshot), "g_ok")
    expect_identical(r6_review_labels("r6reviewfixture::object$read()$", snapshot), character())
    # Six levels where private methods call super: single-decode per context env
    # gives exactly 6 entries in `r6_contexts` (not geometric subset copies).
    make_deep_priv_super <- function(depth) {
        parent <- NULL
        for (d in seq_len(depth)) {
            pub <- list(over = if (d == 1L) function() list(root_over = TRUE) else function() super$over())
            prv <- list(p = function() list(v = 1))
            prv[[paste0("h", d)]] <- if (d == 1L) function() private$p() else function() super$over()
            pub[[paste0("m", d)]] <- eval(parse(text = sprintf("function() private$h%d()", d)))
            args <- list(classname = paste0("D", d), public = pub, private = prv)
            scope <- new.env(parent = baseenv())
            if (!is.null(parent)) {
                scope$Parent <- parent
                args$inherit <- quote(Parent)
            }
            parent <- do.call(R6::R6Class, c(args, list(parent_env = scope)))
        }
        parent$new()
    }
    deep6 <- make_deep_priv_super(6L)
    expect_no_error(snapshot <- r6_review_snapshot(deep6))
    expect_lt(as.numeric(utils::object.size(snapshot)), 16 * 1024^2)
    expect_length(snapshot$roots$object$r6_contexts, 6L)
    expect_identical(r6_review_labels("r6reviewfixture::object$over()$", snapshot), "root_over")
})

test_that("Stored R6 instances cut context cycles where they were cut before", {
    make <- function() {
        P <- R6::R6Class("P", public = list(
            pm = function() private$x(), who = function() list(pw = TRUE)
        ), private = list(x = function() list(a = 1)))
        K <- R6::R6Class("K", inherit = P, public = list(
            run = function() private$h(), direct = function() super$pm(), viaself = function() self$direct()
        ), private = list(h = function() super$pm()))
        K$new()
    }
    snapshot <- r6_review_snapshot(make())
    expect_identical(r6_review_labels("r6reviewfixture::object$direct()$", snapshot), "a")
    expect_identical(r6_review_labels("r6reviewfixture::object$viaself()$", snapshot), "a")
    expect_identical(r6_review_labels("f <- r6reviewfixture::object$direct\nf()$", snapshot), "a")
    # Pre-existing behaviour, unchanged by the storage change: the private
    # environment is already visited when the parent method is reached through
    # a private method, so that method's private context is cut as a cycle.
    expect_identical(r6_review_labels("r6reviewfixture::object$run()$", snapshot), character())
    index <- member_index_thaw(snapshot)
    object <- index$roots$object
    expect_length(object$r6_contexts, 2L)
    bound_direct <- member_access(object, "direct", index, list())
    bound_direct_pm <- member_access(bound_direct$closure$super, "pm", index, list())
    expect_null(bound_direct_pm$closure$private$reason)
    bound_run <- member_access(object, "run", index, list())
    bound_h <- member_access(bound_run$closure$private, "h", index, list())
    bound_cycle_pm <- member_access(bound_h$closure$super, "pm", index, list())
    expect_identical(bound_cycle_pm$closure$private$reason, "cycle")
})

test_that("Generated R6 clone methods are stored as a stub and nothing else is", {
    Plain <- R6::R6Class("ClonePlain", public = list(
        expr = quote(function() list(fake = TRUE)),
        read = function() list(real = TRUE)
    ))
    Custom <- R6::R6Class("CloneCustom", public = list(read = function() list(real = TRUE)))
    Custom$set("public", "clone", function(deep = FALSE) list(custom = TRUE), overwrite = TRUE)
    # A user-defined $set clone mentioning .__enclos_env__ and deep_clone must NOT be stubbed.
    CustomDeep <- R6::R6Class("CloneCustomDeep", public = list(read = function() list(real = TRUE)))
    CustomDeep$set("public", "clone", function(deep = FALSE) {
        e <- self$.__enclos_env__
        if (deep) e$private$deep_clone
        list(custom_deep = TRUE)
    }, overwrite = TRUE)
    # Even wrapping R6's own clone body via $set() is a custom method, not R6's generated one.
    Wrapped <- R6::R6Class("CloneWrapped", public = list(read = function() list(real = TRUE)))
    wrapped_fn <- Wrapped$public_methods$clone
    body(wrapped_fn) <- call("{", body(wrapped_fn), quote(list(wrapped = TRUE)))
    Wrapped$set("public", "clone", wrapped_fn, overwrite = TRUE)
    Fixed <- R6::R6Class("CloneFixed", cloneable = FALSE, public = list(read = function() list(real = TRUE)))
    # Run base-parity completion checks before referencing internal stub helpers.
    snapshot <- r6_review_package_snapshot(list(
        Plain = Plain, Custom = Custom, CustomDeep = CustomDeep,
        Wrapped = Wrapped, Fixed = Fixed,
        plain_inst = Plain$new(), custom_deep_inst = CustomDeep$new()
    ))
    labels <- function(code) r6_review_cursor_labels(code, snapshot)
    expect_identical(labels("r6reviewfixture::Plain$new()$"), c("clone", "expr", "read"))
    expect_identical(labels("r6reviewfixture::Plain$new()$clone()$"), character())
    expect_identical(labels("r6reviewfixture::Plain$new()$expr()$"), character())
    expect_identical(labels("r6reviewfixture::Plain$new()$read()$"), "real")
    expect_identical(labels("r6reviewfixture::Custom$new()$clone()$"), "custom")
    expect_identical(labels("r6reviewfixture::CustomDeep$new()$clone()$"), "custom_deep")
    expect_identical(labels("r6reviewfixture::Wrapped$new()$clone()$"), "wrapped")
    expect_identical(labels("r6reviewfixture::plain_inst$clone()$"), character())
    expect_identical(labels("r6reviewfixture::custom_deep_inst$clone()$"), "custom_deep")
    expect_identical(labels(paste0(
        'Sub <- R6::R6Class("Sub", inherit = r6reviewfixture::CustomDeep, public = list(y = function() self$clone()))\n',
        "Sub$new()$y()$"
    )), "custom_deep")
    expect_identical(labels("r6reviewfixture::Fixed$new()$"), "read")
    # A binary package built against another R6 version carries that version's
    # `clone_method` on the generator and in `public_methods$clone`.
    Skewed <- R6::R6Class("CloneSkewed", public = list(read = function() list(real = TRUE)))
    generated <- Skewed$clone_method
    body(generated) <- call("{", quote(other_version <- TRUE), body(generated))
    environment(generated) <- Skewed
    Skewed$clone_method <- generated
    Skewed$public_methods$clone <- generated
    # Source references (`keep.source = TRUE`) must not defeat matching R6's clone_method.
    with_srcref <- eval(parse(
        text = deparse(asNamespace("R6")$generator_funs$clone_method),
        keep.source = TRUE
    ))
    index <- member_generic_index("")
    stub <- member_function_syntax(member_r6_clone_stub())
    clone_of <- function(generator) member_r6_runtime_shape(generator, index)$fields$new$result_shape$fields$clone
    expect_identical(clone_of(Plain)$function_expr, stub)
    expect_identical(clone_of(Skewed)$function_expr, stub)
    expect_identical(clone_of(Plain)$r6_member_kind, "public method")
    expect_identical(clone_of(Custom)$function_expr, member_function_syntax(Custom$public_methods$clone))
    expect_identical(clone_of(CustomDeep)$function_expr, member_function_syntax(CustomDeep$public_methods$clone))
    expect_identical(clone_of(Wrapped)$function_expr, member_function_syntax(Wrapped$public_methods$clone))
    expect_null(clone_of(Fixed))
    # Stored instances also stub R6's generated clone in both roots and definitions.
    expect_identical(snapshot$roots$plain_inst$fields$clone$function_expr, stub)
    expect_identical(snapshot$definitions[["plain_inst$clone"]], stub)
    expect_lt(as.numeric(utils::object.size(snapshot$roots$plain_inst$fields$clone)), 8 * 1024)
    # A field that holds code is data, whatever the code looks like.
    expr <- member_r6_runtime_shape(Plain, index)$fields$new$result_shape$fields$expr
    expect_null(expr$function_expr)
    expect_identical(expr$r6_member_kind, "public field")
    expect_true(member_r6_generated_clone(Plain$public_methods$clone, Plain))
    expect_true(member_r6_generated_clone(Skewed$public_methods$clone))
    expect_true(member_r6_generated_clone(with_srcref))
    expect_false(member_r6_generated_clone(Custom$public_methods$clone, Custom))
    expect_false(member_r6_generated_clone(CustomDeep$public_methods$clone, CustomDeep))
    expect_false(member_r6_generated_clone(Wrapped$public_methods$clone, Wrapped))
    expect_false(member_r6_generated_clone(Plain$public_fields$expr, Plain))
    expect_lt(as.numeric(utils::object.size(clone_of(Plain))), 8 * 1024)
})

test_that("Unnamed elements in R6 class definitions do not break inference or hover", {
    trailing_code <- '
A <- R6::R6Class("A", public = list(
    initialize = function() NULL,
    f = function(x) list(aa = 1),
), private = list(
    2,
    p = function() list(pp = 1),
), active = list(
    function() 1,
    act = function() 1
))
B <- R6::R6Class("B", inherit = A, public = list(
    1,
    up = function() super$f(1),
    priv = function() super$p()
))
'
    expect_identical(r6_review_cursor_labels(paste0(trailing_code, "A$new()$f(1)$")), "aa")
    expect_identical(r6_review_cursor_labels(paste0(trailing_code, "B$new()$up()$")), "aa")
    expect_identical(r6_review_cursor_labels(paste0(trailing_code, "B$new()$priv()$")), "pp")
    join_code <- paste0(trailing_code, "o <- if (cond) B$new() else B$new()\no$up()$")
    expect_identical(r6_review_cursor_labels(join_code), "aa")
    hover_lines <- c(strsplit(trailing_code, "\n", fixed = TRUE)[[1L]], "A$new()$f(1)")
    uri <- "file:///r6-unnamed-hover.R"
    doc <- Document$new(uri, language = "r", version = 1L, content = hover_lines)
    data <- parse_document(uri, hover_lines)
    data$version <- 1L
    doc$update_parse_data(data)
    ws <- list(member_metadata = collections::dict(), get_documentation = function(...) NULL,
        get_signature = function(...) NULL, get_help = function(...) NULL)
    hover <- hover_reply(1L, uri, ws, doc, list(row = length(hover_lines) - 1L, col = 8L))$result
    expect_identical(hover$contents[[1L]], "```r\nf(x)\n```")
    expect_identical(hover$contents[[2L]], "Public method of R6 class `A`")
})

test_that("Bound R6 methods stored in lists and containers keep super and private chains", {
    chain_code <- '
A <- R6::R6Class("A", cloneable = FALSE, public = list(
    viaO = function() private$only_a(),
    viaP = function() private$sec(),
    pub = function() list(a_pub = 1),
    deep2 = function() private$p1()
), private = list(
    only_a = function() list(only_a = 1),
    sec = function() list(a_sec = 1),
    p1 = function() private$p2(),
    p2 = function() list(ok_2hop = 1)
))
B <- R6::R6Class("B", inherit = A, cloneable = FALSE, public = list(
    upO = function() super$viaO(),
    upP = function() super$viaP(),
    upPub = function() super$pub(),
    deep = function() private$ph(),
    deepPub = function() private$phPub()
), private = list(
    sec = function() list(b_sec = 1),
    ph = function() super$viaO(),
    phPub = function() super$pub()
))
C <- R6::R6Class("C", inherit = B, cloneable = FALSE, public = list(
    up3 = function() super$upO(),
    up3Pub = function() super$upPub()
))
Holder <- R6::R6Class("Holder", cloneable = FALSE, public = list(
    cb = NULL,
    initialize = function(fn) { self$cb <- fn }
), private = list(only_a = function() list(h_only_a = 1)))
'
    labels <- function(call) r6_review_cursor_labels(paste0(chain_code, call))
    expect_identical(labels("l <- list(f = B$new()$upO)\nl$f()$"), "only_a")
    expect_identical(labels('l <- list(f = B$new()$upO)\nl[["f"]]()$'), "only_a")
    expect_identical(labels("l <- list(f = B$new()$upP)\nl$f()$"), "a_sec")
    expect_identical(labels("cfg <- list(cb = list(done = B$new()$upO))\ncfg$cb$done()$"), "only_a")
    expect_identical(labels("wrap <- function() { fn <- B$new()$upO; fn() }\nwrap()$"), "only_a")
    expect_identical(labels("l <- list(f = C$new()$up3)\nl$f()$"), "only_a")
    expect_identical(labels("Holder$new(C$new()$up3)$cb()$"), "h_only_a")
    expect_identical(labels("l <- list(f = B$new()$deep)\nl$f()$"), "only_a")
    expect_identical(labels("l <- list(f = A$new()$deep2)\nl$f()$"), "ok_2hop")
    # Runtime generators and stored R6 instances in a package snapshot.
    rt_env <- new.env(parent = baseenv())
    eval(parse(text = chain_code, keep.source = FALSE), rt_env)
    UnlockedHolder <- R6::R6Class("UnlockedHolder", lock_objects = FALSE, public = list(cb = NULL),
        private = list(only_a = function() list(wrong_holder = 1)))
    snapshot <- r6_review_package_snapshot(list(
        A = rt_env$A, B = rt_env$B, C = rt_env$C,
        objB = rt_env$B$new(), objC = rt_env$C$new(), uhold = UnlockedHolder$new()
    ))
    pkg_labels <- function(call) r6_review_cursor_labels(call, snapshot)
    expect_identical(pkg_labels("l <- list(f = r6reviewfixture::B$new()$upO)\nl$f()$"), "only_a")
    expect_identical(pkg_labels("l <- list(f = r6reviewfixture::objB$upO)\nl$f()$"), "only_a")
    expect_identical(pkg_labels("l <- list(f = r6reviewfixture::objC$up3)\nl$f()$"), "only_a")
    expect_identical(pkg_labels("l <- list(f = r6reviewfixture::objC$up3Pub)\nl$f()$"), "a_pub")
    expect_identical(pkg_labels("l <- list(f = r6reviewfixture::objB$deepPub)\nl$f()$"), "a_pub")
    expect_identical(pkg_labels("l <- list(f = r6reviewfixture::objB$deep)\nl$f()$"), character())
    expect_identical(
        pkg_labels(paste0(chain_code, "Holder$new(r6reviewfixture::objB$upO)$cb()$")),
        "only_a"
    )
    expect_identical(
        pkg_labels("f <- function() {\n  h <- r6reviewfixture::uhold\n  h$cb <- r6reviewfixture::objB$upO\n  h$cb()\n}\nf()$"),
        "only_a"
    )
})

test_that("Joins of nested and multi-trail stored R6 instances keep shared methods and item kinds", {
    # Nested stored instances sharing `private` while differing in `super`.
    K1a <- R6::R6Class("X", public = list(
        n = function() list(k1a_n = TRUE), m = function() private$p
    ), private = list(p = list(k1_p = TRUE)))
    K1b <- R6::R6Class("X", public = list(
        n = function() list(k1b_n = TRUE), m = function() private$p
    ), private = list(p = list(k1_p = TRUE)))
    SubA <- R6::R6Class("X", inherit = K1a, public = list(q = function() super$n()))
    SubB <- R6::R6Class("X", inherit = K1b, public = list(q = function() super$n()))
    en <- new.env(parent = emptyenv())
    en$a <- SubA$new()
    en$b <- SubB$new()
    # Differentiated vs matching `private$nm` reached through `super`:
    PState <- R6::R6Class("PC", public = list(n = function() list(k1_n = TRUE)))
    CState <- R6::R6Class("CC", inherit = PState, public = list(
        initialize = function(nm) private$nm <- nm, q = function() super$n()
    ), private = list(nm = NULL))
    # Same stored instance reached along two outer trails (`H$x` and `H$inner$x`).
    Own <- R6::R6Class("Own", public = list(
        initialize = function(owner) private$owner <- owner,
        m = function() private$tag(),
        leaf_m = function() private$owner$leaf
    ), private = list(owner = NULL, tag = function() list(tg = 1)))
    H <- new.env(parent = emptyenv())
    H$inner <- new.env(parent = emptyenv())
    X <- Own$new(H)
    H$x <- X
    H$inner$x <- X
    H2 <- new.env(parent = emptyenv())
    H2$inner <- new.env(parent = emptyenv())
    H2$inner$leaf <- list(ok_leaf = 1)
    X2 <- Own$new(H2$inner)
    H2$x <- X2
    H2$inner$x <- X2
    snapshot <- r6_review_package_snapshot(list(
        lst = list(a = SubA$new(), b = SubB$new()),
        en = en,
        diff_state = list(a = CState$new("x"), b = CState$new("y")),
        same_state = list(a = CState$new("x"), b = CState$new("x")),
        H = H, H2 = H2
    ))
    expect_identical(
        r6_review_cursor_labels("o <- if (cnd) r6reviewfixture::lst$a else r6reviewfixture::lst$b\no$m()$", snapshot),
        "k1_p"
    )
    expect_identical(
        r6_review_cursor_labels("o <- if (cnd) r6reviewfixture::en$a else r6reviewfixture::en$b\no$m()$", snapshot),
        "k1_p"
    )
    expect_identical(
        r6_review_cursor_labels("o <- if (cnd) r6reviewfixture::lst$a else r6reviewfixture::lst$b\no$q()$", snapshot),
        character()
    )
    items <- r6_review_cursor_items("o <- if (cnd) r6reviewfixture::lst$a else r6reviewfixture::lst$b\no$", snapshot)
    item_m <- items[[match("m", vapply(items, `[[`, character(1L), "label"))]]
    item_q <- items[[match("q", vapply(items, `[[`, character(1L), "label"))]]
    expect_identical(item_m$kind, CompletionItemKind$Method)
    expect_identical(item_m$detail, "m()")
    expect_identical(item_q$kind, CompletionItemKind$Field)
    expect_identical(item_q$detail, "[static member]")
    expect_identical(
        r6_review_cursor_labels(
            "o <- if (cnd) r6reviewfixture::diff_state$a else r6reviewfixture::diff_state$b\no$q()$",
            snapshot
        ),
        character()
    )
    expect_identical(
        r6_review_cursor_labels(
            "o <- if (cnd) r6reviewfixture::same_state$a else r6reviewfixture::same_state$b\no$q()$",
            snapshot
        ),
        "k1_n"
    )
    expect_identical(snapshot$roots$H$fields$x, snapshot$roots$H$fields$inner$fields$x)
    expect_identical(
        r6_review_cursor_labels("o <- if (cnd) r6reviewfixture::H$x else r6reviewfixture::H$inner$x\no$m()$", snapshot),
        "tg"
    )
    expect_identical(r6_review_cursor_labels("r6reviewfixture::H2$x$leaf_m()$", snapshot), "ok_leaf")
    expect_identical(r6_review_cursor_labels("r6reviewfixture::H2$inner$x$leaf_m()$", snapshot), character())
    expect_identical(
        r6_review_cursor_labels(
            "o <- if (cnd) r6reviewfixture::H2$x else r6reviewfixture::H2$inner$x\no$leaf_m()$",
            snapshot
        ),
        character()
    )
})

test_that("Enclosing-function local private bindings survive same-level joins as before", {
    for (portable in c("TRUE", "FALSE")) {
        code <- sprintf('
make <- function(cond) {
    private <- list(second = function() list(leak_second = 1))
    A <- R6::R6Class("A", portable = %s, public = list(
        x = NULL,
        initialize = function(x) self$x <- x,
        run = function() private$first()
    ), private = list(
        first = function() private$second(),
        second = function() list(own_second = 1)
    ))
    o <- if (cond) A$new(1) else A$new("a")
    o$run()$@@
}
', portable)
        expect_identical(r6_review_cursor_labels(code), "leak_second")
    }
    direct_leak <- '
make <- function(cond) {
    private <- list(leak = 1)
    A <- R6::R6Class("A", public = list(
        x = NULL,
        initialize = function(x) self$x <- x,
        run = function() private$h()
    ), private = list(h = function() private))
    o <- if (cond) A$new(1) else A$new("a")
    o$run()$@@
}
'
    expect_identical(r6_review_cursor_labels(direct_leak), "leak")
    no_leak <- '
make <- function(cond) {
    A <- R6::R6Class("A", public = list(
        x = NULL,
        initialize = function(x) self$x <- x,
        run = function() private$h()
    ), private = list(h = function() private))
    o <- if (cond) A$new(1) else A$new("a")
    o$run()$@@
}
'
    expect_identical(r6_review_cursor_labels(no_leak), character())
})

test_that("member_prepare_package preserves referenced R6 generator definitions and class dispatch while pruning dead tables", {
    root <- withr::local_tempdir()
    cache_dir <- file.path(root, "cache")
    dir.create(cache_dir, recursive = TRUE)
    withr::local_envvar(
        R_USER_CACHE_DIR = cache_dir,
        R_USER_DATA_DIR = cache_dir,
        R_USER_CONFIG_DIR = cache_dir,
        XDG_CACHE_HOME = cache_dir,
        R_LANGSVR_PACKAGE_CACHE_DIR = cache_dir
    )
    withr::local_options(languageserver.package_cache_dir = cache_dir)
    lib <- file.path(root, "lib")
    source_dir <- file.path(root, "r6prunefixture")
    dir.create(lib)
    dir.create(file.path(source_dir, "R"), recursive = TRUE)
    writeLines(c(
        "Package: r6prunefixture",
        "Version: 0.0.1",
        "Title: R6 Generator Pruning Fixture",
        "Description: Test R6 generator reachability in member_prepare_package.",
        "License: MIT",
        "Author: Test",
        "Maintainer: Test <test@example.com>",
        "Imports: R6"
    ), file.path(source_dir, "DESCRIPTION"))
    writeLines(c(
        "export(K1, inst_k1, lst_k1, GenDiff, inst_diff, holder_env, holder_list, act_env)",
        "export(make_box, raw_box, make_box_list, raw_box_list, make_wrap, make_del_wrap, make_native_ctor)",
        'export(GenStatic, GenUnref, GenPlain, GenCustomNew, helper_ref, ZUnlinked, AUnlinked, Gen1, "Gen1$weird")',
        'S3method("$", my_wrap)',
        'S3method("$", del_wrap)'
    ), file.path(source_dir, "NAMESPACE"))
    writeLines('
K1 <- R6::R6Class("K1", public = list(
    u3 = function(x = 1L) list(k1_u3 = x),
    f = 42L
))
inst_k1 <- K1$new()
lst_k1 <- list(inst = K1$new())

GenDiff <- R6::R6Class("DiffClass", cloneable = FALSE, public = list(f = 7L))
inst_diff <- GenDiff$new()

ZUnlinked <- R6::R6Class("ZUnlinked", cloneable = FALSE, public = list(z = 1L))
AUnlinked <- ZUnlinked

Gen1 <- R6::R6Class("Gen1", cloneable = FALSE, public = list(a = 1L))
`Gen1$weird` <- function(p = 4L) list(weird_ok = p)

GenLinked <- R6::R6Class("GenLinked", lock_class = FALSE, cloneable = FALSE, public = list(x = 1L))
GenLinked$linked_helper <- function(a = 1L) list(from_linked_env = a)
holder_env <- new.env(parent = emptyenv())
holder_env$g <- GenLinked

GenLinkedList <- R6::R6Class("GenLinkedList", lock_class = FALSE, cloneable = FALSE, public = list(x = 1L))
GenLinkedList$list_helper <- function(b = 1L) list(from_linked_list = b)
holder_list <- list(g = GenLinkedList)

GenB <- R6::R6Class("GenB", lock_class = FALSE, cloneable = FALSE, public = list(b = 1L))
GenB$helper_b <- function(w = 5L) list(trans_ok = w)
GenA <- R6::R6Class("GenA", lock_class = FALSE, cloneable = FALSE, public = list(a = 1L))
GenA$sub_gen <- GenB

GenFn <- R6::R6Class("GenFn", lock_class = FALSE, cloneable = FALSE, public = list(x = 1L))
GenFn$direct_fn <- function(q = 7L) list(direct_fn_ok = q)

GenOnLoad <- R6::R6Class("GenOnLoad", lock_class = FALSE, cloneable = FALSE, public = list(x = 1L))
GenOnLoad$helper <- function(z = 1L) list(from_onload_helper = z)
act_env <- new.env(parent = emptyenv())
.onLoad <- function(libname, pkgname) {
    makeActiveBinding("g", function() GenOnLoad, act_env)
    makeActiveBinding("ga", function() GenA, act_env)
    makeActiveBinding("fn", function() GenFn$direct_fn, act_env)
}

GenCtor <- R6::R6Class("GenCtor", lock_class = FALSE, cloneable = FALSE, public = list(x = 1L))
GenCtor$helper <- function(z = 1L) list(from_ctor_helper = z)
make_box <- function() {
    e <- new.env(parent = emptyenv())
    e$g <- GenCtor
    class(e) <- "my_box"
    e
}
raw_box <- function() structure(new.env(parent = emptyenv()), class = "my_box")

GenList <- R6::R6Class("GenList", lock_class = FALSE, cloneable = FALSE, public = list(x = 1L))
GenList$list_fn <- function(m = 3L) list(from_nested_list = m)
make_box_list <- function() {
    e <- new.env(parent = emptyenv())
    e$box_g <- list(h = GenList$list_fn)
    class(e) <- "box_list"
    e
}
raw_box_list <- function() structure(new.env(parent = emptyenv()), class = "box_list")

GenReg <- R6::R6Class("GenReg", lock_class = FALSE, cloneable = FALSE, public = list(x = 1L))
GenReg$helper <- function(z = 1L) list(from_s3_helper = z)
make_wrap <- function() structure(list(), class = "my_wrap")
`$.my_wrap` <- function(x, i) {
    if (i %in% c("new", "set", "helper")) GenReg[[i]] else NULL
}

GenDel <- R6::R6Class("GenDel", lock_class = FALSE, cloneable = FALSE, public = list(x = 1L))
GenDel$del_fn <- function(u = 10L, v = 20L) list(del_res = u)
wrap_factory <- function(self, orig) {
    out <- function() orig()
    formals(out) <- formals(orig)
    out
}
make_del_wrap <- function() structure(list(), class = "del_wrap")
`$.del_wrap` <- function(x, i) {
    if (i %in% c("del_fn")) {
        fn <- GenDel[[i]]
        wrap_factory(x, fn)
    } else {
        NULL
    }
}

GenU <- R6::R6Class("GenU", cloneable = FALSE, public = list(u = function() list(ok = 1L)))
GenU$mk_cb <- function(x) function(z = 99L) list(cb_out = x)
make_native_ctor <- function() {
    self <- new.env(parent = emptyenv())
    self$fn <- GenU$mk_cb(self)
    class(self) <- "nat_ctor"
    self
}

GenStatic <- R6::R6Class("GenStatic", lock_class = FALSE, cloneable = FALSE, public = list(x = 1L))
GenStatic$create <- function(alpha = 1L) list(from_static = alpha)

GenPlain <- R6::R6Class("GenPlain", lock_class = FALSE, public = list(
    initialize = function(v = 10L) NULL,
    x = 1L
))

GenCustomNew <- R6::R6Class("GenCustomNew", lock_class = FALSE, public = list(x = 1L))
GenCustomNew$new <- local({
    f <- function(special_arg = 123L) list(from_custom_new = special_arg)
    environment(f) <- GenCustomNew
    f
})

helper_ref <- function() 1L

GenUnref <- R6::R6Class("GenUnref", public = list(u = function() list(from_unref = 1L)))
', file.path(source_dir, "R", "fixture.R"))
    rcmd_env <- c(
        callr::rcmd_safe_env(),
        R_USER_CACHE_DIR = cache_dir,
        R_USER_DATA_DIR = cache_dir,
        R_USER_CONFIG_DIR = cache_dir,
        XDG_CACHE_HOME = cache_dir,
        R_LANGSVR_PACKAGE_CACHE_DIR = cache_dir
    )
    install_res <- callr::rcmd(
        "INSTALL",
        cmdargs = c("--no-docs", "--no-multiarch", "--no-test-load", paste0("--library=", lib), source_dir),
        env = rcmd_env, stdout = "|", stderr = "|", system_profile = FALSE, user_profile = FALSE
    )
    expect_identical(install_res$status, 0L)
    withr::local_libpaths(lib, action = "prefix")
    on.exit(tryCatch(unloadNamespace("r6prunefixture"), error = function(e) NULL), add = TRUE)
    snapshot <- member_prepare_package("r6prunefixture", lib_paths = .libPaths())
    metadata <- collections::dict()
    metadata$set("r6prunefixture", member_index_thaw(snapshot))
    ws <- list(
        member_metadata = metadata,
        get_documentation = function(...) NULL,
        get_signature = function(...) NULL,
        get_help = function(...) NULL
    )
    ask_items <- function(code, custom_ws = ws) {
        lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]
        uri <- "file:///r6-prune.R"
        doc <- Document$new(uri, language = "r", version = 1L, content = lines)
        data <- parse_document(uri, lines)
        data$version <- 1L
        doc$update_parse_data(data)
        point <- list(row = length(lines) - 1L, col = nchar(tail(lines, 1L)))
        member_completion(uri, custom_ws, doc, point, TRUE, 200L)
    }
    ask_labels <- function(code, custom_ws = ws) {
        items <- ask_items(code, custom_ws)
        sort(vapply(items, `[[`, character(1L), "label"))
    }
    ask_details <- function(code, custom_ws = ws) {
        items <- ask_items(code, custom_ws)
        labels <- vapply(items, `[[`, character(1L), "label")
        ord <- order(labels)
        stats::setNames(vapply(items[ord], `[[`, character(1L), "detail"), labels[ord])
    }
    ask_kinds_details <- function(code, custom_ws = ws) {
        items <- ask_items(code, custom_ws)
        labels <- vapply(items, `[[`, character(1L), "label")
        ord <- order(labels)
        stats::setNames(
            vapply(items[ord], function(it) paste0("kind=", it$kind, ":", it$detail), character(1L)),
            labels[ord]
        )
    }
    ask_hover <- function(code, col, custom_ws = ws) {
        lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]
        uri <- "file:///r6-prune.R"
        doc <- Document$new(uri, language = "r", version = 1L, content = lines)
        data <- parse_document(uri, lines)
        data$version <- 1L
        doc$update_parse_data(data)
        hover_reply(1L, uri, custom_ws, doc, list(row = length(lines) - 1L, col = col))$result$contents
    }
    ask_sig <- function(code, custom_ws = ws) {
        lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]
        uri <- "file:///r6-prune.R"
        doc <- Document$new(uri, language = "r", version = 1L, content = lines)
        data <- parse_document(uri, lines)
        data$version <- 1L
        doc$update_parse_data(data)
        point <- list(row = length(lines) - 1L, col = nchar(tail(lines, 1L)))
        sigs <- signature_reply(1L, uri, custom_ws, doc, point)$result$signatures
        if (length(sigs)) sigs[[1L]]$label else NULL
    }
    # Stored R6 instance (top-level or nested) joined with Class$new(), hover, and classname != binding_name.
    expect_identical(
        ask_labels("o <- if (cnd) r6prunefixture::inst_k1 else r6prunefixture::K1$new()\no$"),
        c("clone", "f", "u3")
    )
    expect_identical(
        ask_labels("o <- if (cnd) r6prunefixture::lst_k1$inst else r6prunefixture::K1$new()\no$"),
        c("clone", "f", "u3")
    )
    expect_identical(
        ask_hover("o <- if (cnd) r6prunefixture::inst_k1 else r6prunefixture::K1$new()\no$f", 2L),
        c("```r\n42L\n```", "Public field of R6 class `K1` (package `r6prunefixture`)")
    )
    expect_identical(
        ask_labels("o <- if (cnd) r6prunefixture::inst_diff else r6prunefixture::GenDiff$new()\no$"),
        character()
    )
    if (nzchar(system.file(package = "desc"))) {
        desc_snap <- member_prepare_package("desc", lib_paths = .libPaths())
        for (order_first in c("desc", "r6prunefixture")) {
            multi_md <- collections::dict()
            if (identical(order_first, "desc")) {
                multi_md$set("desc", member_index_thaw(desc_snap))
                multi_md$set("r6prunefixture", member_index_thaw(snapshot))
            } else {
                multi_md$set("r6prunefixture", member_index_thaw(snapshot))
                multi_md$set("desc", member_index_thaw(desc_snap))
            }
            multi_ws <- list(
                member_metadata = multi_md,
                get_documentation = function(...) NULL,
                get_signature = function(...) NULL,
                get_help = function(...) NULL
            )
            expect_identical(
                ask_labels(paste0(
                    "d <- desc::desc\n",
                    "o <- if (cnd) r6prunefixture::inst_k1 else r6prunefixture::K1$new()\n",
                    "o$"
                ), multi_ws),
                c("clone", "f", "u3")
            )
        }
        desc_md <- collections::dict()
        desc_md$set("desc", member_index_thaw(desc_snap))
        desc_ws <- list(
            member_metadata = desc_md,
            get_documentation = function(...) NULL,
            get_signature = function(...) NULL,
            get_help = function(...) NULL
        )
        # On real installed `desc`: attached `<Gen>$set` / `$debug` / `$lock` / `$clone_method`.
        expect_identical(
            ask_kinds_details(paste0(
                "library(desc)\n",
                "box <- list(s = description$set, d = description$debug, ",
                "lk = description$lock, cl = description$clone_method)\n",
                "box$"
            ), desc_ws),
            c(
                cl = "kind=2:cl(deep = FALSE)",
                d = "kind=2:d(name)",
                lk = "kind=2:lk()",
                s = "kind=2:s(which = NULL, name = NULL, value, overwrite = FALSE)"
            )
        )
        expect_identical(
            ask_sig("library(desc)\nbox <- list(s = description$set)\nbox$s(", desc_ws),
            "s(which = NULL, name = NULL, value, overwrite = FALSE)"
        )
        expect_identical(
            ask_hover("library(desc)\nbox <- list(s = description$set)\nbox$s", 4L, desc_ws),
            "```r\ns(which = NULL, name = NULL, value, overwrite = FALSE)\n```"
        )
        desc_r6 <- paste0(
            "library(desc)\n",
            'MyCls <- R6::R6Class("MyCls", cloneable = FALSE, public = list(setter = description$set))\n'
        )
        expect_identical(
            ask_kinds_details(paste0(desc_r6, "MyCls$new()$"), desc_ws),
            c(setter = "kind=2:setter(which = NULL, name = NULL, value, overwrite = FALSE)")
        )
        expect_identical(
            ask_sig(paste0(desc_r6, "MyCls$new()$setter("), desc_ws),
            "setter(which = NULL, name = NULL, value, overwrite = FALSE)"
        )
        expect_identical(
            ask_hover(paste0(desc_r6, "MyCls$new()$setter"), 14L, desc_ws),
            c(
                "```r\nsetter(which = NULL, name = NULL, value, overwrite = FALSE)\n```",
                "Public field of R6 class `MyCls`"
            )
        )
        expect_identical(
            ask_labels(
                'library(desc)\nx <- if (cond) description$set("public", "foo") else list(desc_ok = 1L)\nx$',
                desc_ws
            ),
            "desc_ok"
        )
    }
    # Backtick top-level function whose name starts with `<Gen>$`.
    expect_identical(ask_labels("r6prunefixture::`Gen1$weird`()$"), "weird_ok")
    # Linked generator in exported environment, list, and 2-hop chain (`input$links`).
    expect_identical(ask_labels("r6prunefixture::holder_env$g$linked_helper()$"), "from_linked_env")
    expect_identical(ask_details("r6prunefixture::holder_env$g$")[["linked_helper"]], "linked_helper(a = 1L)")
    expect_identical(ask_labels("r6prunefixture::holder_list$g$list_helper()$"), "from_linked_list")
    # Runtime active bindings installed in .onLoad (direct generator, 2-hop, and direct function_key).
    expect_identical(ask_labels("r6prunefixture::act_env$g$helper()$"), "from_onload_helper")
    expect_identical(ask_details("r6prunefixture::act_env$g$")[["helper"]], "helper(z = 1L)")
    expect_identical(ask_sig("r6prunefixture::act_env$g$helper("), "helper(z = 1L)")
    expect_identical(ask_hover("r6prunefixture::act_env$g$helper(1)", 28L), "```r\nhelper(z = 1L)\n```")
    expect_identical(ask_labels("r6prunefixture::act_env$ga$sub_gen$helper_b()$"), "trans_ok")
    expect_identical(ask_labels("r6prunefixture::act_env$fn()$"), "direct_fn_ok")
    expect_identical(ask_sig("r6prunefixture::act_env$fn("), "fn(q = 7L)")
    # Declarative constructor assigning an R6 generator or nested list of generator closures.
    expect_identical(ask_labels("r6prunefixture::raw_box()$g$helper()$"), "from_ctor_helper")
    expect_identical(ask_details("r6prunefixture::raw_box()$g$")[["helper"]], "helper(z = 1L)")
    expect_identical(ask_labels("r6prunefixture::raw_box_list()$box_g$h()$"), "from_nested_list")
    # Cross-class S3 `$` delegating into an R6 generator environment and wrapped `delegation`.
    expect_identical(ask_labels("r6prunefixture::make_wrap()$helper()$"), "from_s3_helper")
    expect_identical(
        ask_details("r6prunefixture::make_wrap()$"),
        c(
            helper = "helper(z = 1L)",
            new = "new(...)",
            set = "set(which = NULL, name = NULL, value, overwrite = FALSE)"
        )
    )
    expect_identical(
        ask_sig("r6prunefixture::make_wrap()$set("),
        "set(which = NULL, name = NULL, value, overwrite = FALSE)"
    )
    expect_identical(
        ask_hover("r6prunefixture::make_wrap()$set", 29L),
        "```r\nset(which = NULL, name = NULL, value, overwrite = FALSE)\n```"
    )
    expect_identical(ask_labels("r6prunefixture::make_del_wrap()$del_fn()$"), "del_res")
    expect_identical(ask_sig("r6prunefixture::make_del_wrap()$del_fn("), "del_fn(u = 10L, v = 20L)")
    # Constructor factory installing a bound closure via `GenU$mk_cb(self)` (`native_factories`).
    expect_identical(ask_details("r6prunefixture::make_native_ctor()$")[["fn"]], "fn(z = 99L)")
    expect_identical(ask_sig("r6prunefixture::make_native_ctor()$fn("), "fn(z = 99L)")
    expect_identical(ask_labels("r6prunefixture::make_native_ctor()$fn()$"), "cb_out")
    # Custom static closure attached directly to an exported R6 generator.
    expect_identical(ask_labels("library(r6prunefixture)\nGenStatic$create()$"), "from_static")
    expect_identical(ask_labels("r6prunefixture::GenUnref$new()$u()$"), "from_unref")
    # On `r6prunefixture`: standard `<Gen>$*` methods after `library(pkg)`.
    expect_identical(
        ask_kinds_details(paste0(
            "library(r6prunefixture)\n",
            "box <- list(s = GenUnref$set, d = GenUnref$debug, ",
            "lk = GenUnref$lock, cl = GenUnref$clone_method)\n",
            "box$"
        )),
        c(
            cl = "kind=2:cl(deep = FALSE)",
            d = "kind=2:d(name)",
            lk = "kind=2:lk()",
            s = "kind=2:s(which = NULL, name = NULL, value, overwrite = FALSE)"
        )
    )
    expect_identical(
        ask_sig("library(r6prunefixture)\nbox <- list(s = GenUnref$set)\nbox$s("),
        "s(which = NULL, name = NULL, value, overwrite = FALSE)"
    )
    expect_identical(
        ask_hover("library(r6prunefixture)\nbox <- list(s = GenUnref$set)\nbox$s", 4L),
        "```r\ns(which = NULL, name = NULL, value, overwrite = FALSE)\n```"
    )
    expect_identical(
        ask_kinds_details(paste0(
            "library(r6prunefixture)\n",
            'MyCls <- R6::R6Class("MyCls", cloneable = FALSE, public = list(setter = GenUnref$set))\n',
            "MyCls$new()$"
        )),
        c(setter = "kind=2:setter(which = NULL, name = NULL, value, overwrite = FALSE)")
    )
    # Unattached `<Gen>$new` when package index is selected by another symbol.
    expect_identical(
        ask_sig("h <- r6prunefixture::helper_ref\nbox <- list(ctor = GenPlain$new)\nbox$ctor("),
        "ctor(...)"
    )
    expect_identical(
        ask_hover("h <- r6prunefixture::helper_ref\nbox <- list(ctor = GenPlain$new)\nbox$ctor", 5L),
        "```r\nctor(...)\n```"
    )
    # Custom `<Gen>$new` closure whose enclosing environment is `<Gen>`.
    expect_identical(
        ask_labels("h <- r6prunefixture::helper_ref\nbox <- list(ctor = GenCustomNew$new)\nbox$ctor()$"),
        "from_custom_new"
    )
    expect_identical(
        ask_sig("h <- r6prunefixture::helper_ref\nbox <- list(ctor = GenCustomNew$new)\nbox$ctor("),
        "ctor(special_arg = 123L)"
    )
    # `<Gen>$set()` `.never` dead-branch inference on `processx` and `r6prunefixture` (+ valid 3-arg control).
    expect_identical(
        ask_labels(paste0(
            "library(r6prunefixture)\n",
            "f <- function(bad) {\n",
            "    if (bad) return(GenUnref$set())\n",
            "    list(survived = 1L)\n",
            "}\n",
            "f(cond)$"
        )),
        "survived"
    )
    if (nzchar(system.file(package = "processx"))) {
        proc_snap <- member_prepare_package("processx", lib_paths = .libPaths())
        proc_md <- collections::dict()
        proc_md$set("processx", member_index_thaw(proc_snap))
        proc_ws <- list(
            member_metadata = proc_md,
            get_documentation = function(...) NULL,
            get_signature = function(...) NULL,
            get_help = function(...) NULL
        )
        expect_identical(
            ask_labels("library(processx)\nx <- if (cond) process$set() else list(ok_branch = 1L)\nx$", proc_ws),
            "ok_branch"
        )
        expect_identical(
            ask_labels(
                'library(processx)\nx <- if (cond) process$set("public", "foo") else list(ok_miss = 1L)\nx$',
                proc_ws
            ),
            "ok_miss"
        )
        expect_identical(
            ask_labels(
                'library(processx)\nx <- if (cond) process$set("public", 123L, 1L) else list(ok_bad_nm = 1L)\nx$',
                proc_ws
            ),
            "ok_bad_nm"
        )
        expect_identical(
            ask_labels(
                'library(processx)\nx <- if (cond) process$set("public", "foo", 1L) else list(ok_branch = 1L)\nx$',
                proc_ws
            ),
            character()
        )
    }
    # Shared helper parity via r6_review_package_snapshot() as well.
    inmem_snap <- r6_review_package_snapshot(list(
        K1 = R6::R6Class("K1", public = list(u3 = function() list(ok = 1L), f = 42L)),
        inst_k1 = R6::R6Class("K1", public = list(u3 = function() list(ok = 1L), f = 42L))$new()
    ))
    expect_identical(
        r6_review_cursor_labels("o <- if (cnd) r6reviewfixture::inst_k1 else r6reviewfixture::K1$new()\no$", inmem_snap),
        c("clone", "f", "u3")
    )
    # Unreferenced generator tables and dead alias pairs are pruned while `<Gen>$*` keys remain resolvable.
    expect_null(inmem_snap$properties[["K1"]])
    expect_identical(inmem_snap$members[["K1"]], character())
    expect_null(snapshot$properties[["GenUnref"]])
    expect_null(snapshot$registries[["GenUnref"]])
    expect_identical(snapshot$members[["GenUnref"]], character())
    retained_defs <- c("GenUnref$set", "GenUnref$debug", "GenUnref$clone_method", "GenPlain$new", "GenCustomNew$new")
    expect_identical(intersect(retained_defs, names(snapshot$definitions)), retained_defs)
    expect_true(is.symbol(snapshot$definitions[["GenUnref$set"]]))
    expect_true(is.call(snapshot$definitions[["GenCustomNew$new"]]))
    if (exists("member_r6_standard_generator_fn", mode = "function")) {
        ns_fix <- asNamespace("r6prunefixture")
        expect_false(member_r6_standard_generator_fn(
            ns_fix$GenCustomNew$new, "new", ns_fix$GenCustomNew
        ))
        expect_false(member_r6_standard_generator_fn(
            ns_fix$GenPlain$new, "new", ns_fix$GenCustomNew
        ))
        expect_true(member_r6_standard_generator_fn(
            ns_fix$GenPlain$new, "new", ns_fix$GenPlain
        ))
        ret_input <- member_namespace_input(
            ns_fix, package = "r6prunefixture", exports = getNamespaceExports(ns_fix)
        )
        ret_idx <- member_package_index(ret_input)
        ret_idx$return_definitions <- "stale"
        member_prepare_r6_roots(ret_idx, ret_input, ns_fix)
        expect_identical(
            ret_idx$return_definitions,
            digest::digest(ret_idx$definitions, algo = "xxhash64")
        )
    }
    expect_null(snapshot$properties[["ZUnlinked"]])
    expect_null(snapshot$properties[["AUnlinked"]])
    expect_null(snapshot$properties[["Gen1"]])
    expect_null(snapshot$properties[["GenReg"]])
    expect_null(snapshot$properties[["GenDel"]])
    expect_null(snapshot$properties[["GenU"]])
    expect_null(snapshot$properties[["GenStatic"]])
})

test_that("Internal R6 shape fields do not leak into receiver, context, or bound-method joins", {
    # Depth-shifted hierarchies with identical super/private surfaces.
    expect_identical(
        r6_review_cursor_labels(paste0(
            'G0 <- R6::R6Class("G0", cloneable = FALSE)\n',
            'G1 <- R6::R6Class("G", cloneable = FALSE)\n',
            'G2 <- R6::R6Class("G", inherit = G0, cloneable = FALSE)\n',
            'mk <- function(P) R6::R6Class("Leaf", inherit = P, cloneable = FALSE, ',
            "public = list(m = function() list(ok = 1)))\n",
            "o <- if (cond) mk(G1)$new() else mk(G2)$new()\n",
            "o$m()$"
        )),
        "ok"
    )
    # Public data fields (`elements`, `literal`) and active bindings carry no level tags.
    expect_identical(
        r6_review_cursor_labels(paste0(
            'G0 <- R6::R6Class("G0")\n',
            'P1 <- R6::R6Class("P", public = list(idx = 1L, items = list(list(hit = 1L))))\n',
            'P2 <- R6::R6Class("P", inherit = G0, public = list(idx = 1L, items = list(list(hit = 1L))))\n',
            "o <- if (cond) P1$new() else P2$new()\n",
            "o$items[[o$idx]]$"
        )),
        "hit"
    )
    act_lines <- c(
        'G0 <- R6::R6Class("G0")',
        'P1 <- R6::R6Class("Parent", active = list(act = function() 1L))',
        'P2 <- R6::R6Class("Parent", inherit = G0, active = list(act = function() 1L))',
        'S1 <- R6::R6Class("S1", inherit = P1)',
        'S2 <- R6::R6Class("S2", inherit = P2)',
        "o <- if (cond) S1$new() else S2$new()",
        "o$act"
    )
    uri <- "file:///r6-act-hover.R"
    doc <- Document$new(uri, language = "r", version = 1L, content = act_lines)
    data <- parse_document(uri, act_lines)
    data$version <- 1L
    doc$update_parse_data(data)
    ws <- list(
        member_metadata = collections::dict(),
        get_documentation = function(...) NULL,
        get_signature = function(...) NULL,
        get_help = function(...) NULL
    )
    act_hover <- hover_reply(1L, uri, ws, doc, list(row = length(act_lines) - 1L, col = 3L))$result
    expect_identical(act_hover$contents, "Active binding of R6 class `Parent`")
    # Cursor `private` and `self$get_priv()` join cleanly without dropping `r6_self`.
    expect_identical(
        r6_review_cursor_labels(paste0(
            'A <- R6::R6Class("A", public = list(\n',
            "    pub_field = list(from_pub = 1),\n",
            "    get_priv = function() private,\n",
            "    probe = function(cond) {\n",
            "        p <- if (cond) private else self$get_priv()\n",
            "        p$h()$@@\n",
            "    }\n",
            "), private = list(h = function() self$pub_field))"
        )),
        "from_pub"
    )
    # List-boxed bound methods across same-surface inheritance and single-vs-joined receivers.
    expect_identical(
        r6_review_cursor_labels(paste0(
            'A <- R6::R6Class("A", cloneable = FALSE, public = list(m = function() list(ok = 1)))\n',
            'B <- R6::R6Class("A", inherit = A, cloneable = FALSE)\n',
            "l <- list(a = A$new()$m, b = B$new()$m)\n",
            "fn <- if (cond) l$a else l$b\n",
            "fn()$"
        )),
        "ok"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            'A <- R6::R6Class("A", cloneable = FALSE, public = list(m = function() private$pm()), ',
            "private = list(pm = function() list(ok_pm = 1)))\n",
            'B <- R6::R6Class("A", inherit = A, cloneable = FALSE)\n',
            "l <- list(a = A$new()$m, b = B$new()$m)\n",
            "fn <- if (cond) l$a else l$b\n",
            "fn()$"
        )),
        "ok_pm"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            'P1 <- R6::R6Class("P1", cloneable = FALSE, public = list(p = function() 1))\n',
            'P2 <- R6::R6Class("P2", cloneable = FALSE, public = list(p = function() 2))\n',
            'mk <- function(P) R6::R6Class("Leaf", inherit = P, cloneable = FALSE, ',
            "public = list(m = function() list(ok = 1)))\n",
            "l <- list(a = mk(P1)$new()$m, b = mk(P2)$new()$m)\n",
            "fn <- if (cond) l$a else l$b\n",
            "fn()$"
        )),
        character()
    )
    single_vs_join <- paste0(
        'A <- R6::R6Class("A", cloneable = FALSE, public = list(\n',
        "    x = NULL,\n",
        "    initialize = function(x = 1) self$x <- x,\n",
        "    m = function() list(ok = 1),\n",
        "    mp = function() private$p\n",
        "), private = list(p = list(ok_p = 1)))\n",
        "o1 <- A$new(1)\n",
        'o2 <- if (cnd2) A$new(1) else A$new("a")\n',
        "l <- list(a = o1$m, b = o2$m, ap = o1$mp, bp = o2$mp)\n"
    )
    expect_identical(r6_review_cursor_labels(paste0(single_vs_join, "fn <- if (cnd1) l$a else l$b\nfn()$")), "ok")
    expect_identical(r6_review_cursor_labels(paste0(single_vs_join, "fn <- if (cnd1) l$ap else l$bp\nfn()$")), "ok_p")
    # Stored-instance bound contexts trim unreachable `r6_contexts` and `r6_visited`,
    # while keeping reachable visited trails distinct (`PWithPm` soundness control).
    P1Sup <- R6::R6Class("P1Sup", cloneable = FALSE, public = list(self_m = function() self$pub_f))
    KSup <- R6::R6Class("KSup", inherit = P1Sup, cloneable = FALSE, public = list(
        pub_f = list(ok_pub = 1L),
        get_sup = function() super,
        via_priv_sup = function() private$h()
    ), private = list(h = function() super))
    PWithPm <- R6::R6Class("PWithPm", cloneable = FALSE, public = list(
        pm = function() private$x(),
        self_m = function() self$pub_f
    ), private = list(x = function() list(ok_x = 1L)))
    KWithPm <- R6::R6Class("KWithPm", inherit = PWithPm, cloneable = FALSE, public = list(
        pub_f = list(ok_pub = 1L),
        get_sup = function() super,
        via_priv_sup = function() private$h()
    ), private = list(h = function() super))
    K1a <- R6::R6Class("X", public = list(
        n = function() list(k1a_n = TRUE),
        m = function() private$p
    ), private = list(p = list(k1_p = TRUE)))
    K1b <- R6::R6Class("X", public = list(
        n = function() list(k1b_n = TRUE),
        m = function() private$p
    ), private = list(p = list(k1_p = TRUE)))
    SubA <- R6::R6Class("X", inherit = K1a, public = list(q = function() super$n()))
    SubB <- R6::R6Class("X", inherit = K1b, public = list(q = function() super$n()))
    stored_snap <- r6_review_package_snapshot(list(
        obj_sup = KSup$new(),
        obj_sup_with_pm = KWithPm$new(),
        lst_sub = list(a = SubA$new(), b = SubB$new())
    ))
    expect_identical(
        r6_review_cursor_labels(paste0(
            "s <- if (cond) r6reviewfixture::obj_sup$get_sup() else r6reviewfixture::obj_sup$via_priv_sup()\n",
            "s$self_m()$"
        ), stored_snap),
        "ok_pub"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            "s <- if (cond) r6reviewfixture::obj_sup_with_pm$get_sup() else ",
            "r6reviewfixture::obj_sup_with_pm$via_priv_sup()\n",
            "s$self_m()$"
        ), stored_snap),
        character()
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            "box <- list(fa = r6reviewfixture::lst_sub$a$m, fb = r6reviewfixture::lst_sub$b$m)\n",
            "fn <- if (cond) box$fa else box$fb\n",
            "fn()$"
        ), stored_snap),
        "k1_p"
    )
})

test_that("Equivalent R6 instances, private scopes, bound methods, generators, and Reduce branches join cleanly", {
    doc_sig <- function(code) {
        lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]
        uri <- "file:///r6-equiv-join.R"
        doc <- Document$new(uri, language = "r", version = 1L, content = lines)
        data <- parse_document(uri, lines)
        data$version <- 1L
        doc$update_parse_data(data)
        ws <- list(
            member_metadata = collections::dict(),
            get_documentation = function(...) NULL,
            get_signature = function(...) NULL,
            get_help = function(...) NULL
        )
        point <- list(row = length(lines) - 1L, col = nchar(tail(lines, 1L)))
        sigs <- signature_reply(1L, uri, ws, doc, point)$result$signatures
        if (length(sigs)) sigs[[1L]]$label else NULL
    }
    # B1: Overridden ancestor private data literal (`Parent1` vs `Parent2`) + non-overridden reverse control.
    b1_code <- paste0(
        'Parent1 <- R6::R6Class("Par", cloneable = FALSE, private = list(dead = 1L))\n',
        'Parent2 <- R6::R6Class("Par", cloneable = FALSE, private = list(dead = 2L))\n',
        'mk <- function(Par) R6::R6Class("Leaf", inherit = Par, cloneable = FALSE, ',
        "public = list(pub_f = list(ok_pub = 1L), get_priv = function() private, mp = function() private$pm()), ",
        "private = list(dead = 99L, pm = function() private$sec, self_pm = function() self$pub_f, ",
        "sec = list(ok_sec = 1L)))\n"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(b1_code, "o <- if (cond) mk(Parent1)$new() else mk(Parent2)$new()\no$mp()$")),
        "ok_sec"
    )
    expect_identical(
        r6_review_cursor_labels(
            paste0(b1_code, "o <- if (cond) mk(Parent1)$new() else mk(Parent2)$new()\no$get_priv()$pm()$")
        ),
        "ok_sec"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            'Parent1 <- R6::R6Class("Par", cloneable = FALSE, private = list(dead = 1L))\n',
            'Parent2 <- R6::R6Class("Par", cloneable = FALSE, private = list(dead = 2L))\n',
            'mk <- function(Par) R6::R6Class("Leaf", inherit = Par, cloneable = FALSE, ',
            "public = list(mp = function() private$pm()), ",
            "private = list(pm = function() private$sec, sec = list(ok_sec = 1L)))\n",
            "o <- if (cond) mk(Parent1)$new() else mk(Parent2)$new()\n",
            "o$mp()$"
        )),
        character()
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            'G0 <- R6::R6Class("G0", cloneable = FALSE)\n',
            'G1 <- R6::R6Class("G", cloneable = FALSE)\n',
            'G2 <- R6::R6Class("G", inherit = G0, cloneable = FALSE)\n',
            'mk <- function(Par) R6::R6Class("Leaf", inherit = Par, cloneable = FALSE, public = list(',
            "v = NULL, initialize = function(v) self$v <- v, mp = function() private$pm()), ",
            "private = list(pm = function() private$sec, sec = list(ok_sec = 1L)))\n",
            'o <- if (cond) mk(G1)$new(1L) else mk(G2)$new("a")\n',
            "o$mp()$"
        )),
        character()
    )
    # Depth-shifted empty ancestors (`G1` vs `G2`) + masked ancestor method reverse control (`extra` in `super`).
    b2_code <- paste0(
        'G0 <- R6::R6Class("G0", cloneable = FALSE)\n',
        'G1 <- R6::R6Class("G", cloneable = FALSE)\n',
        'G2 <- R6::R6Class("G", inherit = G0, cloneable = FALSE)\n',
        'mk <- function(Par) R6::R6Class("Leaf", inherit = Par, cloneable = FALSE, ',
        "public = list(pub_f = list(ok_pub = 1L), get_priv = function() private, mp = function() private$pm()), ",
        "private = list(pm = function() private$sec, self_pm = function() self$pub_f, sec = list(ok_sec = 1L)))\n"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(b2_code, "o <- if (cond) mk(G1)$new() else mk(G2)$new()\no$mp()$")),
        "ok_sec"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(b2_code, "o <- if (cond) mk(G1)$new() else mk(G2)$new()\no$get_priv()$pm()$")),
        "ok_sec"
    )
    idx_pos <- member_generic_index("")
    idx_pos$r6_attached <- TRUE
    bnd_pos <- list()
    for (ex in str2expression(paste0(
        b2_code,
        "inst_g1 <- mk(G1)$new()\n",
        "inst_g2 <- mk(G2)$new()\n"
    ))) {
        bnd_pos[as.character(ex[[2L]])] <- list(member_infer(ex[[3L]], idx_pos, bnd_pos))
    }
    possible <- member_possible_receiver(
        list(possible_returns = list(bnd_pos$inst_g1, bnd_pos$inst_g2)),
        idx_pos, bnd_pos
    )
    expect_identical(possible$ambiguous, character())
    expect_identical(
        r6_review_cursor_labels(paste0(
            'G0 <- R6::R6Class("G0", cloneable = FALSE, public = list(extra = function() 1L))\n',
            'G1 <- R6::R6Class("G", cloneable = FALSE)\n',
            'G2 <- R6::R6Class("G", inherit = G0, cloneable = FALSE)\n',
            'mk <- function(Par) R6::R6Class("Leaf", inherit = Par, cloneable = FALSE, ',
            "public = list(extra = 0L, mp = function() private$pm()), ",
            "private = list(pm = function() private$sec, sec = list(ok_sec = 1L)))\n",
            "o <- if (cond) mk(G1)$new() else mk(G2)$new()\n",
            "o$mp()$"
        )),
        character()
    )
    idx_data <- member_generic_index("")
    idx_data$r6_attached <- TRUE
    bnd_data <- list()
    for (ex in str2expression(paste0(
        'G0 <- R6::R6Class("G0", cloneable = FALSE, public = list(extra = function() 1L))\n',
        'G1 <- R6::R6Class("G", cloneable = FALSE)\n',
        'G2 <- R6::R6Class("G", inherit = G0, cloneable = FALSE)\n',
        'mk_data <- function(Par) R6::R6Class("Mid", inherit = Par, cloneable = FALSE, public = list(extra = 0L))\n',
        "d_single <- mk_data(G1)$new()\n",
        "d_joined <- if (cond) mk_data(G1)$new() else mk_data(G2)$new()\n"
    ))) {
        bnd_data[as.character(ex[[2L]])] <- list(member_infer(ex[[3L]], idx_data, bnd_data))
    }
    expect_false(identical(member_shape_key(bnd_data$d_joined), member_shape_key(bnd_data$d_single)))
    # Leaf with masked public field and diverged instance `r6_super` drops `r6_private` on join.
    expect_identical(
        r6_review_cursor_labels(paste0(
            'G0 <- R6::R6Class("G0", cloneable = FALSE, public = list(get_priv = function() private))\n',
            'P1 <- R6::R6Class("P", inherit = G0, cloneable = FALSE, public = list(sup_only = function() 1L))\n',
            'P2 <- R6::R6Class("P", inherit = G0, cloneable = FALSE, public = list(sup_only = function() 2L))\n',
            'mk <- function(P) R6::R6Class("C", inherit = P, cloneable = FALSE, ',
            "public = list(sup_only = 42L), private = list(sec = list(ok = 1L)))\n",
            "o <- if (cond) mk(P1)$new() else mk(P2)$new()\n",
            "o$get_priv()$sec$"
        )),
        character()
    )
    # B5: Joined `private` scopes from equivalent instances retain `r6_self` (+ differing-`self` reverse controls).
    expect_identical(
        r6_review_cursor_labels(paste0(
            b1_code,
            "p <- if (cond) mk(Parent1)$new()$get_priv() else mk(Parent2)$new()$get_priv()\n",
            "p$self_pm()$"
        )),
        "ok_pub"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            b2_code,
            "p <- if (cond) mk(G1)$new()$get_priv() else mk(G2)$new()$get_priv()\n",
            "p$self_pm()$"
        )),
        "ok_pub"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            'A <- R6::R6Class("A", cloneable = FALSE, public = list(',
            "v = NULL, pub_f = list(ok_pub = 1L), initialize = function(v) self$v <- v, ",
            "get_priv = function() private), private = list(self_pm = function() self$pub_f))\n",
            'p <- if (cond) A$new(1L)$get_priv() else A$new("a")$get_priv()\n',
            "p$self_pm()$"
        )),
        character()
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            'Parent1 <- R6::R6Class("Par", cloneable = FALSE, public = list(pub_f = list(p1 = 1L)), ',
            "private = list(dead = 1L))\n",
            'Parent2 <- R6::R6Class("Par", cloneable = FALSE, public = list(pub_f = list(p2 = 1L)), ',
            "private = list(dead = 1L))\n",
            'mk <- function(Par) R6::R6Class("Leaf", inherit = Par, cloneable = FALSE, ',
            "public = list(get_priv = function() private), ",
            "private = list(dead = 99L, self_pm = function() self$pub_f))\n",
            "p <- if (cond) mk(Parent1)$new()$get_priv() else mk(Parent2)$new()$get_priv()\n",
            "p$self_pm()$"
        )),
        character()
    )
    # Direct, `[[idx]]`, and container-boxed bound R6 method joins (+ reverse soundness controls).
    g2_depth <- paste0(
        'G0 <- R6::R6Class("G0", cloneable = FALSE)\n',
        'G1 <- R6::R6Class("G", cloneable = FALSE)\n',
        'G2 <- R6::R6Class("G", inherit = G0, cloneable = FALSE)\n'
    )
    g2_ov <- paste0(
        'P1 <- R6::R6Class("P", cloneable = FALSE, private = list(secret = 1L))\n',
        'P2 <- R6::R6Class("P", cloneable = FALSE, private = list(secret = 2L))\n'
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            g2_depth,
            'mk <- function(P) R6::R6Class("Leaf", inherit = P, cloneable = FALSE, ',
            "private = list(sec = list(ok_sec = 1L)), public = list(m = function() private$sec))\n",
            "fn <- if (cond) mk(G1)$new()$m else mk(G2)$new()$m\n",
            "fn()$"
        )),
        "ok_sec"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            g2_ov,
            'mk <- function(P) R6::R6Class("C", inherit = P, cloneable = FALSE, ',
            "private = list(secret = 99L, out = list(ans_hit = 1L)), public = list(ans = function() private$out))\n",
            "fn <- if (cond) mk(P1)$new()$ans else mk(P2)$new()$ans\n",
            "fn()$"
        )),
        "ans_hit"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            g2_depth,
            'mk <- function(P) R6::R6Class("Leaf", inherit = P, cloneable = FALSE, ',
            "private = list(sec = list(ok_elem = 1L)), public = list(m = function() private$sec))\n",
            "xs <- list(mk(G1)$new()$m, mk(G2)$new()$m)\n",
            "xs[[idx]]()$"
        )),
        "ok_elem"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            g2_depth,
            'mk <- function(P) R6::R6Class("Leaf", inherit = P, cloneable = FALSE, ',
            "private = list(pm = function() list(ok_pm = 1L)), public = list(m = function() private$pm()))\n",
            "box <- list(a = mk(G1)$new()$m, b = mk(G2)$new()$m)\n",
            "fn <- if (cond) box$a else box$b\n",
            "fn()$"
        )),
        "ok_pm"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            g2_ov,
            'mk <- function(P) R6::R6Class("C", inherit = P, cloneable = FALSE, ',
            "private = list(secret = 99L, pm = function() list(ans_pm = 1L)), ",
            "public = list(ans = function() private$pm()))\n",
            "box <- list(a = mk(P1)$new()$ans, b = mk(P2)$new()$ans)\n",
            "fn <- if (cond) box$a else box$b\n",
            "fn()$"
        )),
        "ans_pm"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            g2_ov,
            'mk <- function(P) R6::R6Class("C", inherit = P, cloneable = FALSE, ',
            "private = list(out = list(ans_hit = 1L)), public = list(ans = function() private$out))\n",
            "fn <- if (cond) mk(P1)$new()$ans else mk(P2)$new()$ans\n",
            "fn()$"
        )),
        character()
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            'A <- R6::R6Class("A", cloneable = FALSE, public = list(',
            "v = NULL, initialize = function(v) self$v <- v, m = function() list(ok = 1L)))\n",
            'fn <- if (cond) A$new(1L)$m else A$new("a")$m\n',
            "fn()$"
        )),
        character()
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            'P <- R6::R6Class("P", cloneable = FALSE, public = list(v = NULL, initialize = function(v) self$v <- v, ',
            "get_priv = function() private), private = list(pm = function() list(ok = 1L)))\n",
            "o1 <- if (c1) P$new(1L) else P$new(2L)\n",
            'o2 <- if (c1) P$new(1L) else P$new("a")\n',
            "fn <- if (c2) o1$get_priv()$pm else o2$get_priv()$pm\n",
            "fn()$"
        )),
        character()
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            'P1 <- R6::R6Class("P", cloneable = FALSE, private = list(pm = function() list(p1 = 1L)))\n',
            'P2 <- R6::R6Class("P", cloneable = FALSE, private = list(pm = function() list(p2 = 1L)))\n',
            'mk <- function(P) R6::R6Class("Leaf", inherit = P, cloneable = FALSE, ',
            "public = list(m = function() private$pm()))\n",
            "box <- list(a = mk(P1)$new()$m, b = mk(P2)$new()$m)\n",
            "fn <- if (cond) box$a else box$b\n",
            "fn()$"
        )),
        character()
    )
    # Joined R6 generators (`gen$new`, signature help, and `inherit = gen`) + reverse soundness controls.
    g2f_code <- paste0(
        g2_depth,
        'mk <- function(P) R6::R6Class("Leaf", inherit = P, cloneable = FALSE, ',
        "public = list(initialize = function(alpha = 1L) NULL, m = function() list(gen_ok = 1L)))\n",
        "gen <- if (cond) mk(G1) else mk(G2)\n"
    )
    expect_identical(r6_review_cursor_labels(paste0(g2f_code, "gen$new()$m()$")), "gen_ok")
    expect_identical(doc_sig(paste0(g2f_code, "gen$new(")), "new(alpha = 1L)")
    expect_identical(
        r6_review_cursor_labels(paste0(
            g2_ov,
            'mk <- function(P) R6::R6Class("Mid", inherit = P, cloneable = FALSE, ',
            "private = list(secret = 99L), public = list(mm = function() list(from_mid = 1L)))\n",
            "gen <- if (cond) mk(P1) else mk(P2)\n",
            'Sub <- R6::R6Class("Sub", inherit = gen, cloneable = FALSE, public = list(sm = function() super$mm()))\n',
            "Sub$new()$sm()$"
        )),
        "from_mid"
    )
    g2f_rev <- paste0(
        g2_ov,
        'mk <- function(P) R6::R6Class("Mid", inherit = P, cloneable = FALSE, ',
        "public = list(mm = function() list(from_mid = 1L)))\n",
        "gen <- if (cond) mk(P1) else mk(P2)\n"
    )
    expect_identical(r6_review_cursor_labels(paste0(g2f_rev, "gen$new()$mm()$")), character())
    expect_identical(
        r6_review_cursor_labels(paste0(
            g2f_rev,
            'Sub <- R6::R6Class("Sub", inherit = gen, cloneable = FALSE, public = list(sm = function() super$mm()))\n',
            "Sub$new()$sm()$"
        )),
        character()
    )
    # Nested containers (`POS3`-`POS6`: unnamed list `[[1L]]`, R6 private field, list of generators/bound methods).
    nested_hdr <- paste0(
        g2_depth,
        'mk_in <- function(P) R6::R6Class("In", inherit = P, cloneable = FALSE, ',
        "private = list(s = list(in_ok = 1L)), public = list(ans = function() private$s))\n",
        'mk_out <- function(P) R6::R6Class("Out", inherit = P, cloneable = FALSE, ',
        "private = list(inner = mk_in(P)$new()), public = list(go = function() private$inner$ans()))\n"
    )
    expect_identical(
        r6_review_cursor_labels(
            paste0(nested_hdr, "xs <- if (cond) list(mk_in(G1)$new()) else list(mk_in(G2)$new())\nxs[[1L]]$ans()$")
        ),
        "in_ok"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(nested_hdr, "o <- if (cond) mk_out(G1)$new() else mk_out(G2)$new()\no$go()$")),
        "in_ok"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            nested_hdr,
            "bag <- if (cond) list(g = mk_in(G1), f = mk_in(G1)$new()$ans) else ",
            "list(g = mk_in(G2), f = mk_in(G2)$new()$ans)\n",
            "bag$g$new()$ans()$"
        )),
        "in_ok"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            nested_hdr,
            "bag <- if (cond) list(g = mk_in(G1), f = mk_in(G1)$new()$ans) else ",
            "list(g = mk_in(G2), f = mk_in(G2)$new()$ans)\n",
            "bag$f()$"
        )),
        "in_ok"
    )
    # `Reduce()` with a branched step body (+ non-overridden reverse soundness control).
    expect_identical(
        r6_review_cursor_labels(paste0(
            g2_ov,
            'mk <- function(P) R6::R6Class("C", inherit = P, cloneable = FALSE, ',
            "private = list(secret = 99L), public = list(ans = function() list(from_ans = 1L)))\n",
            "r <- Reduce(function(acc, el) if (cond) mk(P1)$new() else mk(P2)$new(), unknown_xs, init = mk(P1)$new())\n",
            "r$ans()$"
        )),
        "from_ans"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            g2_depth,
            'mk <- function(P) R6::R6Class("Leaf", inherit = P, cloneable = FALSE, ',
            "public = list(ans = function() list(from_leaf = 1L)))\n",
            "r <- Reduce(function(acc, el) if (cond) mk(G1)$new() else mk(G2)$new(), unknown_xs, init = mk(G1)$new())\n",
            "r$ans()$"
        )),
        "from_leaf"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            g2_ov,
            'mk <- function(P) R6::R6Class("C", inherit = P, cloneable = FALSE, ',
            "private = list(pm = function() private$out, out = list(from_out = 1L)), ",
            "public = list(ans = function() private$pm()))\n",
            "r <- Reduce(function(acc, el) if (cond) mk(P1)$new() else mk(P2)$new(), unknown_xs, init = mk(P1)$new())\n",
            "r$ans()$"
        )),
        character()
    )
})

test_that("Joining instance private with declared private is commutative and preserves inherited and outer scopes", {
    base_cls <- paste0(
        'B0 <- R6::R6Class("B0", cloneable = FALSE, ',
        "private = list(b0_sec = list(from_b0 = 1L), b0_pm = function() private$b0_sec))\n",
        'P <- R6::R6Class("P", inherit = B0, cloneable = FALSE, ',
        "public = list(v = NULL, initialize = function(v = 1) self$v <- v, get_priv = function() private), ",
        "private = list(sec = list(secret_ok = 1L), pm = function() private$sec))\n",
        "o_single <- P$new(1)\n",
        'o_joined <- if (c1) P$new(1) else P$new("a")\n'
    )
    # Commutativity on own private method `pm` (`character(0)` in both branch orders).
    expect_identical(
        r6_review_cursor_labels(paste0(base_cls, "p <- if (c2) o_single$get_priv() else o_joined$get_priv()\np$pm()$")),
        character()
    )
    expect_identical(
        r6_review_cursor_labels(paste0(base_cls, "p <- if (c2) o_joined$get_priv() else o_single$get_priv()\np$pm()$")),
        character()
    )
    # Inherited ancestor private method `b0_pm` (`r6_owner_level < r6_declared_level`) survives in both orders.
    expect_identical(
        r6_review_cursor_labels(
            paste0(base_cls, "p <- if (c2) o_single$get_priv() else o_joined$get_priv()\np$b0_pm()$")
        ),
        "from_b0"
    )
    expect_identical(
        r6_review_cursor_labels(
            paste0(base_cls, "p <- if (c2) o_joined$get_priv() else o_single$get_priv()\np$b0_pm()$")
        ),
        "from_b0"
    )
    # POS2_L11_inh_pm_only: Child with 0 own private methods inheriting Parent private method via list-boxed bound m.
    expect_identical(
        r6_review_cursor_labels(paste0(
            'Parent <- R6::R6Class("Parent", cloneable = FALSE, ',
            "private = list(psec = list(ok_inh = 1L), inh_pm = function() private$psec))\n",
            'Child <- R6::R6Class("Child", inherit = Parent, cloneable = FALSE, ',
            "public = list(v = NULL, initialize = function(v = 1) self$v <- v, m = function() private$inh_pm()), ",
            "private = list(cdat = 99L))\n",
            "o1 <- Child$new(1)\n",
            'o2 <- if (c1) Child$new(1) else Child$new("a")\n',
            "l <- list(a = o1$m, b = o2$m)\n",
            "fn <- if (c2) l$a else l$b\n",
            "fn()$"
        )),
        "ok_inh"
    )
    # Enclosing lexical `private` binding: single-vs-joined drops `pm` in both orders, while both-joined keeps it.
    outer_tpl <- paste0(
        "make_probe <- function(c1, c2) {\n",
        "    private <- list(sec = list(outer_leak = 1L))\n",
        '    P <- R6::R6Class("P", cloneable = FALSE, ',
        "public = list(v = NULL, initialize = function(v = 1) self$v <- v, get_priv = function() private), ",
        "private = list(sec = list(secret_ok = 1L), pm = function() private$sec))\n",
        "    %s\n",
        "    p$pm()$@@\n",
        "}\n"
    )
    expect_identical(
        r6_review_cursor_labels(sprintf(
            outer_tpl,
            'o1 <- P$new(1); o2 <- if (c1) P$new(1) else P$new("a"); p <- if (c2) o1$get_priv() else o2$get_priv()'
        )),
        character()
    )
    expect_identical(
        r6_review_cursor_labels(sprintf(
            outer_tpl,
            'o1 <- P$new(1); o2 <- if (c1) P$new(1) else P$new("a"); p <- if (c2) o2$get_priv() else o1$get_priv()'
        )),
        character()
    )
    expect_identical(
        r6_review_cursor_labels(sprintf(
            outer_tpl,
            paste0(
                'o1 <- if (c1) P$new(1) else P$new(2L); o2 <- if (c1) P$new(1) else P$new("a"); ',
                "p <- if (c2) o1$get_priv() else o2$get_priv()"
            )
        )),
        "outer_leak"
    )
    # SC10_decl_priv_diff_super: Declared `private` tables with diverged `super` methods do not share own-level `pm`.
    decl_sup <- paste0(
        'P1 <- R6::R6Class("P", cloneable = FALSE, public = list(sup_m = function() list(from_p1 = 1L)))\n',
        'P2 <- R6::R6Class("P", cloneable = FALSE, public = list(sup_m = function() list(from_p2 = 1L)))\n',
        'mk_d <- function(P, d) R6::R6Class("C", inherit = P, cloneable = FALSE, ',
        "public = list(v = NULL, initialize = function(v) self$v <- v, sup_m = function() 0L, ",
        "get_priv = function() private), private = list(only = d, pm = function() super$sup_m()))\n"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            decl_sup,
            'p <- if (c2) (if (c1) mk_d(P1, 1L)$new(1) else mk_d(P1, 1L)$new("a"))$get_priv() else ',
            '(if (c1) mk_d(P2, 2L)$new(1) else mk_d(P2, 2L)$new("a"))$get_priv()\n',
            "p$pm()$"
        )),
        character()
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            decl_sup,
            'p <- if (c2) (if (c1) mk_d(P2, 2L)$new(1) else mk_d(P2, 2L)$new("a"))$get_priv() else ',
            '(if (c1) mk_d(P1, 1L)$new(1) else mk_d(P1, 1L)$new("a"))$get_priv()\n',
            "p$pm()$"
        )),
        character()
    )
    # POS1_decl_shift: Depth-shifted declared `private` tables with identical `super` preserve `r6_declared_level`.
    expect_identical(
        r6_review_cursor_labels(paste0(
            "make_p <- function(c1, c2) {\n",
            "    private <- list(leak = list(outer_ok = 1L))\n",
            '    G0 <- R6::R6Class("G0", cloneable = FALSE)\n',
            '    G1 <- R6::R6Class("G", cloneable = FALSE)\n',
            '    G2 <- R6::R6Class("G", inherit = G0, cloneable = FALSE)\n',
            '    mk <- function(G, d) R6::R6Class("C", inherit = G, cloneable = FALSE,\n',
            "        public = list(v = NULL, initialize = function(v) self$v <- v, get_priv = function() private),\n",
            "        private = list(only = d, pm = function() private$leak)\n",
            "    )\n",
            '    p <- if (c2) (if (c1) mk(G1, 1L)$new(1) else mk(G1, 1L)$new("a"))$get_priv() else ',
            '(if (c1) mk(G2, 2L)$new(1) else mk(G2, 2L)$new("a"))$get_priv()\n',
            "    p$pm()$@@\n",
            "}"
        )),
        "outer_ok"
    )
    if (exists("member_r6_value_equiv", mode = "function")) {
        expect_false(member_r6_value_equiv(
            member_value(
                type = "function", function_expr = quote(function() list(ok = 1L)),
                receiver_name = "self", receiver_value = member_literal(1L)
            ),
            member_value(
                type = "function", function_expr = quote(function() list(ok = 1L)),
                receiver_name = "self", receiver_value = member_literal(2L)
            )
        ))
        idx_m7 <- member_generic_index("")
        idx_m7$r6_attached <- TRUE
        gen_m7 <- member_infer(
            quote(R6::R6Class("P", cloneable = FALSE, private = list(sec = list(ok = 1L), pm = function() private$sec))),
            idx_m7, list(), budget = member_request_budget()
        )
        inst_m7 <- gen_m7$fields$new$result_shape
        p_decl_m7 <- member_r6_declared_private(list(r6_layers = member_r6_all_layers(inst_m7)), 1L)
        expect_false(member_r6_value_equiv(inst_m7$r6_private, p_decl_m7))
        expect_false(member_r6_value_equiv(p_decl_m7, inst_m7$r6_private))
    }
})

test_that("R6 super table comparison preserves data-shadowed ancestor slot order and join commutativity", {
    make_doc_ws <- function(code, uri = "file:///r6-super-order.R") {
        lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]
        doc <- Document$new(uri, language = "r", version = 1L, content = lines)
        data <- parse_document(uri, lines)
        data$version <- 1L
        doc$update_parse_data(data)
        ws <- list(
            member_metadata = collections::dict(),
            get_documentation = function(...) NULL,
            get_signature = function(...) NULL,
            get_help = function(...) NULL
        )
        list(uri = uri, lines = lines, doc = doc, ws = ws)
    }
    doc_sig <- function(code) {
        ctx <- make_doc_ws(code)
        point <- list(row = length(ctx$lines) - 1L, col = nchar(tail(ctx$lines, 1L)))
        sigs <- signature_reply(1L, ctx$uri, ctx$ws, ctx$doc, point)$result$signatures
        if (length(sigs)) sigs[[1L]]$label else NULL
    }
    doc_hover <- function(code) {
        ctx <- make_doc_ws(code)
        point <- list(row = length(ctx$lines) - 1L, col = max(0L, nchar(tail(ctx$lines, 1L)) - 1L))
        hover_reply(1L, ctx$uri, ctx$ws, ctx$doc, point)$result$contents
    }
    resolve_val <- function(code) {
        ctx <- make_doc_ws(code)
        point <- list(row = length(ctx$lines) - 1L, col = nchar(tail(ctx$lines, 1L)))
        cursor <- member_cursor(ctx$doc, point)
        member_resolve_cursor(ctx$uri, ctx$ws, ctx$doc, point, cursor)$value
    }
    # 3-level R6 public data-to-method override order across completion, bound method,
    # generator join, `$new(` signature, method signature, hover, and `Reduce()` fixed-point convergence.
    setup_1a <- paste0(
        'G <- R6::R6Class("G", cloneable = FALSE, public = list(a = 10L, b = 20L))\n',
        "fa <- function() 3L\n",
        "fb <- function() 2L\n",
        "mkP <- function(first_b) {\n",
        "    if (first_b) {\n",
        '        R6::R6Class("P", inherit = G, cloneable = FALSE, public = list(b = fb, a = fa))\n',
        "    } else {\n",
        '        R6::R6Class("P", inherit = G, cloneable = FALSE, public = list(a = fa, b = fb))\n',
        "    }\n",
        "}\n",
        'mkL <- function(first_b) R6::R6Class("L", inherit = mkP(first_b), cloneable = FALSE, ',
        "public = list(initialize = function(z = 1L) NULL, ans = function(x = 1L) list(ok_1a = x)))\n"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(setup_1a, "o <- if (cond) mkL(TRUE)$new() else mkL(FALSE)$new()\no$ans()$")),
        "ok_1a"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(setup_1a, "fn <- if (cond) mkL(TRUE)$new()$ans else mkL(FALSE)$new()$ans\nfn()$")),
        "ok_1a"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(setup_1a, "g <- if (cond) mkL(TRUE) else mkL(FALSE)\ng$new()$ans()$")),
        "ok_1a"
    )
    expect_identical(
        doc_sig(paste0(setup_1a, "g <- if (cond) mkL(TRUE) else mkL(FALSE)\ng$new(")),
        "new(z = 1L)"
    )
    expect_identical(
        doc_sig(paste0(setup_1a, "o <- if (cond) mkL(TRUE)$new() else mkL(FALSE)$new()\no$ans(")),
        "ans(x = 1L)"
    )
    expect_identical(
        doc_hover(paste0(setup_1a, "o <- if (cond) mkL(TRUE)$new() else mkL(FALSE)$new()\no$ans")),
        c("```r\nans(x = 1L)\n```", "Public method of R6 class `L`")
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            setup_1a,
            "acc <- Reduce(function(acc, el) if (cond) mkL(TRUE)$new() else mkL(FALSE)$new(), ",
            "unknown_xs, init = mkL(TRUE)$new())\n",
            "acc$ans()$"
        )),
        "ok_1a"
    )
    # 3-level public (`C1 -> C2 -> C3`) and private (`D1 -> D2 -> D3`)
    # ancestor data slot reservation (`m = 123L` vs `m = 123L, z = 456L` overridden by methods at level 2).
    w1_both <- paste0(
        "m_fn <- function() list(pm = 1L)\n",
        "z_fn <- function() list(pz = 2L)\n",
        "pm_fn <- function() list(ppm = 1L)\n",
        "pz_fn <- function() list(ppz = 2L)\n",
        "u_fn <- function() super\n",
        "w_fn <- function() super$m()\n",
        "wp_fn <- function() super$pm()\n",
        "v_fn <- function() list(ok = 1L)\n",
        "pub_fn_reg <- function(cond) {\n",
        "    if (cond) {\n",
        '        R6::R6Class("C3",\n',
        '            inherit = R6::R6Class("C2",\n',
        '                inherit = R6::R6Class("C1", public = list(m = 123L)),\n',
        "                public = list(z = z_fn, m = m_fn)\n",
        "            ),\n",
        "            public = list(u = u_fn, w = w_fn, v = v_fn)\n",
        "        )$new()\n",
        "    } else {\n",
        '        R6::R6Class("C3",\n',
        '            inherit = R6::R6Class("C2",\n',
        '                inherit = R6::R6Class("C1", public = list(m = 123L, z = 456L)),\n',
        "                public = list(m = m_fn, z = z_fn)\n",
        "            ),\n",
        "            public = list(u = u_fn, w = w_fn, v = v_fn)\n",
        "        )$new()\n",
        "    }\n",
        "}\n",
        "prv_fn_reg <- function(cond) {\n",
        "    if (cond) {\n",
        '        R6::R6Class("D3",\n',
        '            inherit = R6::R6Class("D2",\n',
        '                inherit = R6::R6Class("D1", private = list(pm = 123L)),\n',
        "                private = list(pz = pz_fn, pm = pm_fn)\n",
        "            ),\n",
        "            public = list(u = u_fn, w = wp_fn, v = v_fn)\n",
        "        )$new()\n",
        "    } else {\n",
        '        R6::R6Class("D3",\n',
        '            inherit = R6::R6Class("D2",\n',
        '                inherit = R6::R6Class("D1", private = list(pm = 123L, pz = 123L)),\n',
        "                private = list(pm = pm_fn, pz = pz_fn)\n",
        "            ),\n",
        "            public = list(u = u_fn, w = wp_fn, v = v_fn)\n",
        "        )$new()\n",
        "    }\n",
        "}\n",
        "pub_u <- function(cond) pub_fn_reg(cond)$u()\n",
        "pub_w <- function(cond) pub_fn_reg(cond)$w()\n",
        "pub_v <- function(cond) pub_fn_reg(cond)$v()\n",
        "prv_u <- function(cond) prv_fn_reg(cond)$u()\n",
        "prv_w <- function(cond) prv_fn_reg(cond)$w()\n",
        "prv_v <- function(cond) prv_fn_reg(cond)$v()\n"
    )
    expect_identical(r6_review_cursor_labels(paste0(w1_both, "pub_u(ucond)$")), c("m", "z"))
    expect_identical(r6_review_cursor_labels(paste0(w1_both, "pub_w(ucond)$")), "pm")
    expect_identical(r6_review_cursor_labels(paste0(w1_both, "pub_v(ucond)$")), "ok")
    expect_identical(r6_review_cursor_labels(paste0(w1_both, "prv_u(ucond)$")), c("pm", "pz"))
    expect_identical(r6_review_cursor_labels(paste0(w1_both, "prv_w(ucond)$")), "ppm")
    expect_identical(r6_review_cursor_labels(paste0(w1_both, "prv_v(ucond)$")), "ok")
    # 4-level R6 hierarchy (`G -> M -> P -> L`) where every class declares `[a, b]` in the same order,
    # `M1` temporarily masks `a` with data `a = 10L` while `M2` does not, and `P` re-overrides `[a, b]`
    # with active bindings or public/private methods.
    setup_1b <- paste0(
        'G <- R6::R6Class("G", cloneable = FALSE, public = list(a = function() 1L, b = function() 2L))\n',
        "mkM <- function(mask_data) {\n",
        "    if (mask_data) {\n",
        '        R6::R6Class("M", inherit = G, cloneable = FALSE, public = list(a = 10L))\n',
        "    } else {\n",
        '        R6::R6Class("M", inherit = G, cloneable = FALSE, public = list())\n',
        "    }\n",
        "}\n",
        'mkP <- function(mask_data) R6::R6Class("P", inherit = mkM(mask_data), cloneable = FALSE, ',
        "active = list(a = function(v) 1L, b = function(v) 2L))\n",
        'mkL <- function(mask_data) R6::R6Class("L", inherit = mkP(mask_data), cloneable = FALSE, ',
        "public = list(initialize = function(z = 1L) NULL, ans = function(x = 1L) list(ok_1b = x)))\n"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(setup_1b, "o <- if (cond) mkL(TRUE)$new() else mkL(FALSE)$new()\no$ans()$")),
        "ok_1b"
    )
    expect_identical(
        doc_sig(paste0(setup_1b, "g <- if (cond) mkL(TRUE) else mkL(FALSE)\ng$new(")),
        "new(z = 1L)"
    )
    expect_identical(
        doc_sig(paste0(setup_1b, "o <- if (cond) mkL(TRUE)$new() else mkL(FALSE)$new()\no$ans(")),
        "ans(x = 1L)"
    )
    expect_identical(
        doc_hover(paste0(setup_1b, "o <- if (cond) mkL(TRUE)$new() else mkL(FALSE)$new()\no$ans")),
        c("```r\nans(x = 1L)\n```", "Public method of R6 class `L`")
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            "fa <- function() 1L\n",
            "fb <- function() 2L\n",
            'G <- R6::R6Class("G", cloneable = FALSE, public = list(a = 10L, b = fb))\n',
            'mkM <- function(m) if (m) R6::R6Class("M", inherit = G, cloneable = FALSE, public = list(a = 20L)) ',
            'else R6::R6Class("M", inherit = G, cloneable = FALSE, public = list())\n',
            'mkP <- function(m) if (m) R6::R6Class("P", inherit = mkM(TRUE), cloneable = FALSE, ',
            "public = list(b = fb, a = fa)) else ",
            'R6::R6Class("P", inherit = mkM(FALSE), cloneable = FALSE, ',
            "public = list(a = fa, b = fb))\n",
            'mkL <- function(m) R6::R6Class("L", inherit = mkP(m), cloneable = FALSE, ',
            "public = list(ans = function(x = 1L) list(ok_1b_pub = x)))\n",
            "o <- if (cond) mkL(TRUE)$new() else mkL(FALSE)$new()\n",
            "o$ans()$"
        )),
        "ok_1b_pub"
    )
    expect_identical(
        r6_review_cursor_labels(paste0(
            "fa <- function() 1L\n",
            "fb <- function() 2L\n",
            'G <- R6::R6Class("G", cloneable = FALSE, private = list(a = 10L, b = fb))\n',
            'mkM <- function(m) if (m) R6::R6Class("M", inherit = G, cloneable = FALSE, private = list(a = 20L)) ',
            'else R6::R6Class("M", inherit = G, cloneable = FALSE, private = list())\n',
            'mkP <- function(m) if (m) R6::R6Class("P", inherit = mkM(TRUE), cloneable = FALSE, ',
            "private = list(b = fb, a = fa)) else ",
            'R6::R6Class("P", inherit = mkM(FALSE), cloneable = FALSE, ',
            "private = list(a = fa, b = fb))\n",
            'mkL <- function(m) R6::R6Class("L", inherit = mkP(m), cloneable = FALSE, ',
            "public = list(ans = function(x = 1L) list(ok_1b_prv = x)))\n",
            "o <- if (cond) mkL(TRUE)$new() else mkL(FALSE)$new()\n",
            "o$ans()$"
        )),
        "ok_1b_prv"
    )
    # 3-level and 4-level reverse soundness controls (`character(0)`)
    # and join commutativity (`member_r6_value_equiv(f_u_ab, f_u_ba)`).
    setup_1c <- paste0(
        'G1 <- R6::R6Class("G", cloneable = FALSE, public = list(a = 10L, b = 20L))\n',
        'G2 <- R6::R6Class("G", cloneable = FALSE, public = list(b = 20L, a = 10L))\n',
        "fa <- function() 3L\n",
        "fb <- function() 2L\n",
        'mkP <- function(G) R6::R6Class("P", inherit = G, cloneable = FALSE, public = list(b = fb, a = fa))\n',
        'mkL <- function(G, v_val) R6::R6Class("L", inherit = mkP(G), cloneable = FALSE, ',
        "public = list(v = v_val, ans = function(x = 1L) list(ok_1c1 = x)))\n",
        "o <- if (cond) mkL(G1, 1L)$new() else mkL(G2, 2L)$new()\n"
    )
    expect_identical(r6_review_cursor_labels(paste0(setup_1c, "o$ans()$")), character())
    expect_null(doc_sig(paste0(setup_1c, "o$ans(")))
    expect_identical(
        r6_review_cursor_labels(paste0(
            'mkG <- function(f) if (f) R6::R6Class("G", cloneable = FALSE, ',
            "public = list(a = function() 1L, b = function() 2L)) else ",
            'R6::R6Class("G", cloneable = FALSE, public = list(b = function() 2L, a = function() 1L))\n',
            'mkM <- function(f) if (f) R6::R6Class("M", inherit = mkG(TRUE), cloneable = FALSE, ',
            'public = list(a = 10L)) else R6::R6Class("M", inherit = mkG(FALSE), cloneable = FALSE, public = list())\n',
            'mkP <- function(f) if (f) R6::R6Class("P", inherit = mkM(TRUE), cloneable = FALSE, ',
            "active = list(a = function(v) 1L, b = function(v) 2L)) else ",
            'R6::R6Class("P", inherit = mkM(FALSE), cloneable = FALSE, ',
            "active = list(b = function(v) 2L, a = function(v) 1L))\n",
            'mkL <- function(f, v_val) R6::R6Class("L", inherit = mkP(f), cloneable = FALSE, ',
            "public = list(v = v_val, ans = function(x = 1L) list(ok_1c3 = x)))\n",
            "o <- if (cond) mkL(TRUE, 1L)$new() else mkL(FALSE, 2L)$new()\n",
            "o$ans()$"
        )),
        character()
    )
    w1_3l <- paste0(
        "m_fn <- function() list(pm = 1L)\n",
        "z_fn <- function() list(pz = 2L)\n",
        "u_fn <- function() super\n",
        "w_fn <- function() super$m()\n",
        "v_fn <- function() list(ok = 1L)\n",
        "f_ab <- function(cond) {\n",
        "    if (cond) {\n",
        '        R6::R6Class("C3",\n',
        '            inherit = R6::R6Class("C2",\n',
        '                inherit = R6::R6Class("C1", public = list(m = 123L, z = z_fn)),\n',
        "                public = list(m = m_fn, z = z_fn)\n",
        "            ),\n",
        "            public = list(u = u_fn, w = w_fn, v = v_fn)\n",
        "        )$new()\n",
        "    } else {\n",
        '        R6::R6Class("C3",\n',
        '            inherit = R6::R6Class("C2",\n',
        '                inherit = R6::R6Class("C1", public = list(z = z_fn)),\n',
        "                public = list(m = m_fn, z = z_fn)\n",
        "            ),\n",
        "            public = list(u = u_fn, w = w_fn, v = v_fn)\n",
        "        )$new()\n",
        "    }\n",
        "}\n",
        "f_ba <- function(cond) {\n",
        "    if (cond) {\n",
        '        R6::R6Class("C3",\n',
        '            inherit = R6::R6Class("C2",\n',
        '                inherit = R6::R6Class("C1", public = list(z = z_fn)),\n',
        "                public = list(m = m_fn, z = z_fn)\n",
        "            ),\n",
        "            public = list(u = u_fn, w = w_fn, v = v_fn)\n",
        "        )$new()\n",
        "    } else {\n",
        '        R6::R6Class("C3",\n',
        '            inherit = R6::R6Class("C2",\n',
        '                inherit = R6::R6Class("C1", public = list(m = 123L, z = z_fn)),\n',
        "                public = list(m = m_fn, z = z_fn)\n",
        "            ),\n",
        "            public = list(u = u_fn, w = w_fn, v = v_fn)\n",
        "        )$new()\n",
        "    }\n",
        "}\n",
        "f_u_ab <- function(cond) f_ab(cond)$u()\n",
        "f_u_ba <- function(cond) f_ba(cond)$u()\n",
        "f_w_ab <- function(cond) f_ab(cond)$w()\n",
        "f_w_ba <- function(cond) f_ba(cond)$w()\n",
        "f_v_ab <- function(cond) f_ab(cond)$v()\n",
        "f_v_ba <- function(cond) f_ba(cond)$v()\n"
    )
    expect_identical(names(resolve_val(paste0(w1_3l, "f_ab(TRUE)$u()$"))$fields), c("m", "z"))
    expect_identical(names(resolve_val(paste0(w1_3l, "f_ab(FALSE)$u()$"))$fields), c("z", "m"))
    for (q in c(
        "f_u_ab(unknown_cond)$", "f_u_ba(unknown_cond)$",
        "f_w_ab(unknown_cond)$", "f_w_ba(unknown_cond)$",
        "f_v_ab(unknown_cond)$", "f_v_ba(unknown_cond)$"
    )) {
        expect_identical(r6_review_cursor_labels(paste0(w1_3l, q)), character())
    }
    u_ab_val <- resolve_val(paste0(w1_3l, "f_u_ab(unknown_cond)$"))
    u_ba_val <- resolve_val(paste0(w1_3l, "f_u_ba(unknown_cond)$"))
    expect_null(u_ab_val$fields)
    expect_null(u_ba_val$fields)
    if (exists("member_r6_value_equiv", mode = "function")) {
        expect_true(member_r6_value_equiv(u_ab_val, u_ba_val))
    } else {
        expect_identical(u_ab_val, u_ba_val)
    }
    expect_identical(
        r6_review_cursor_labels(paste0(
            "pm_fn <- function() list(ppm = 1L)\n",
            "pz_fn <- function() list(ppz = 2L)\n",
            "u_fn <- function() super\n",
            "f_prv <- function(cond) {\n",
            "    if (cond) {\n",
            '        R6::R6Class("D3",\n',
            '            inherit = R6::R6Class("D2",\n',
            '                inherit = R6::R6Class("D1", private = list(pm = 123L, pz = pz_fn)),\n',
            "                private = list(pm = pm_fn, pz = pz_fn)\n",
            "            ),\n",
            "            public = list(u = u_fn)\n",
            "        )$new()\n",
            "    } else {\n",
            '        R6::R6Class("D3",\n',
            '            inherit = R6::R6Class("D2",\n',
            '                inherit = R6::R6Class("D1", private = list(pz = pz_fn)),\n',
            "                private = list(pm = pm_fn, pz = pz_fn)\n",
            "            ),\n",
            "            public = list(u = u_fn)\n",
            "        )$new()\n",
            "    }\n",
            "}\n",
            "f_prv(unknown_cond)$u()$"
        )),
        character()
    )
    # Check (d1): portable = FALSE bare methods bound at level 1 vs level 2 preserve closure$super distinction
    expect_identical(
        r6_review_cursor_labels(paste0(
            "local({\n",
            "    f_np <- function() list(ok = 1L)\n",
            '    P_np <- R6::R6Class("C", portable = FALSE, cloneable = FALSE, public = list(m1 = f_np))\n',
            '    C_np <- R6::R6Class("C", inherit = P_np, portable = FALSE, cloneable = FALSE, public = list(\n',
            "        m2 = f_np,\n",
            "        run = function(cond) {\n",
            "            fn <- if (cond) m1 else m2\n",
            "            fn()$ok@@\n",
            "        }\n",
            "    ))\n",
            "})"
        )),
        character()
    )
})
