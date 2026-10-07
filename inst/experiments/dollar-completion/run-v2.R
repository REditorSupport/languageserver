# Rscript inst/experiments/dollar-completion/run-v2.R /path/to/r-polars
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
experiment_dir <- "inst/experiments/dollar-completion"
source(file.path(experiment_dir, "inference.R"))
source(file.path(experiment_dir, "polars-adapter.R"))
source(file.path(experiment_dir, "inference-v2.R"))
source(file.path(experiment_dir, "polars-adapter-v2.R"))
checks <- 0L
check <- function(ok, description) {
    if (!isTRUE(ok)) stop(description, call. = FALSE)
    checks <<- checks + 1L
}
cold <- system.time(index <- static_polars_index(args[[1L]]))[["elapsed"]]
expect_type <- function(code, type, member = NULL) {
    result <- static_complete(code, index, attached = TRUE)
    check(identical(result$type, type), paste("receiver type:", code))
    if (!is.null(member)) check(member %in% result$labels, paste("member:", code))
}
cases <- list(
    'pl$DataFrame(a=1:3)$' = c("polars_data_frame", "lazy"),
    'pl$LazyFrame(a=1:3)$filter(pl$col("a")>1)$collect()$' = c("polars_data_frame", "select"),
    'pl$DataFrame(a=1:3)$group_by("a")$agg(pl$all()$sum())$' = c("polars_data_frame", "select"),
    'pl$DataFrame(t=1:3)$group_by_dynamic("t", every="1i")$agg(pl$all()$sum())$' = c("polars_data_frame", "select"),
    'pl$DataFrame(t=1:3)$rolling("t", period="1i")$agg(pl$all()$sum())$' = c("polars_data_frame", "select"),
    'pl$col("a")$str$to_uppercase()$' = c("polars_expr", "alias"),
    'pl$col("a")$str$decode("hex")$' = c("polars_expr", "cast"),
    'pl$col("a")$list$sum()$' = c("polars_expr", "alias"),
    'pl$Series("a",1:3)$sum()$' = c("polars_series", "rename"),
    'pl$Series("a",1:3)$str$to_uppercase()$' = c("polars_series", "rename"),
    'as_polars_series(data.frame(a=1:3))$struct$unnest()$' = c("polars_data_frame", "select"),
    'cs$numeric()$' = c("polars_selector", "as_expr"),
    '(cs$numeric() & cs$starts_with("a"))$' = c("polars_selector", "as_expr"),
    '(pl$col("a")^2)$' = c("polars_expr", "alias"),
    'pl$when(pl$col("a")>0)$then(1)$when(pl$col("a")>2)$then(2)$otherwise(0)$' = c("polars_expr", "alias"),
    'pl$Int32$' = c("polars_dtype", "to_dtype_expr"),
    'pl$Int32$to_dtype_expr()$' = c("polars_datatype_expr", "default_value"),
    'pl$concat(pl$DataFrame(a=1),pl$DataFrame(a=2))$' = c("polars_data_frame", "select"),
    'pl$concat(pl$LazyFrame(a=1),pl$LazyFrame(a=2))$' = c("polars_lazy_frame", "collect"),
    'pl$concat(pl$Series("a",1),pl$Series("a",2))$' = c("polars_series", "rename"),
    '(pl$col("a")+pl$col("b"))$meta$pop()[[1]]$' = c("polars_expr", "alias"),
    'pl$col("a")$and(pl$col("b"),pl$col("c"))$' = c("polars_expr", "alias"),
    'pl$scan_parquet(x)$lazy_sink_parquet(y)$' = c("polars_lazy_frame", "collect"),
    'pl$QueryOptFlags()$' = c("QueryOptFlags", "predicate_pushdown")
)
for (code in names(cases)) expect_type(code, cases[[code]][[1L]], cases[[code]][[2L]])

sample <- paste(c(
    'q <- pl$scan_csv(csv_file, infer_schema_files = 10)$filter(pl$col("Sepal.Length") > 5)$group_by(',
    '  "Species", .maintain_order = TRUE',
    ')$agg(pl$all()$sum())'), collapse = "\n")
positions <- gregexpr("$", sample, fixed = TRUE)[[1L]]
types <- c("pl", "polars_lazy_frame", "pl", "polars_lazy_frame", "polars_lazy_group_by", "pl", "polars_expr")
closers <- c("", "", ")", "", "", ")", ")")
for (i in seq_along(positions)) {
    result <- static_complete(substr(sample, 1L, positions[[i]]), index, TRUE, closers[[i]])
    check(identical(result$type, types[[i]]), paste("original query cursor", i))
}
expect_type(paste(sample, "q$", sep = "\n"), "polars_lazy_frame", "collect")
for (code in c("as_polars_df(unresolved)$", "pl <- unrelated\npl$", "cs <- unrelated\ncs$",
    "pl$unknown()$", "pl$scan_csv(x)$columns$")) {
    check(length(static_complete(code, index, attached = TRUE)$labels) == 0L, paste("unknown:", code))
}
check(length(static_complete("pl$", index)$labels) == 0L, "unattached root stays unknown")
check("numeric" %in% static_complete("polars::cs$", index)$labels, "namespace-qualified selector root")
check("scan_csv" %in% static_complete("library(polars)\npl$", index)$labels, "syntactic import")
series <- static_complete('pl$Series("a",1:3)$', index, TRUE)
check(!any(c("exclude", "inspect", "over", "rolling") %in% series$labels), "Series excludes follow dispatch constants")
check(index$members$polars_namespace_series_struct[["unnest"]] == "series_struct_unnest", "Series own method precedes delegated Expr method")
delegated <- index$members$polars_series[["sum"]]
check(identical(index$definitions[[delegated]][[2L]], index$definitions$expr__sum[[2L]]), "delegated signature comes from current method")

generic_code <- paste(c(
    'early <- function(flag) { if(flag) return(list(shared=1, a=1)); list(shared=2,b=2) }',
    'uncertain <- function(flag) { if(flag) return(arbitrary()); list(a=1) }',
    'loop <- function(xs) { out <- list(a=1); for(x in xs) out <- arbitrary(); out }',
    'passthrough <- function(x) x',
    'match_args <- function(x, long_name=TRUE, ..., after=FALSE) if(long_name) x else list(other=1)',
    'choice <- function(x) switch(x,a=list(alpha=1),b=list(beta=1),list(fallback=1))',
    'dispatch <- function(x) UseMethod("dispatch")',
    'dispatch.foo <- function(x) NextMethod()',
    'dispatch.bar <- function(x) list(inherited=1)',
    'dispatch.default <- function(x) list(default=1)',
    'closure_call <- function(f) f()',
    'factory <- function(x) function() x',
    'new.env <- function(...) arbitrary()',
    'shadowed <- function() new.env()'
), collapse = "\n")
generic <- static_generic_index(generic_code)
generic$classes <- list()
expect_labels <- function(code, labels) {
    result <- static_complete(code, generic)
    check(identical(result$labels, labels), paste("general inference:", code))
}
expect_labels("early(unknown)$", "shared")
expect_labels("uncertain(unknown)$", character())
expect_labels("loop(unknown)$", character())
expect_labels("passthrough(list(a=1,b=2))$", c("a", "b"))
expect_labels("match_args(list(a=1),long=FALSE)$", "other")
expect_labels("choice(\"a\")$", "alpha")
expect_labels("choice(\"b\")$", "beta")
expect_labels("choice(unresolved)$", character())
expect_labels('dispatch(structure(list(),class=c("foo","bar")))$', "inherited")
expect_labels("dispatch(unresolved)$", character())
expect_labels("closure_call(factory(list(a=1)))$", "a")
expect_labels("closure_call(factory(list(b=1)))$", "b")
expect_labels("shadowed()$", character())
expect_labels("list <- function(...) arbitrary()\nlist(a=1)$", character())
expect_labels("tryCatch(list(a=1),error=function(e) list(b=1))$", character())

# User constructors, methods, defaults, dispatch/hooks, and arguments are AST
# data. Nothing in this expression may run during inference.
marker <- tempfile("v2-must-not-execute-")
danger <- sprintf('pl$scan_csv({writeLines("ran", %s); stop("user argument")})$filter({stop("predicate")})$', encodeString(marker, quote = '"'))
expect_type(danger, "polars_lazy_frame", "collect")
canary <- static_generic_index(sprintf('f <- function(x={writeLines("ran",%s); stop("default")}) list(a=1)', encodeString(marker, quote = '"')))
check("a" %in% static_complete("f()$", canary)$labels, "default analyzed without execution")
check(!file.exists(marker) && !"polars" %in% loadedNamespaces(), "user/native canaries intact")

custom <- paste(c(
    'shortcuts <- function(s) { self <- new.env(); self$`_s` <- s;',
    'self$square <- function() self$`_s`*self$`_s`; self$cube <- function() self$`_s`*self$`_s`*self$`_s`;',
    'class(self) <- c("custom_namespace","polars_object"); self }',
    'pl$api$register_series_namespace("math", shortcuts)',
    's <- as_polars_series(1:3)',
    's$math$square()$'), collapse = "\n")
check("rename" %in% static_complete(custom, index, TRUE)$labels, "literal custom registration and result")
check(!"math" %in% static_complete('as_polars_series(1:3)$', index, TRUE)$labels, "registration stays document-local")

# Native output mutation changes type, rather than relying on a method table.
changed <- static_polars_index(args[[1L]])
rewrite <- function(node) {
    if (missing(node)) return(node)
    if (is.symbol(node) && identical(as.character(node), ".savvy_wrap_PlRLazyFrame")) return(as.name(".savvy_wrap_PlRDataFrame"))
    if (is.call(node)) return(as.call(lapply(as.list(node), rewrite)))
    node
}
changed$definitions$PlRLazyFrame_filter <- rewrite(changed$definitions$PlRLazyFrame_filter)
changed$cache <- new.env(parent = emptyenv())
check(identical(static_complete('pl$scan_csv(x)$filter(p)$', changed, TRUE)$type, "polars_data_frame"), "changed native result follows body")
# A shallow recursive request must not pollute later independent requests.
probe <- static_complete('pl$select(pl$date_range(as.Date("2021-01-01"),as.Date("2021-01-05")))$', index, TRUE)
check(identical(probe$type, "polars_data_frame"), "summary after recursive traversal")
source(file.path(experiment_dir, "runtime-inspection.R"))
fixture <- new.env(parent = emptyenv())
fixture$api <- new.env(parent = emptyenv())
fixture$api$factory <- function(x) list(next_method = function() list(finish = x))
makeActiveBinding("unsafe", function() {
    writeLines("getter", marker)
    stop("getter ran")
}, fixture$api)
delayedAssign("deferred", {writeLines("promise", marker); stop("promise ran")}, assign.env = fixture$api)
runtime <- inspection_shape_index(inspection_snapshot(fixture))
check("finish" %in% static_complete('api$factory(unresolved)$next_method()$', runtime, TRUE)$labels,
    "runtime registry body uses same chain engine")
check("unsafe" %in% static_complete("api$", runtime, TRUE)$labels, "active name offered without value")
check(length(static_complete("api$unsafe$", runtime, TRUE)$labels) == 0L, "active value stays unknown")
check(length(static_complete("api$deferred$", runtime, TRUE)$labels) == 0L, "runtime promise stays unknown")
fixture$container <- list(registry = fixture$api)
snapshot <- inspection_snapshot(fixture)
check(identical(snapshot$fields$container$kind, "list"), "list of registries read without dispatch")
getter <- local({label <- "current"; function() label})
makeActiveBinding("literal_getter", getter, fixture$api)
first <- inspection_snapshot(fixture)
environment(getter)$label <- "changed"
second <- inspection_snapshot(fixture)
check(!identical(inspection_fingerprint(first), inspection_fingerprint(second)), "captured getter literal invalidates snapshot")
check(!file.exists(marker), "runtime getter/promise canaries intact")
dataset_shapes <- inspection_data_shapes("datasets")
check(identical(dataset_shapes$mtcars$type, "data.frame"), "serialized data class extracted")
check(all(c("mpg", "cyl") %in% names(dataset_shapes$mtcars$fields)), "serialized data fields extracted")
check(isTRUE(rlang::env_binding_are_lazy(as.environment("package:datasets"), "mtcars")[[1L]]), "attached dataset promise not forced")
warm <- system.time(for (i in seq_len(100L)) static_complete(paste0(sample, "\nq$"), index, TRUE))[["elapsed"]] / 100L
cat("Extended inference checks:", checks, "passed\n")
cat(sprintf("Source index %.3f s; warm original query %.3f ms/request\n", cold, warm * 1000))
cat("No Polars namespace loaded and no user expression evaluated.\n")
