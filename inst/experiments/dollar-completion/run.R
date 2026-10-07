# From the languageserver repository root:
# Rscript inst/experiments/dollar-completion/run.R /path/to/r-polars
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
experiment_dir <- "inst/experiments/dollar-completion"
source(file.path(experiment_dir, "inference.R"))
source(file.path(experiment_dir, "polars-adapter.R"))

checks <- 0L
check <- function(ok, description) {
    if (!isTRUE(ok)) stop(description, call. = FALSE)
    checks <<- checks + 1L
}
measure <- function(expr) unname(system.time(expr)[["elapsed"]])
cold <- measure(index <- static_polars_index(args[[1L]]))

sample <- paste(c(
    'q <- pl$scan_csv(csv_file, infer_schema_files = 10)$filter(pl$col("Sepal.Length") > 5)$group_by(',
    '  "Species",',
    '  .maintain_order = TRUE',
    ')$agg(pl$all()$sum())'
), collapse = "\n")

# Every $ in the original example, with the right amount of delimiter repair.
# The receiver chain after the final ) and q$ exercise inferred assignments.
cursors <- gregexpr("$", sample, fixed = TRUE)[[1L]]
expected_types <- c("pl", "polars_lazy_frame", "pl", "polars_lazy_frame",
    "polars_lazy_group_by", "pl", "polars_expr")
expected_members <- c("scan_csv", "filter", "col", "group_by", "agg", "all", "sum")
closers <- c("", "", ")", "", "", ")", ")")
rows <- list()
for (i in seq_along(cursors)) {
    code <- substr(sample, 1L, cursors[[i]])
    result <- static_complete(code, index, attached = TRUE, closers = closers[[i]])
    check(identical(result$type, expected_types[[i]]), paste("receiver type at $", i))
    check(expected_members[[i]] %in% result$labels, paste("member at $", i))
    rows[[i]] <- data.frame(cursor = i, receiver = result$type,
        candidates = length(result$labels), expected = expected_members[[i]])
}
print(do.call(rbind, rows), row.names = FALSE)
for (code in c(paste0(sample, "$"), paste(sample, "q$", sep = "\n"))) {
    result <- static_complete(code, index, attached = TRUE)
    check(identical(result$type, "polars_lazy_frame"), "final query and assignment types")
    check("collect" %in% result$labels, "final query members")
}

# Static arguments are unnecessary for method shape. Even invalid runtime
# calls and expressions with side effects must yield completions without IO.
marker <- tempfile("static-completion-must-not-create-")
danger <- sprintf('pl$scan_csv({writeLines("executed", %s); stop("ran user code")})$fil',
    encodeString(marker, quote = '"'))
result <- static_complete(danger, index, attached = TRUE)
check("filter" %in% result$labels && all(startsWith(result$labels, "fil")),
    "unevaluated argument and partial prefix")
check(!file.exists(marker), "no user argument execution")
check(!"polars" %in% loadedNamespaces(), "no r-polars namespace loading")
result <- static_complete('polars::pl$col("x")$str$to_', index)
check(identical(result$type, "polars_namespace_expr_str"), "namespace property shape")
check("to_date" %in% result$labels, "namespace method completion")
for (code in c('pl$unknown()$', 'unknown()$', 'pl$col("x")$str$unknown()$',
    'pl <- unrelated\npl$', 'pl$scan_csv(x)$columns$', 'pl$scan_csv(x)$collect_schema()$')) {
    result <- static_complete(code, index, attached = TRUE)
    check(length(result$labels) == 0L, paste("unknown stops inference:", code))
}
check(length(static_complete("pl$", index)$labels) == 0L, "unresolved root is unknown")
check("scan_csv" %in% static_complete("library(polars)\npl$", index)$labels,
    "statically attached root")

# Change a native wrapper's declared output in the parsed AST. The resulting
# public completion shape must follow metadata rather than a filter lookup.
modified <- static_polars_index(args[[1L]])
fn <- modified$definitions$PlRLazyFrame_filter
rewrite <- function(node) {
    if (missing(node)) return(node)
    if (is.symbol(node) && identical(as.character(node), ".savvy_wrap_PlRLazyFrame")) {
        return(as.name(".savvy_wrap_PlRDataFrame"))
    }
    if (is.call(node)) {
        parts <- lapply(as.list(node), rewrite)
        return(as.call(parts))
    }
    node
}
modified$definitions$PlRLazyFrame_filter <- rewrite(fn)
result <- static_complete('pl$scan_csv(x)$filter(predicate)$', modified, attached = TRUE)
check(identical(result$type, "polars_data_frame"), "return type follows modified wrapper metadata")

# Package-independent factory shapes. Analyze the source without evaluating
# definitions or running the functions.
definitions <- paste(c(
    'list_factory <- function(x) list(filter = function(p) list(collect = function() x), value = x)',
    'env_factory <- function(x) {',
    '  self <- new.env(parent = emptyenv())',
    '  self$filter <- function(predicate) self',
    '  self$group_by <- function(...) list(agg = function(...) self)',
    '  self$collect <- function() list(value = x)',
    '  self',
    '}',
    'passthrough <- function(x) x',
    'same_shape <- function(flag) if (flag) list(value = 1) else list(value = 2)',
    'unknown_branch <- function(flag) if (flag) list(value = 1) else arbitrary()',
    'early_return <- function(flag) { if (flag) return(arbitrary()); list(value = 1) }',
    'recursive <- function() recursive()'
), collapse = "\n")
generic <- static_generic_index(definitions)
cases <- list(
    'list_factory(unresolved)$filter(predicate)$' = "collect",
    'env_factory(unresolved)$filter(predicate)$group_by("x")$agg()$' = c("collect", "filter", "group_by"),
    'env_factory(unresolved)$collect()$' = "value",
    'passthrough(list(alpha = 1, beta = 2))$' = c("alpha", "beta"),
    'same_shape(unresolved)$' = "value",
    'unknown_branch(unresolved)$' = character(),
    'early_return(unresolved)$' = character(),
    'recursive()$' = character(),
    'unknown()$' = character()
)
for (code in names(cases)) {
    result <- static_complete(code, generic)
    check(identical(result$labels, cases[[code]]), paste("generic shape:", code))
}
check(!exists("env_factory", globalenv(), inherits = FALSE), "no factory evaluation")
check(!exists("list_factory", globalenv(), inherits = FALSE), "no list factory evaluation")

# Connect cursor recovery to languageserver's existing native delimiter scanner.
# Loading languageserver here is development code, not execution of document
# source or any r-polars functions.
pkgload::load_all(quiet = TRUE, helpers = FALSE)
for (i in seq_along(cursors)) {
    prefix <- strsplit(substr(sample, 1L, cursors[[i]]), "\n", fixed = TRUE)[[1L]]
    repaired <- languageserver:::missing_closing_delimiters(prefix)
    check(identical(repaired, closers[[i]]), paste("native delimiter recovery:", i))
    result <- static_complete(paste(prefix, collapse = "\n"), index,
        attached = TRUE, closers = repaired)
    check(identical(result$type, expected_types[[i]]), paste("recovered receiver:", i))
}
for (code in c("pl$", "pl$scan", "pl$scan_csv(csv_file)$fil")) {
    document <- languageserver:::Document$new("file:///static-experiment.R", content = code)
    token <- document$detect_token(list(row = 0L, col = nchar(code)), forward = FALSE)
    check(identical(token$full_token, "") && identical(token$accessor, ""),
        "baseline scanner has no $ member context")
}

# Coverage means a non-Unknown static return shape, not runtime validation.
coverage <- do.call(rbind, lapply(c("pl", "polars_lazy_frame", "polars_lazy_group_by", "polars_expr"), function(type) {
    members <- index$members[[type]]
    methods <- names(members)[!is.na(members) & !startsWith(names(members), "_")]
    known <- vapply(methods, function(member) {
        bindings <- list(receiver = static_value(type = type))
        expr <- as.call(list(call("$", as.name("receiver"), as.name(member))))
        length(static_infer(expr, index, bindings)$type) > 0L
    }, logical(1L))
    data.frame(type = type, methods = length(methods), inferred = sum(known))
}))
print(coverage, row.names = FALSE)
warm <- measure(for (i in seq_len(1000L)) {
    static_complete(paste0(sample, "$"), index, attached = TRUE)
}) / 1000L
memory <- sum(vapply(as.list(index), function(value) {
    as.numeric(object.size(if (is.environment(value)) as.list(value) else value))
}, numeric(1L)))
cat(sprintf("Checks passed: %d\nIndex: %.3f s; warm parse + inference: %.3f ms/request\n",
    checks, cold, warm * 1000))
cat(sprintf("Approximate retained metadata: %.2f MiB\n", memory / 1024^2))
cat("R:", R.version.string, "\n")
