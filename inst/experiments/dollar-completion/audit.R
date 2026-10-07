# Corpus audit; optional third argument v2 selects extended inference/extraction.
# Rscript inst/experiments/dollar-completion/audit.R /path/to/r-polars /output/dir [v2|v2-data|production]
# Parses all canonical user articles and generated Rd examples; never runs them.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) %in% c(2L, 3L))
root <- normalizePath(args[[1L]])
output <- args[[2L]]
dir.create(output, recursive = TRUE, showWarnings = FALSE)
production <- length(args) == 3L && identical(args[[3L]], "production")
extended <- production || length(args) == 3L && args[[3L]] %in% c("v2", "v2-data")
if (production) {
    pkgload::load_all(quiet = TRUE, helpers = FALSE)
    ns <- asNamespace("languageserver")
    for (name in ls(ns, all.names = TRUE)) {
        if (startsWith(name, "member_")) assign(sub("^member_", "static_", name), get(name, ns))
    }
    static_polars_index <- function(root) member_package_index(member_source_input(root))
} else {
    source("inst/experiments/dollar-completion/inference.R")
    source("inst/experiments/dollar-completion/polars-adapter.R")
    if (extended) {
        source("inst/experiments/dollar-completion/inference-v2.R")
        source("inst/experiments/dollar-completion/polars-adapter-v2.R")
    }
}
index <- static_polars_index(root)
data_roots <- list()
if (length(args) == 3L && identical(args[[3L]], "v2-data")) {
    source("inst/experiments/dollar-completion/runtime-inspection.R")
    data_roots <- inspection_data_shapes("datasets")
    for (name in names(data_roots)) index$namespace_roots[paste0("datasets::", name)] <- list(data_roots[[name]])
}

relative <- function(path) substring(path, nchar(root) + 2L)
compact <- function(node, max_chars = 180L) {
    if (missing(node)) return("")
    substr(paste(deparse(node, width.cutoff = 500L), collapse = " "), 1L, max_chars)
}
safe_infer <- function(node, bindings) {
    tryCatch(static_infer(node, index, bindings), error = function(e) {
        value <- static_value()
        attr(value, "error") <- conditionMessage(e)
        value
    })
}
is_seed <- function(node) {
    if (missing(node)) return(FALSE)
    key <- static_name(node)
    if (!is.null(key) && (key %in% c("pl", "cs") ||
            startsWith(key, "as_polars_") || startsWith(key, "is_polars_"))) return(TRUE)
    if (static_head(node, "::") && identical(static_name(node[[2L]]), "polars")) return(TRUE)
    FALSE
}
related <- function(node, origins) {
    result <- FALSE
    static_walk(node, function(child) {
        if (is_seed(child) || is.symbol(child) && as.character(child) %in% names(origins) &&
                isTRUE(origins[[as.character(child)]])) result <<- TRUE
    })
    result
}

extract_rd <- function(path) {
    rd <- tools::parse_Rd(path)
    examples <- rd[vapply(rd, function(node) identical(attr(node, "Rd_tag"), "\\examples"), logical(1L))]
    if (!length(examples)) return(list())
    # Rd2ex is static text conversion. Include dontrun/donttest, preserving
    # examplesIf scaffolding; no example or macro expression is evaluated.
    code <- capture.output(tools::Rd2ex(rd, out = "", commentDontrun = FALSE, commentDonttest = FALSE))
    list(list(code = paste(code, collapse = "\n"),
        line = as.integer(attr(examples[[1L]], "srcref")[[1L]]), block = "examples"))
}
extract_fences <- function(path) {
    lines <- readLines(path, warn = FALSE)
    blocks <- list()
    start <- NULL
    fence <- NULL
    language <- NULL
    for (i in seq_along(lines)) {
        line <- lines[[i]]
        if (is.null(start)) {
            match <- regexec("^\\s*(`{3,}|~{3,})(.*)$", line, perl = TRUE)
            pieces <- regmatches(line, match)[[1L]]
            if (length(pieces)) {
                fence <- pieces[[2L]]
                language <- trimws(pieces[[3L]])
                start <- i
            }
        } else if (grepl(paste0("^\\s*", fence, "\\s*$"), line)) {
            if (grepl("^(r|R)(\\s|$)|^\\{[rR]([ ,}]|$)", language)) {
                blocks[[length(blocks) + 1L]] <- list(
                    code = paste(lines[seq.int(start + 1L, i - 1L)], collapse = "\n"),
                    line = start + 1L, block = language)
            }
            start <- NULL
        }
    }
    blocks
}

manual <- sort(list.files(file.path(root, "man"), "\\.Rd$", full.names = TRUE))
articles <- c(file.path(root, "README.Rmd"),
    sort(list.files(file.path(root, "vignettes"), "\\.[RrqQ]md$", full.names = TRUE)),
    file.path(root, "altdoc", "reference_home.Rmd"))
supplemental <- file.path(root, c("DEVELOPMENT.md", "NEWS.md"))
files <- c(articles, manual, supplemental)
inventory <- list()
file_inventory <- list()
rows <- list()
contexts <- list()

for (path in files) {
    corpus <- if (path %in% manual) "reference" else if (path %in% articles) "articles" else "supplemental"
    blocks <- if (path %in% manual) extract_rd(path) else extract_fences(path)
    file_inventory[[length(file_inventory) + 1L]] <- data.frame(
        corpus = corpus, path = relative(path), r_blocks = length(blocks))
    bindings <- c(data_roots, index$roots)
    origins <- list(pl = TRUE, cs = TRUE)
    for (block_i in seq_along(blocks)) {
        block <- blocks[[block_i]]
        parsed <- tryCatch(parse(text = block$code, keep.source = TRUE), error = function(e) e)
        inventory[[length(inventory) + 1L]] <- data.frame(
            corpus = corpus, path = relative(path), block = block_i,
            line = block$line, parsed = !inherits(parsed, "error"),
            error = if (inherits(parsed, "error")) conditionMessage(parsed) else "",
            stringsAsFactors = FALSE)
        if (inherits(parsed, "error")) next

        # Record every real $ AST node, including nested call receivers. This
        # avoids counting dollars in strings, regular expressions, comments,
        # Python, or prose. Top-level assignments are propagated in source order.
        scan <- function(node, env, org, in_function = FALSE, guarded = FALSE) {
            if (missing(node) || !is.call(node)) return(invisible(NULL))
            if (static_head(node, "$")) {
                value <- safe_infer(node[[2L]], env)
                type <- paste(value$type, collapse = "|")
                members <- if (extended) static_members(value, index, env) else static_lookup(index$members, value$type)
                if (!extended && !is.null(value$fields)) members <- setNames(rep(NA_character_, length(value$fields)), names(value$fields))
                member <- static_name(node[[3L]])
                is_related <- related(node[[2L]], org)
                reason <- if (is.null(member)) "dynamic_member" else if (!nzchar(type)) {
                    if (is.symbol(node[[2L]]) && !as.character(node[[2L]]) %in% names(env)) "unbound_receiver" else "unknown_result"
                } else if (!length(members)) "no_member_metadata" else if (!member %in% names(members)) {
                    "member_missing"
                } else "offered"
                rows[[length(rows) + 1L]] <<- data.frame(
                    corpus = corpus, path = relative(path), block = block_i,
                    source_line = block$line, receiver = compact(node[[2L]]), member = if (is.null(member)) "" else member,
                    inferred_type = type, candidates = length(names(members)),
                    offered = identical(reason, "offered"), reason = reason,
                    polars_related = is_related, in_function = in_function,
                    guarded = guarded, error = if (is.null(attr(value, "error"))) "" else attr(value, "error"),
                    stringsAsFactors = FALSE)
                contexts[[length(rows)]] <<- list(node = node, bindings = env)
            }
            if (static_head(node, "function")) {
                local_env <- env
                local_org <- org
                # Avoid assuming the caller's type for untyped callback params.
                for (formal in names(node[[2L]])) {
                    local_env[formal] <- list(static_value())
                    local_org[formal] <- list(FALSE)
                }
                scan(node[[3L]], local_env, local_org, TRUE, guarded)
                return(invisible(NULL))
            }
            if (static_head(node, "{")) {
                for (child in as.list(node)[-1L]) {
                    scan(child, env, org, in_function, guarded)
                    if (static_head(child, "<-") && is.symbol(child[[2L]])) {
                        name <- as.character(child[[2L]])
                        env[name] <- list(safe_infer(child[[3L]], env))
                        org[name] <- list(related(child[[3L]], org))
                    }
                }
                return(invisible(NULL))
            }
            for (child in as.list(node)) scan(child, env, org, in_function,
                guarded || static_head(node, "if"))
            invisible(NULL)
        }
        consume <- function(expr, guarded = FALSE) {
            # Rd examplesIf puts the examples in withAutoprint({ ... }). The
            # audit treats this scaffolding as a documentation wrapper, without
            # evaluating the guard, and keeps guarded status in the output.
            if (static_head(expr, "if") && length(expr) >= 3L &&
                    static_head(expr[[3L]], "withAutoprint")) {
                body <- expr[[3L]][[2L]]
                for (child in as.list(body)[-1L]) consume(child, guarded = TRUE)
                return(invisible(NULL))
            }
            scan(expr, bindings, origins, guarded = guarded)
            if (static_head(expr, "<-") && is.symbol(expr[[2L]])) {
                name <- as.character(expr[[2L]])
                bindings[name] <<- list(safe_infer(expr[[3L]], bindings))
                origins[name] <<- list(related(expr[[3L]], origins))
            }
            if (extended) bindings <<- static_registration_effect(expr, index, bindings)
            invisible(NULL)
        }
        for (expr in parsed) consume(expr)
    }
    cat(relative(path), "\n")
}
inventory <- do.call(rbind, inventory)
rows <- do.call(rbind, rows)
write.csv(inventory, file.path(output, "blocks.csv"), row.names = FALSE)
write.csv(do.call(rbind, file_inventory), file.path(output, "files.csv"), row.names = FALSE)
write.csv(rows, file.path(output, "dollars.csv"), row.names = FALSE)
saveRDS(contexts, file.path(output, "contexts.rds"))

aggregate_rows <- function(data, column) {
    groups <- split(data, data[[column]])
    do.call(rbind, lapply(names(groups), function(name) {
        x <- groups[[name]]
        data.frame(group = name, dollars = nrow(x), polars_related = sum(x$polars_related),
            receivers_known = sum(nzchar(x$inferred_type)), offered = sum(x$offered),
            offered_percent = round(mean(x$offered) * 100, 1L))
    }))
}
summary <- aggregate_rows(rows, "corpus")
by_file <- aggregate_rows(rows, "path")
write.csv(summary, file.path(output, "summary.csv"), row.names = FALSE)
write.csv(by_file, file.path(output, "by-file.csv"), row.names = FALSE)
write.csv(aggregate_rows(rows[rows$polars_related, ], "path"),
    file.path(output, "polars-by-file.csv"), row.names = FALSE)
write.csv(aggregate_rows(rows[rows$polars_related, ], "corpus"),
    file.path(output, "polars-summary.csv"), row.names = FALSE)
write.csv(aggregate_rows(rows[rows$polars_related, ], "reason"),
    file.path(output, "reasons.csv"), row.names = FALSE)
polars <- rows[rows$polars_related, ]
polars$root <- polars$receiver %in% c("pl", "cs", "polars::pl", "polars::cs")
root_rows <- do.call(rbind, lapply(split(polars, polars$corpus), function(x) {
    do.call(rbind, lapply(c(TRUE, FALSE), function(root) {
        data <- x[x$root == root, ]
        data.frame(corpus = x$corpus[[1L]], direct_root = root,
            dollars = nrow(data), offered = sum(data$offered),
            offered_percent = round(mean(data$offered) * 100, 1L))
    }))
}))
write.csv(root_rows, file.path(output, "roots.csv"), row.names = FALSE)
source_inventory <- as.data.frame(table(unlist(index$locations)), stringsAsFactors = FALSE)
names(source_inventory) <- c("path", "top_level_definitions")
source_paths <- sort(list.files(file.path(root, "R"), "\\.[Rr]$"))
source_inventory <- merge(data.frame(path = source_paths), source_inventory, all.x = TRUE)
source_inventory$top_level_definitions[is.na(source_inventory$top_level_definitions)] <- 0L
write.csv(source_inventory, file.path(output, "source-files.csv"), row.names = FALSE)

# Inspect all registries, including namespace surfaces, with a known receiver.
# This metric deliberately does not assume examples successfully construct it.
surfaces <- list()
method_rows <- list()
types <- names(index$members)[!startsWith(names(index$members), "polars::")]
if (extended) types <- setdiff(types, setdiff(names(index$registries), c("pl", "cs", "pl__api")))
for (type in types) {
    members <- index$members[[type]]
    member_names <- as.character(names(members))
    methods <- member_names[!is.na(members) & !startsWith(member_names, "_")]
    known <- 0L
    for (member in methods) {
        expr <- as.call(list(call("$", as.name("receiver"), as.name(member))))
        result <- safe_infer(expr, list(receiver = static_value(type = type)))
        if (length(result$type)) known <- known + 1L
        method_rows[[length(method_rows) + 1L]] <- data.frame(type = type, member = member,
            function_key = members[[member]], result = paste(result$type, collapse = "|"),
            known = length(result$type) > 0L, stringsAsFactors = FALSE)
    }
    surfaces[[length(surfaces) + 1L]] <- data.frame(type = type,
        members = sum(!startsWith(member_names, "_")), methods = length(methods), inferred = known)
}
write.csv(do.call(rbind, surfaces), file.path(output, "surfaces.csv"), row.names = FALSE)
write.csv(do.call(rbind, method_rows), file.path(output, "methods.csv"), row.names = FALSE)

cat("\nAll corpus dollar occurrences:\n")
print(summary, row.names = FALSE)
cat("\nSyntactically Polars-related:\n")
print(aggregate_rows(rows[rows$polars_related, ], "corpus"), row.names = FALSE)
cat("\nBlocks:", nrow(inventory), "Parse failures:", sum(!inventory$parsed),
    "Inference errors:", sum(nzchar(rows$error)), "\n")
stopifnot(!"polars" %in% loadedNamespaces())
