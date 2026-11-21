# R_LIBS=/path/to/library Rscript --vanilla scoped-inference.R results.csv [files] [calls] [samples]
# Time provider work after indexing and parsing. Each S3 request uses a different
# call site. Each member request starts without a cached receiver for the cursor.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 1L, length(args) <= 4L)
size <- function(i, default) {
    n <- if (length(args) >= i) as.integer(args[[i]]) else default
    stopifnot(!is.na(n), n >= 1L)
    n
}
files <- size(2L, 1000L)
calls <- size(3L, 1000L)
samples <- size(4L, 7L)
ns <- asNamespace("languageserver")

main <- function() {
    root <- tempfile("scoped-inference-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE))
    dir.create(file.path(root, "R"))
    writeLines(c("Package: scopedbench", "Version: 0.0.1"), file.path(root, "DESCRIPTION"))
    writeLines(character(), file.path(root, "NAMESPACE"))
    ns$lsp_settings$set("index_persistent_cache", FALSE)
    ns$lsp_settings$set("diagnostics", FALSE)
    ns$lsp_settings$set("s3_dispatch", TRUE)
    workspace <- ns$Workspace$new(root)
    for (pkg in ns$startup_packages) workspace$s3_metadata$set(pkg, ns$s3_prepare_namespace(pkg))

    open_document <- function(name, content) {
        path <- file.path(root, "R", name)
        writeLines(content, path)
        uri <- ns$path_to_uri(path)
        doc <- ns$Document$new(uri, version = 1L, content = content)
        workspace$documents$set(uri, doc)
        parsed <- ns$parse_document(uri, content)
        parsed$version <- doc$version
        workspace$update_parse_data(uri, parsed)
        doc
    }
    generic_lines <- c("g <- function(x, ...) UseMethod('g')",
        "g.numeric <- function(x, digits = 3, ...) NULL", "object <- 42", rep("g(object, )", samples))
    generic <- open_document("generic.R", generic_lines)
    for (i in seq_len(files - 1L)) {
        path <- file.path(root, "R", sprintf("module-%04d.R", i))
        writeLines(sprintf("helper_%d <- function(x) x", i), path)
        workspace$index$update_path(path)
    }
    stopifnot(length(ns$s3_context_sources(generic$uri, workspace, generic)) == files)
    measure <- function(name, count, request, validate) {
        rows <- lapply(seq_len(samples), function(i) {
            gc()
            start <- proc.time()
            result <- request(i)
            duration <- proc.time() - start
            stopifnot(validate(result))
            data.frame(workload = name, size = count, sample = i, elapsed_ms = 1000 * duration[[3L]],
                cpu_ms = 1000 * sum(duration[1:2]))
        })
        do.call(rbind, rows)
    }
    s3 <- measure("indexed package S3 dispatch", files, function(i) {
        ns$resolve_s3_call(generic$uri, workspace, generic, list(row = i + 2L, col = 10L))
    }, function(result) identical(result$method$name, "g.numeric"))

    member_lines <- c("pipeline <- function() {", "object <- list(field = 1)",
        sprintf("trace_output(arg_%05d)", seq_len(calls)), "object$field", "}")
    member <- open_document("pipeline.R", member_lines)
    point <- list(row = calls + 2L, col = 7L)
    completion <- measure("function member completion", calls, function(i) {
        member$member_receivers$clear()
        ns$completion_reply(1L, member$uri, workspace, member, point, list())
    }, function(reply) "field" %in% vapply(reply$result$items, `[[`, "", "label"))
    result <- rbind(s3, completion)
    utils::write.csv(result, args[[1L]], row.names = FALSE)
    print(stats::aggregate(cbind(elapsed_ms, cpu_ms) ~ workload + size, result, stats::median))
}

main()
