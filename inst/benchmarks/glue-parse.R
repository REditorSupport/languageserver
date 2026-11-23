# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/glue-parse.R label 200,1000,3000 5 > parse.csv
# Elapsed times exclude startup, fixture construction, GC before the parse,
# serialization, and the separate allocation-profile pass.
args <- commandArgs(trailingOnly = TRUE)
label <- if (length(args)) args[[1L]] else "installed"
sizes <- if (length(args) > 1L) as.integer(strsplit(args[[2L]], ",", fixed = TRUE)[[1L]]) else c(200L, 1000L, 3000L)
rounds <- if (length(args) > 2L) as.integer(args[[3L]]) else 5L
stopifnot(all(sizes > 0L), rounds > 0L)

ns <- asNamespace("languageserver")
supports_glue <- exists("glue_template_index", ns, inherits = FALSE)
shapes <- c("ordinary", "sparse-same", "sparse-unique", "dense-same", "dense-unique")
cases <- expand.grid(lines = sizes, shape = shapes, stringsAsFactors = FALSE)
sources <- lapply(seq_len(nrow(cases)), function(i) {
    ids <- seq_len(cases$lines[[i]])
    shape <- cases$shape[[i]]
    names <- sprintf("v_%05d", ids)
    lines <- sprintf('%s <- paste0("Value {v_%05d}")', names, ids)
    at <- if (startsWith(shape, "sparse")) seq.int(1L, length(ids), by = 50L) else if (shape == "ordinary") integer() else ids
    symbol <- if (endsWith(shape, "same")) rep("field", length(at)) else sprintf("field_%05d", at)
    lines[at] <- sprintf('%s <- glue::glue("Value {%s}")', names[at], symbol)
    list(lines = lines, expected = length(at))
})

parse_one <- function(fixture) ns$parse_document("file:///bench.R", fixture$lines, include_xml = FALSE)
for (i in c(1L, nrow(cases))) invisible(parse_one(sources[[i]]))

# Vary the order of sizes and shapes between rounds, with the same schedule
# available for every installed revision.
set.seed(7439L)
times <- matrix(NA_real_, nrow(cases), rounds)
for (pass in seq_len(rounds)) {
    for (i in sample.int(nrow(cases))) {
        gc()
        start <- proc.time()[["elapsed"]]
        parsed <- parse_one(sources[[i]])
        times[i, pass] <- 1000 * (proc.time()[["elapsed"]] - start)
        stopifnot(!parsed$parse_error, length(parsed$glue_templates) == if (supports_glue) sources[[i]]$expected else 0L)
        rm(parsed)
    }
}

# Rprofmem records cumulative allocations. The serialized worker snapshot and
# its interpolation index measure the output retained for the main process.
storage <- function(fixture) {
    allocated <- NA_real_
    if (capabilities("profmem")) {
        file <- tempfile()
        on.exit(unlink(file), add = TRUE)
        gc()
        utils::Rprofmem(file)
        parsed <- tryCatch(parse_one(fixture), finally = utils::Rprofmem(NULL))
        records <- readLines(file, warn = FALSE)
        bytes <- suppressWarnings(as.double(sub(" .*", "", records)))
        allocated <- sum(bytes, na.rm = TRUE)
    } else {
        parsed <- parse_one(fixture)
    }
    index <- parsed$glue_templates
    c(allocated_bytes = allocated, snapshot_bytes = length(serialize(as.list(parsed), NULL)),
        index_bytes = if (length(index)) length(serialize(index, NULL)) else 0,
        index_object_bytes = as.numeric(utils::object.size(index)))
}
memory <- do.call(rbind, lapply(sources, storage))
result <- data.frame(label = label, cases, rounds = rounds, median_ms = apply(times, 1L, stats::median), memory)
utils::write.csv(result, stdout(), row.names = FALSE)
