# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/roxygen-class-fields.R [samples]
args <- commandArgs(trailingOnly = TRUE)
samples <- if (length(args)) as.integer(args[[1L]]) else 5L
stopifnot(length(args) <= 1L, !is.na(samples), samples >= 1L)
ns <- asNamespace("languageserver")
has_class_tags <- exists("roxygen_class_block", ns, inherits = FALSE)
if (!has_class_tags) cat("This installation does not index class member tags.\n")

milliseconds <- function(fn) {
    batch <- function(n) {
        start <- Sys.time()
        for (i in seq_len(n)) fn()
        as.numeric(difftime(Sys.time(), start, units = "secs"))
    }
    fn()
    probe <- batch(4L) / 4L
    n <- min(500L, max(1L, ceiling(0.025 / max(probe, 1e-6))))
    1000 * median(replicate(samples, batch(n))) / n
}

cat("fields  documentation       hint index (ms)\n")
for (n in c(32L, 128L, 256L)) {
    members <- paste0("f", seq_len(n))
    fields <- paste0("  ", members, " = NULL", ifelse(seq_len(n) == n, "", ","))
    for (kind in c("bare", "prose", "inherited prose", "typed")) {
        rows <- fields
        if (kind != "bare") {
            tag <- if (kind == "typed") "(`Receiver`) A field." else "A field."
            rows <- as.vector(rbind(paste("  #' @field", members, tag), fields))
        }
        parent <- if (kind == "inherited prose") "inherit = Parent, " else ""
        lines <- c(paste0('Box <- R6::R6Class("Box", ', parent, "public = list("), rows, "))")
        syntax <- ns$syntax_parse_data(utils::getParseData(parse(text = lines, keep.source = TRUE)))
        index <- function() ns$member_roxygen_type_index(lines, syntax, uri = "file:///wide-class.R")
        hints <- index()
        if (has_class_tags && kind %in% c("inherited prose", "typed")) {
            records <- attr(hints, "classes", exact = TRUE)
            stopifnot(length(records) == 1L)
            record <- records[[1L]]$hints
            stopifnot(identical(attr(record, "roxygen_documented_fields", exact = TRUE), members))
            if (kind == "typed") {
                sites <- attr(hints, "occurrences", exact = TRUE)
                stopifnot(identical(names(record$field), members), all(lengths(record$field) == 1L),
                    all(unlist(record$field, use.names = FALSE) == "Receiver"),
                    identical(sites$parameter, members), all(sites$type == "Receiver"),
                    identical(substring(lines[sites$row + 1L], sites$start + 1L, sites$end), sites$type))
            }
        } else {
            stopifnot(is.null(hints))
        }
        cat(sprintf("%6d  %-17s %10.3f\n", n, kind, milliseconds(index)))
    }
}
