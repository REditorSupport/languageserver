# Find References and Rename in an already-parsed, multi-file R workspace.
# Use the same script against separate installed revisions, in serial:
# R_LIBS=/tmp/head/lib Rscript --vanilla inst/benchmarks/member-navigation.R head.csv 32 16 3 head owner
# R_LIBS=/tmp/base/lib Rscript --vanilla inst/benchmarks/member-navigation.R base.csv 32 16 3 base legacy
#
# Arguments: CSV, consumer files, reads per owner per file, samples, revision
# label, expected behavior (owner or legacy), optional case (all, lexical,
# simple, chained, method). The fixture also has one shared declarations file. Every
# consumer sources it and interleaves a shared member read, a same-named read
# on its own object, and a same-named lexical read. The chained case uses
# $child$value in place of $value. The lexical control uses the simple fixture.
# Select method explicitly to benchmark calls to function-valued fields, with a
# configurable hook between pairs of calls that could change receiver bindings.
# Each unrelated owner also has the rename destination already. The exact-range
# checks require it to be excluded without blocking the target rename.
# The historical all run includes lexical, simple, and chained only.
#
# Each cold request gets a fresh Workspace and fresh Documents with accepted
# worker parse results. Its warm counterpart repeats that request on the same
# state. Setup, GC before each request, validation and protocol transport are
# outside the timer. Rename edits are inspected without applying them.
# The pre-PR provider returns no member occurrences. Its empty-response timings
# measure that behavior, not the cost of resolving the intended references.

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || identical(args[[1L]], "--help")) {
    cat("Usage: Rscript --vanilla member-navigation.R OUTPUT.csv [FILES=16] [READS=12] [SAMPLES=3]",
        "[LABEL=installed] [owner|legacy] [all|lexical|simple|chained|method]\n")
    quit(status = if (length(args)) 0L else 1L)
}
if (length(args) > 7L) stop("Too many arguments")
arg <- function(i, default) if (length(args) >= i) args[[i]] else default
positive_integer <- function(i, default) {
    value <- arg(i, as.character(default))
    number <- suppressWarnings(as.integer(value))
    if (!grepl("^[0-9]+$", value) || is.na(number) || number < 1L) stop("Expected a positive integer: ", value)
    number
}
output <- args[[1L]]
files <- positive_integer(2L, 16L)
reads <- positive_integer(3L, 12L)
samples <- positive_integer(4L, 3L)
revision <- arg(5L, "installed")
expect <- match.arg(arg(6L, "owner"), c("owner", "legacy"))
selected_case <- match.arg(arg(7L, "all"), c("all", "lexical", "simple", "chained", "method"))
ns <- asNamespace("languageserver")

location_key <- function(uri, line, col, end_line = line, end_col = col + 5L) {
    paste(uri, line, col, end_line, end_col, sep = "|")
}

make_fixture <- function(root, shape) {
    dir.create(root)
    object <- switch(shape, simple = "list(value = %dL)", chained = "list(child = list(value = %dL))",
        method = "list(value = function(x) x + %dL)")
    field <- if (shape == "chained") "$child$value" else "$value"
    suffix <- if (shape == "method") "(1L)" else ""
    hook <- if (shape == "method") 'getOption("navigation.refresh", function() NULL)()'
    other_object <- if (shape == "method") "list(value = function(x) x + %dL, renamed_value = 0L)" else object
    filenames <- c("declarations.R", sprintf("consumer-%04d.R", seq_len(files)))
    contents <- vector("list", length(filenames))
    contents[[1L]] <- c("value <- 7L", paste("shared <-", sprintf(object, 1L)))
    local_names <- sprintf("other_%04d", seq_len(files))
    for (i in seq_len(files)) {
        accesses <- c(paste0("shared", field, suffix), paste0(local_names[[i]], field, suffix), "value", hook)
        contents[[i + 1L]] <- c('source("declarations.R")', paste(local_names[[i]], "<-", sprintf(other_object, i)),
            rep(accesses, reads))
    }
    paths <- file.path(root, filenames)
    uris <- vapply(paths, ns$path_to_uri, "", USE.NAMES = FALSE)
    # The package's worker returns serialized parse data. Snapshot once before
    # any provider sees it, and restore pristine copies for each cold request.
    snapshots <- lapply(seq_along(paths), function(i) {
        writeLines(contents[[i]], paths[[i]], useBytes = TRUE)
        parsed <- ns$parse_document(uris[[i]], contents[[i]])
        stopifnot(!isTRUE(parsed$parse_error))
        parsed$version <- 1L
        serialize(parsed, NULL)
    })

    # Expected UTF-16 ranges come directly from this ASCII fixture. Check the
    # declaration plus every target, so a full-sized result from the wrong
    # owner, duplicate occurrences, or an off-by-one edit fails validation.
    target_line <- paste0("shared", field)
    target_col <- nchar(target_line) - nchar("value")
    declaration_col <- regexpr("value", contents[[1L]][[2L]], fixed = TRUE)[[1L]] - 1L
    member_rows <- 2L + (3L + length(hook)) * (seq_len(reads) - 1L)
    consumer_uris <- rep(uris[-1L], each = reads)
    member_keys <- c(location_key(uris[[1L]], 1L, declaration_col),
        location_key(consumer_uris, rep(member_rows, files), target_col))
    lexical_keys <- c(location_key(uris[[1L]], 0L, 0L),
        location_key(consumer_uris, rep(member_rows + 2L, files), 0L))
    list(root = root, uris = uris, contents = contents, snapshots = snapshots,
        member = list(point = list(row = 2L, col = target_col + 1L), keys = sort(member_keys)),
        lexical = list(point = list(row = 4L, col = 1L), keys = sort(lexical_keys)))
}

fresh_workspace <- function(fixture) {
    workspace <- ns$Workspace$new(fixture$root)
    for (i in seq_along(fixture$uris)) {
        uri <- fixture$uris[[i]]
        document <- ns$Document$new(uri, language = "r", version = 1L, content = fixture$contents[[i]])
        document$is_open <- TRUE
        workspace$documents$set(uri, document)
        workspace$update_parse_data(uri, unserialize(fixture$snapshots[[i]]))
    }
    stopifnot(isTRUE(workspace$index$enabled), workspace$documents$size() == length(fixture$uris))
    workspace
}

response_keys <- function(reply, provider) {
    if (!is.null(reply$error)) stop("Provider returned an error: ", reply$error$message)
    key <- function(uri, r) location_key(uri, r$start$line, r$start$character, r$end$line, r$end$character)
    if (provider == "references") {
        return(sort(vapply(reply$result, function(x) key(x$uri, x$range), "", USE.NAMES = FALSE)))
    }
    changes <- reply$result$changes
    if (!length(changes)) return(character())
    keys <- lapply(names(changes), function(uri) {
        vapply(changes[[uri]], function(edit) {
            if (!identical(edit$newText, "renamed_value")) stop("Wrong replacement text: ", edit$newText)
            key(uri, edit$range)
        }, "", USE.NAMES = FALSE)
    })
    sort(unlist(keys, use.names = FALSE))
}

measure <- function(fixture, scenario, provider, sample) {
    workspace <- fresh_workspace(fixture)
    uri <- fixture$uris[[2L]]
    document <- workspace$documents$get(uri)
    target <- if (scenario == "lexical") fixture$lexical else fixture$member
    request <- if (provider == "references") {
        function() ns$references_reply(1L, uri, workspace, document, target$point, include_declaration = TRUE)
    } else {
        function() ns$rename_reply(1L, uri, workspace, document, target$point, "renamed_value")
    }
    rows <- lapply(c("cold", "warm"), function(cache) {
        gc()
        cpu <- proc.time()
        start <- Sys.time()
        reply <- request()
        elapsed_ms <- as.numeric(difftime(Sys.time(), start, units = "secs")) * 1000
        cpu <- proc.time() - cpu
        actual <- response_keys(reply, provider)
        correct <- identical(actual, target$keys)
        should_match <- expect == "owner" || scenario == "lexical"
        if ((should_match && !correct) || (!should_match && length(actual))) {
            message <- sprintf("%s %s %s sample %d: returned %d, missing %d, unexpected %d (expected behavior %s)",
                scenario, provider, cache, sample, length(actual), length(setdiff(target$keys, actual)),
                length(setdiff(actual, target$keys)), expect)
            stop(message)
        }
        data.frame(revision, scenario, provider, cache, sample, consumer_files = files,
            reads_per_owner_per_file = reads, member_read_sites = 2 * as.double(files) * reads,
            expected = length(target$keys), returned = length(actual), matches_expected = correct,
            outcome = if (correct) "exact" else "unsupported_empty",
            elapsed_ms, cpu_ms = 1000 * unname(cpu[["user.self"]] + cpu[["sys.self"]]),
            r_version = as.character(getRversion()), platform = R.version$platform,
            package_version = as.character(utils::packageVersion("languageserver")), library = find.package("languageserver"))
    })
    do.call(rbind, rows)
}

main <- function() {
    ns$lsp_settings$set("index_persistent_cache", FALSE)
    ns$lsp_settings$set("diagnostics", FALSE)
    ns$lsp_settings$set("member_completion", TRUE)
    root <- tempfile("member-navigation-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    cases <- if (selected_case == "all") c("lexical", "simple", "chained") else selected_case
    fixture <- NULL
    previous_shape <- NULL
    rows <- list()
    for (scenario in cases) {
        shape <- if (scenario == "lexical") "simple" else scenario
        if (!identical(shape, previous_shape)) {
            fixture <- NULL
            gc()
            fixture <- make_fixture(file.path(root, shape), shape)
            previous_shape <- shape
        }
        for (provider in c("references", "rename")) {
            for (sample in seq_len(samples)) {
                message(sprintf("%s %s, sample %d/%d", scenario, provider, sample, samples))
                rows[[length(rows) + 1L]] <- measure(fixture, scenario, provider, sample)
            }
        }
    }
    data <- do.call(rbind, rows)
    utils::write.csv(data, output, row.names = FALSE)
    print(stats::aggregate(elapsed_ms ~ scenario + provider + cache + outcome, data, stats::median), row.names = FALSE)
    cat("Raw samples:", output, "\n")
}
main()
