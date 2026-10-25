coverage_shards <- function(files, timings, count) {
    contexts <- sub("\\.[Rr]$", "", sub("^test[-_]", "", files))
    stopifnot(length(count) == 1L, is.finite(count), count >= 1L,
        count == as.integer(count), length(files) >= count,
        !anyDuplicated(files), !anyDuplicated(contexts),
        all(c("file", "seconds") %in% names(timings)),
        !anyDuplicated(timings$file), is.numeric(timings$seconds),
        all(is.finite(timings$seconds)), all(timings$seconds >= 0))

    files <- sort(files, method = "radix")
    seconds <- timings$seconds[match(files, timings$file)]
    known <- seconds[!is.na(seconds)]
    # New test files must join a shard even before they have a recorded timing.
    seconds[is.na(seconds)] <- if (length(known)) median(known) else 1
    seconds <- pmax(seconds, 1)
    shards <- integer(length(files))
    loads <- numeric(count)
    # Assign the longest files first, breaking ties by filename and shard ID.
    for (index in order(-seconds, files, method = "radix")) {
        shard <- which.min(loads)
        shards[index] <- shard
        loads[shard] <- loads[shard] + seconds[index]
    }
    data.frame(file = files, seconds = seconds, shard = shards)
}

coverage_filter <- function(files) {
    names <- sub("\\.[Rr]$", "", sub("^test[-_]", "", files))
    # Escape every regex metacharacter so similarly named files cannot overlap.
    escaped <- vapply(strsplit(names, "", fixed = TRUE), function(chars) {
        special <- chars %in% c("\\", ".", "^", "$", "|", "(", ")", "[", "]",
            "{", "}", "*", "+", "?")
        chars[special] <- paste0("\\", chars[special])
        paste0(chars, collapse = "")
    }, character(1))
    paste0("^(", paste(escaped, collapse = "|"), ")$")
}

if (sys.nframe() == 0L) {
    args <- commandArgs(trailingOnly = TRUE)
    count <- as.integer(args[1])
    selected <- as.integer(args[2])
    stopifnot(length(args) == 2L, !is.na(selected), selected >= 1L,
        selected <= count)
    files <- list.files("tests/testthat", pattern = "^test.*\\.[Rr]$")
    timings <- read.csv(".github/coverage-timings.csv")
    plan <- coverage_shards(files, timings, count)
    print(aggregate(seconds ~ shard, plan, sum), row.names = FALSE)
    shard_files <- plan$file[plan$shard == selected]
    cat("Shard", selected, "tests:\n", paste(shard_files, collapse = "\n"), "\n")
    filter <- coverage_filter(shard_files)
    env_file <- Sys.getenv("GITHUB_ENV", "")
    if (nzchar(env_file)) {
        cat(paste0("TESTTHAT_FILTER=", filter, "\n"), file = env_file, append = TRUE)
    } else {
        cat("TESTTHAT_FILTER=", filter, "\n", sep = "")
    }
}
