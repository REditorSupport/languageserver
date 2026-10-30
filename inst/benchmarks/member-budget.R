# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/member-budget.R results.csv 3
# Compare time policies using identical prepared metadata and fresh caches.
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else "member-budget-performance.csv"
passes <- if (length(args) > 1L) as.integer(args[[2L]]) else 3L
stopifnot(!is.na(passes), passes > 0L)
Sys.setenv(TESTTHAT = "", R_COVR = "", R_PROFILE_USER = "/dev/null", R_ENVIRON_USER = "/dev/null")
benchmark <- new.env(parent = asNamespace("languageserver"))
benchmark$output <- output
benchmark$passes <- passes
evalq({
    run <- function() {
        previous <- options(languageserver.package_cache_dir = FALSE)
        on.exit(options(previous), add = TRUE)
        snapshot <- member_prepare_package("polars")
        ns <- asNamespace("languageserver")
        original_budget <- member_request_budget
        original_infer <- member_infer
        replace <- function(name, value) {
            unlockBinding(name, ns)
            assign(name, value, ns)
            lockBinding(name, ns)
        }
        on.exit({
            replace("member_request_budget", original_budget)
            replace("member_infer", original_infer)
        }, add = TRUE)
        policy <- "adaptive"
        stall <- 0
        clock <- function() {
            if (stall > 0) Sys.sleep(stall)
            base::proc.time()
        }
        infer <- original_infer
        environment(infer) <- list2env(list(proc.time = clock), parent = environment(infer))
        replace("member_infer", infer)
        replace("member_request_budget", function() {
            budget <- original_budget()
            if (policy == "fixed_1s") {
                budget$deadline <- budget$started + 1
                budget$scheduled_deadline <- NULL
            }
            budget
        })
        chain <- function(stages) {
            c("library(polars)", "item <- as_polars_df(opaque_source())",
                rep("item <- item$sort('value')$unique('value')", stages), "item$")
        }
        workloads <- list(
            ordinary = list(lines = c("item <- list(value = 1)", "item$"), stall = 0),
            long_chain = list(lines = chain(12L), stall = 0),
            contended_chain = list(lines = chain(12L), stall = 0.025),
            wall_bound = list(lines = chain(12L), stall = 0.15),
            over_budget_chain = list(lines = chain(30L), stall = 0),
            wide_conversions = list(lines = c("library(polars)",
                    paste0("item <- list(", paste(rep("as_polars_df(opaque_source())", 200L), collapse = ","), ")"), "item$"), stall = 0),
            node_bound = list(lines = c(paste0("item <- list(", paste(rep("1", 25000L), collapse = ","), ")"), "item$"), stall = 0),
            recursive = list(lines = c("factory <- function(x) factory(x)", "item <- factory(opaque_source())", "item$"), stall = 0)
        )
        rows <- list()
        for (workload in names(workloads)) {
            lines <- workloads[[workload]]$lines
            parsed <- parse_document("file:///budget-benchmark.R", lines)
            for (pass in seq_len(passes)) {
                # Alternate order to reduce systematic first-run/JIT bias.
                for (selected in if (pass %% 2L) c("fixed_1s", "adaptive") else c("adaptive", "fixed_1s")) {
                    policy <- selected
                    stall <- workloads[[workload]]$stall
                    metadata <- MemberMetadataCache$new(32 * 1024^2)
                    metadata$set("polars", snapshot)
                    document <- Document$new("file:///budget-benchmark.R", language = "r", version = 1L, content = lines)
                    data <- parsed
                    data$version <- 1L
                    document$update_parse_data(data)
                    workspace <- list(member_metadata = metadata)
                    point <- list(row = length(lines) - 1L, col = nchar(tail(lines, 1L)))
                    cursor <- member_cursor(document, point)
                    for (temperature in c("cold", "repeat", "warm")) {
                        gc()
                        start <- proc.time()
                        value <- member_resolve_cursor(document$uri, workspace, document, point, cursor)
                        elapsed <- proc.time() - start
                        stopifnot(!is.null(value))
                        rows[[length(rows) + 1L]] <- data.frame(workload = workload, policy = policy, pass = pass,
                            temperature = temperature, elapsed_ms = 1000 * elapsed[[3L]], cpu_ms = 1000 * sum(elapsed[1:2]),
                            nodes = 20000L - value$budget$remaining, exhausted = isTRUE(value$budget$exhausted),
                            timed_out = isTRUE(value$budget$timed_out), reason = if (is.null(value$value$reason)) "" else value$value$reason,
                            receivers = document$member_receivers$size(), bindings = document$member_binding_cache$size())
                    }
                }
            }
        }
        results <- do.call(rbind, rows)
        utils::write.csv(results, output, row.names = FALSE)
        print(aggregate(cbind(elapsed_ms, cpu_ms, nodes) ~ workload + policy + temperature, results, stats::median), row.names = FALSE)
        print(aggregate(cbind(exhausted, timed_out) ~ workload + policy + temperature, results, sum), row.names = FALSE)
        stopifnot(all(results$nodes <= 20001L))
        cat("Metadata preparation/parsing/GC are excluded. Stalled cases inject 25 or 150 ms at each inference clock checkpoint.\n")
    }
    run()
}, benchmark)
