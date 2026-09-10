# Run against an installed package, selecting its library with R_LIBS:
# Rscript inst/benchmarks/providers.R results.csv 1000
# The second argument controls the number of four-line functions.
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else "provider-performance.csv"
blocks <- if (length(args) > 1L) as.integer(args[[2L]]) else 1000L
stopifnot(!is.na(blocks), blocks >= 10L)

benchmark <- new.env(parent = asNamespace("languageserver"))
benchmark$output <- output
benchmark$blocks <- blocks
evalq(
    {
        lsp_settings$set("index_persistent_cache", FALSE)
        lsp_settings$set("diagnostics", FALSE)
        content <- unlist(lapply(seq_len(blocks), function(i) {
            c(
                sprintf("function_%d <- function(input, other = 2) {", i),
                "    value <- sum(input, other)",
                "    value + input",
                "}"
            )
        }), use.names = FALSE)
        uri <- "file:///performance-fixture.R"
        document <- Document$new(uri, language = "r", version = 1L, content = content)
        workspace <- Workspace$new(NULL)
        workspace$documents$set(uri, document)
        parsed <- parse_document(uri, content)
        parsed$version <- 1L
        workspace$update_parse_data(uri, parsed)
        viewport <- range(
            position(length(content) - 40L, 0L),
            position(length(content), 0L)
        )
        point <- list(row = length(content) - 2L, col = 7L)
        results <- new.env(parent = emptyenv())
        results$rows <- list()
        measure <- function(name, run, iterations = 20L, samples = 5L) {
            invisible(run())
            # Calibrate cheap operations above the timer resolution. Warm
            # caches remain intentional and are labelled in the workload.
            repeat {
                start <- proc.time()[[3L]]
                for (iteration in seq_len(iterations)) invisible(run())
                if (proc.time()[[3L]] - start >= 0.02 || iterations >= 10000L) break
                iterations <- iterations * 2L
            }
            times <- numeric(samples)
            for (sample in seq_len(samples)) {
                gc()
                start <- proc.time()[[3L]]
                for (iteration in seq_len(iterations)) invisible(run())
                times[[sample]] <- (proc.time()[[3L]] - start) * 1000 / iterations
            }
            row <- data.frame(
                operation = name, lines = length(content),
                median_ms = median(times), min_ms = min(times), max_ms = max(times),
                iterations = iterations, samples = samples
            )
            results$rows[[length(results$rows) + 1L]] <- row
            cat(sprintf("%-34s %10.3f ms\n", name, row$median_ms))
        }
        measure("parse_document", function() parse_document(uri, content), 1L, 3L)
        measure("shallow_workspace_index", function() {
            index_shallow_summary(
                file.path(tempdir(), "performance-fixture.R"), content, tempdir()
            )
        }, 5L)
        measure("token_lookup_near_end", function() {
            xdoc_find_token(
                document$parse_data$xml_doc, point$row + 1L, point$col + 1L
            )
        }, 100L)
        measure("document_highlight", function() {
            document_highlight_reply(
                1L, uri, workspace, document, point
            )
        })
        measure("definition", function() {
            definition_reply(1L, uri, workspace, document, point)
        })
        measure("hover_warm", function() {
            hover_reply(1L, uri, workspace, document, point)
        })
        measure("signature_help_warm", function() {
            signature_reply(
                1L, uri, workspace, document,
                list(row = length(content) - 3L, col = 23L)
            )
        })
        measure("document_symbols_warm", function() {
            document_symbol_reply(
                1L, uri, workspace, document, list(hierarchicalDocumentSymbolSupport = TRUE)
            )
        }, 3L)
        measure("folding_ranges_warm", function() {
            document_folding_range_reply(
                1L, uri, workspace, document
            )
        }, 5L)
        measure("inline_values_viewport", function() {
            inline_value_reply(
                1L, uri, workspace, document, viewport
            )
        })
        measure("inlay_hints_viewport", function() {
            inlay_hint_reply(
                1L, uri, workspace, document, viewport
            )
        })
        measure("completion", function() {
            completion_reply(
                1L, uri, workspace, document, list(row = length(content) - 2L, col = 7L),
                list(completionItem = list(snippetSupport = FALSE))
            )
        })
        defaults <- paste0("arg", seq_len(200L),
            " = c('value', 'alternative')", collapse = ", ")
        sig <- paste0("long_function(", defaults, ")")
        measure("signature_parameters_warm", function() parse_signature_parameters(sig))
        call_text <- paste0("f(", paste(rep("g(1, 2)", 500L), collapse = ", "), ", ")
        measure("active_argument_500_arguments", function() {
            detect_active_parameter(
                call_text, 0L, 1L, 0L, nchar(call_text)
            )
        })
        previous <- rep.int(c(0L, 1L, 3L, 2L, 0L), 50000L)
        current <- previous
        current[[length(current) %/% 2L]] <- 4L
        measure("semantic_delta_50000_tokens", function() semantic_token_delta(previous, current))
        measure("package_completion", function() package_completion("a"))
        namespace <- PackageNamespace$new("stats")
        measure("package_signature_warm", function() namespace$get_signature("lm"))
        documents <- collections::dict()
        for (i in seq_len(500L)) {
            doc <- new.env(parent = emptyenv())
            doc$uri <- paste0("file:///workspace-file", i, ".R")
            symbols <- paste0("function", i, "_", seq_len(100L))
            doc$parse_data <- list(
                functs = symbols, nonfuncts = character(),
                objects = symbols
            )
            documents$set(doc$uri, doc)
        }
        global <- GlobalEnv$new(documents)
        measure("workspace_symbols_50000_warm", function() global$get_symbols())
        write.csv(do.call(rbind, results$rows), output, row.names = FALSE)
        cat(sprintf(
            "R %s; %s; languageserver %s\n", getRversion(), R.version$platform,
            as.character(utils::packageVersion("languageserver"))
        ))
    },
    benchmark
)
