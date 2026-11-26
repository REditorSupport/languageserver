# Index the package files that actually consume a topic. Updates, removals and
# closed-buffer refreshes replace the previous set for just this URI.
roxygen_update_uri_membership <- function(store, uri, old, new) {
    if (!length(old) && !length(new)) return(invisible(NULL))
    for (name in setdiff(old, new)) {
        rest <- setdiff(store$get(name, character()), uri)
        if (length(rest)) store$set(name, rest) else store$remove(name)
    }
    for (name in setdiff(new, old)) store$set(name, c(store$get(name, character()), uri))
    invisible(NULL)
}

# Sparse, source-backed edges. The physical range covers just the terminal
# topic name, so package qualifications, selectors and multiline layout survive.
roxygen_topic_uses <- function(hints, content, partial = FALSE, verified = NULL) {
    uses <- list()
    if (isTRUE(attr(hints, "has_inheritance", exact = TRUE))) {
        for (record in attr(hints, "topics", exact = TRUE)) {
            for (i in seq_along(record$inherit)) {
                edge <- record$inherit[[i]]
                if (!is.null(record$inherit_ranges)) edge$range <- record$inherit_ranges[i, ]
                edge$needed <- setdiff(record$formals, record$documented)
                name <- edge$topic
                uses[[name]] <- c(uses[[name]], list(edge))
            }
        }
    }
    if (!partial || !roxygen_has_inheritance(content)) return(uses)
    # The lexer can keep the documentation while dropping an unfinished
    # declaration. An unclaimed matching use prevents a partial workspace edit.
    lines <- if (is.null(verified)) grep("^\\s*#'", content) else sort(unique(verified))
    tags <- lines[grepl("^\\s*#'\\s*@inheritParams(?:\\s|$)", content[lines], perl = TRUE)]
    for (line in tags) {
        value <- sub("^\\s*#'\\s*@inheritParams\\s*", "", content[[line]], perl = TRUE)
        while (!nzchar(trimws(value)) && line < length(content)) {
            line <- line + 1L
            if (!nzchar(trimws(content[[line]]))) next
            if (!line %in% lines) {
                if (grepl("^\\s*#(?!')", content[[line]], perl = TRUE)) next
                break
            }
            value <- sub("^\\s*#'\\s*", "", content[[line]], perl = TRUE)
        }
        topic <- sub("[[:space:]].*$", "", trimws(value))
        if (!nzchar(topic) || !grepl(r"((*UCP)^([[:alpha:]][[:alnum:].]*::)?[[:alnum:]_.-]+$)", topic, perl = TRUE)) next
        found <- any(vapply(uses[[topic]], function(edge) length(edge$range) && edge$range[[1L]] == line - 1L, logical(1L)))
        if (!found) uses[[topic]] <- c(uses[[topic]], list(list(topic = topic, uncertain = TRUE)))
    }
    uses
}

# A topic name is a documentation identity, independent of lexical aliases and
# source() execution order. Standalone scripts share topics only within a file.
roxygen_topic_rename <- function(workspace, document, old, new, formal = NULL, owner_uri = document$uri) {
    if (identical(old, new)) return(NULL)
    if (is.null(document)) {
        index <- workspace$index
        if (!length(old) || !length(owner_uri) || !isTRUE(index$enabled)) return(NULL)
        key <- index_canonical_uri(owner_uri)
        summary <- index$summaries$get(key, NULL)
        donor <- any(vapply(summary$roxygen_topics[[old]], function(t) {
            identical(t$topic, old) && !isTRUE(t$fixed_name) && !is.null(t$formals)
        }, logical(1L)))
        if (!donor) return(NULL)
        names <- c(old, new)
        root <- summary$package_root
        if (!is.null(root)) {
            pkg <- tryCatch(read.dcf(file.path(root, "DESCRIPTION"), fields = "Package")[[1L]], error = function(e) NULL)
            names <- c(names, if (!is.null(pkg)) paste(pkg, names, sep = "::"))
        }
        consumers <- unique(unlist(lapply(names, index$roxygen_inherit_uris), use.names = FALSE))
        in_scope <- function(uri) identical(uri, key) || (!is.null(root) && identical(index$package_root_for_uri(uri), root))
        if (any(vapply(consumers, in_scope, logical(1L))) || index$package_topics_pending(root)) {
            return(list(problem = "A current donor declaration is needed to verify @inheritParams before renaming"))
        }
        return(NULL)
    }
    parsed <- document$parse_data
    data <- parsed$member_data
    records <- attr(data$type_hints, "topics", exact = TRUE)
    if (!length(records)) return(NULL)
    if (is.null(formal)) {
        local <- roxygen_document_topics(document)[[old]]
        affected <- Filter(function(t) identical(t$topic, old) && !isTRUE(t$fixed_name) && !is.null(t$formals), local)
        if (!length(affected)) return(NULL)
        record <- affected[[1L]]
    } else {
        row <- member_navigation_token(parsed, document$from_lsp_position(formal$range$start))
        s <- parsed$syntax_data
        if (!row || s$token[[row]] != "SYMBOL_FORMALS") return(NULL)
        old <- reference_symbol_names(s$text[[row]])
        if (identical(old, new)) return(NULL)
        owner <- s$parent_row[[row]]
        record <- records[[paste(s$line1[[owner]], s$col1[[owner]], sep = ":")]]
        if (is.null(record)) return(NULL)
    }
    idx <- workspace$index
    pkg <- data$document_package
    root <- if (isTRUE(idx$enabled)) idx$package_root_for_uri(document$uri)
    failure <- function(msg) list(problem = msg)
    if (!is.null(root) && idx$package_topics_pending(root)) return(failure("Package documentation is still being indexed"))
    if (is.null(root) && !is.null(pkg) && !isTRUE(idx$enabled)) {
        # Index-off package workspaces retain their R sources. A missing peer
        # could contain a directive that an edit to this topic would invalidate.
        paths <- list.files(dirname(path_from_uri(document$uri)), pattern = "[.][Rr]$", full.names = TRUE)
        scanned <- vapply(paths, path_to_uri, "")
    } else {
        scanned <- character()
    }
    here <- index_canonical_uri(document$uri)
    open <- if (length(scanned) || !is.null(root)) workspace_s4_open_uris(workspace, document$uri, workspace_document_uris(workspace, document$uri))
    overlay <- if (!is.null(root)) {
        keys <- unique(names(open))
        keys[vapply(keys, function(uri) identical(idx$package_root_for_uri(uri), root), logical(1L))]
    }
    cache <- new.env(parent = emptyenv())
    snapshot <- function(uri) {
        if (exists(uri, cache, inherits = FALSE)) return(cache[[uri]])
        if (sum(names(open) == uri) > 1L) return(NULL)
        summary <- if (!is.null(root)) idx$summaries$get(uri, NULL)
        aliases <- unique(c(if (identical(uri, here)) document$uri, unname(open[which(names(open) == uri)]), summary$roxygen_uri, uri))
        doc <- NULL
        for (alias in aliases) {
            doc <- workspace$documents$get(alias, NULL)
            if (!is.null(doc)) break
        }
        content <- NULL
        if (!is.null(doc) && !isTRUE(doc$is_open) && !identical(uri, here) && !is.null(summary)) {
            # A retained package document may still have its discarded editor
            # contents while the close/reload is being processed.
            content <- tryCatch(read_text_lines(path_from_uri(uri)), error = function(e) NULL)
            if (is.null(content) || !identical(get_content_hash(content), summary$content_hash)) return(NULL)
            doc <- NULL
        }
        if (!is.null(doc)) {
            if (is.null(roxygen_hint_document(workspace, doc$uri, doc))) return(NULL)
            pd <- doc$parse_data
            uses <- roxygen_topic_uses(pd$member_data$type_hints, doc$content,
                isTRUE(pd$parse_error) || length(pd$partial_incomplete_lines) > 0L, as.integer(pd$member_data$hint_comments))
            value <- list(uri = doc$uri, topics = roxygen_document_topics(doc), uses = uses, content = doc$content)
        } else if (!is.null(summary) && (!isFALSE(summary$cacheable) || !is.null(content))) {
            if (is.null(content)) content <- tryCatch(read_text_lines(path_from_uri(uri)), error = function(e) NULL)
            if (is.null(content) || !identical(get_content_hash(content), summary$content_hash)) return(NULL)
            value <- list(uri = uri, topics = summary$roxygen_topics, uses = summary$inherit_uses, content = content)
        } else {
            return(NULL)
        }
        cache[[uri]] <- value
        value
    }
    peers <- function(names, consumers) {
        found <- character()
        if (!is.null(root)) {
            lookup <- if (consumers) idx$roxygen_inherit_uris else idx$roxygen_topic_uris
            found <- unique(unlist(lapply(names, lookup), use.names = FALSE))
            found <- found[vapply(found, function(uri) identical(idx$package_root_for_uri(uri), root), logical(1L))]
        }
        unique(c(here, overlay, scanned, found))
    }
    lookup <- function(name) {
        rows <- list()
        for (uri in peers(name, FALSE)) {
            file <- snapshot(uri)
            if (is.null(file)) return(list(NULL))
            rows <- c(rows, lapply(file$topics[[name]], function(x) {
                x$uri <- uri
                x
            }))
        }
        rows
    }
    resolve <- function(name) {
        part <- strsplit(name, "::", fixed = TRUE)[[1L]]
        if (length(part) == 2L && !identical(part[[1L]], pkg)) return(NULL)
        name <- utils::tail(part, 1L)
        matches <- lookup(name)
        if (any(vapply(matches, is.null, logical(1L)))) return(NULL)
        owners <- Filter(function(t) identical(t$topic, name), matches)
        if (length(owners) != 1L) return(NULL)
        owner <- owners[[1L]]
        if (!identical(name, owner$page)) matches <- lookup(owner$page)
        if (!any(vapply(matches, is.null, logical(1L))) &&
                sum(vapply(matches, function(t) identical(t$page, owner$page), logical(1L))) == 1L) owner
    }
    if (!is.null(formal) && length(record$inherit) && !old %in% record$documented) {
        before <- roxygen_inherited_groups(record, here, resolve)
        after <- record
        after$formals[after$formals == old] <- new
        if (old %in% unlist(lapply(before, `[[`, "names"), use.names = FALSE) ||
                new %in% unlist(lapply(roxygen_inherited_groups(after, here, resolve), `[[`, "names"), use.names = FALSE)) {
            return(failure("Renaming this parameter would change documentation inherited through @inheritParams"))
        }
    }
    if (is.null(record$topic)) return(NULL)
    names <- if (is.null(formal)) c(old, new) else record$topic
    names <- unique(c(names, if (!is.null(pkg)) paste(pkg, names, sep = "::")))
    result <- list()
    checked <- FALSE
    for (uri in peers(names, TRUE)) {
        file <- snapshot(uri)
        if (is.null(file)) return(failure("A current package source is needed to verify @inheritParams before renaming"))
        for (ref in intersect(names(file$uses), names)) {
            if (!checked) {
                owner <- resolve(record$topic)
                if (is.null(owner) || !identical(owner$uri, here)) return(failure("An @inheritParams donor has ambiguous source documentation"))
                if (is.null(formal)) {
                    conflict <- lookup(new)
                    changes_target <- function(t) {
                        is.null(t) || identical(t$topic, new) || (!isTRUE(record$fixed_page) && identical(t$page, new))
                    }
                    if (any(vapply(conflict, changes_target, logical(1L)))) {
                        return(failure("The new name conflicts with another documentation topic or page"))
                    }
                    if (!grepl(roxygen_topic_name_pattern, new, perl = TRUE)) return(failure("The new name cannot be written in @inheritParams"))
                }
                checked <- TRUE
            }
            if (is.null(formal) && !identical(sub("^.*::", "", ref), old)) {
                return(failure("The new name would capture an existing @inheritParams topic reference"))
            }
            for (edge in file$uses[[ref]]) {
                span <- edge$range
                if (!length(span)) return(failure("Incomplete R code prevents verifying an @inheritParams directive"))
                if (!is.null(formal)) {
                    spelling <- c(old, new, sub("^[.]", "", c(old, new)), paste0(".", c(old, new)))
                    terms <- strsplit(edge$selector, "[^[:alnum:]_.]+", perl = TRUE)[[1L]]
                    if (any(c(old, new) %in% terms)) return(failure("Renaming this parameter would invalidate an @inheritParams selector"))
                    if (!any(edge$needed %in% spelling) || !old %in% record$documented) next
                    available <- unlist(record$groups, use.names = FALSE)
                    selected <- roxygen_selected_params(available, edge$selector)
                    if (any(vapply(record$groups, function(group) old %in% group && any(group %in% selected), logical(1L)))) {
                        return(failure("Renaming this parameter would change @inheritParams documentation"))
                    }
                    next
                }
                line <- file$content[[span[[1L]] + 1L]]
                if (!identical(substr(line, span[[2L]] + 1L, span[[3L]]), old)) return(failure("The source range of an @inheritParams directive has changed"))
                loc <- location(file$uri, range(position(span[[1L]], code_point_to_unit(line, span[[2L]])),
                        position(span[[1L]], code_point_to_unit(line, span[[3L]]))))
                attr(loc, "roxygen_label") <- new
                result[[length(result) + 1L]] <- loc
            }
        }
    }
    list(locations = result)
}
