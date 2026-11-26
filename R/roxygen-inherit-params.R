# NULL documentation topics have physical type mentions but no function AST.
roxygen_declared_types <- function(hints) {
    if (is.null(hints)) return(character())
    unique(c(unlist(hints, use.names = FALSE), attr(hints, "occurrences", exact = TRUE)$type))
}

# Index both the documented name and its output page. A topic sharing a page
# with another block needs Rd-section merging and cannot be resolved in isolation.
roxygen_topic_map <- function(hints) {
    topics <- attr(hints, "topics", exact = TRUE)
    if (!length(topics)) return(NULL)
    topics <- Filter(function(x) !is.null(x$topic), topics)
    if (!length(topics)) return(NULL)
    names <- vapply(topics, `[[`, "", "topic")
    pages <- vapply(topics, `[[`, "", "page")
    other <- pages != names
    split(c(topics, topics[other]), c(names, pages[other]))
}

roxygen_hint_document <- function(workspace, uri, document = NULL) {
    if (is.null(uri)) return(NULL)
    doc <- if (!is.null(workspace$documents)) workspace$documents$get(uri, NULL)
    if (is.null(doc) && identical(document$uri, uri)) doc <- document
    if (is.null(doc) && !is.null(workspace$documents) && startsWith(uri, "file:")) {
        canonical <- index_canonical_uri(uri)
        if (!identical(canonical, uri)) doc <- workspace$documents$get(canonical, NULL)
    }
    if (!is.null(doc$parse_data) && identical(doc$version, doc$parse_data$version) &&
            identical(doc$content, doc$parsed_content)) doc
}

roxygen_document_topics <- function(document) {
    data <- document$parse_data
    if (is.null(data$roxygen_topic_map)) {
        map <- roxygen_topic_map(data$member_data$type_hints)
        if (!is.environment(data)) return(map)
        data$roxygen_topic_map <- if (is.null(map)) list() else map
    }
    data$roxygen_topic_map
}

# Literal roxygen selections, including exclusions and ranges. Never evaluate
# expressions supplied in documentation. Invalid selectors select no entries.
roxygen_selected_params <- function(args, selector) {
    if (!nzchar(selector)) return(args)
    args <- setdiff(args, "...")
    terms <- strsplit(selector, "[[:space:]]+")[[1L]]
    selected <- rep(startsWith(terms[[1L]], "-"), length(args))
    for (term in terms) {
        exclude <- startsWith(term, "-")
        if (exclude) {
            term <- substring(term, 2L)
            if (startsWith(term, "(") && endsWith(term, ")")) {
                term <- substr(term, 2L, nchar(term) - 1L)
            } else if (grepl(":", term, fixed = TRUE)) {
                return(character())
            }
        }
        if (!nzchar(term) || endsWith(term, ":")) return(character())
        ends <- strsplit(term, ":", fixed = TRUE)[[1L]]
        if (!all(grepl(paste0("(*UCP)^", roxygen_type_identifier, "$"), ends, perl = TRUE))) return(character())
        pos <- match(ends, args)
        if (length(pos) > 2L || anyNA(pos)) return(character())
        if (length(pos) == 2L) pos <- seq.int(pos[[1L]], pos[[2L]])
        selected[pos] <- !exclude
    }
    args[selected]
}

# Expand each donor once per lookup. Incomplete branches keep their verified
# documentation but cannot establish which later donor wins a missing parameter.
roxygen_inherited_groups <- function(record, uri, donor) {
    remaining <- 256L
    expanded <- new.env(parent = emptyenv())
    walk <- function(topic, path) {
        groups <- lapply(seq_along(topic$groups), function(i) {
            names <- topic$groups[[i]]
            types <- topic$group_types[[i]]
            origin <- if (length(types)) list(types = types, uri = topic$uri, row = topic$row, col = topic$col)
            list(names = names, origin = origin)
        })
        needed <- setdiff(topic$formals, topic$documented)
        if (!length(needed) || !length(topic$inherit)) return(list(groups = groups, complete = TRUE))
        sources <- vapply(topic$inherit, `[[`, "", "topic")
        for (ref in unique(sources)) {
            parent <- donor(ref)
            if (is.null(parent) || length(path) >= 32L) return(list(groups = groups, complete = FALSE))
            id <- paste(parent$uri, parent$row, parent$col, sep = "\n")
            if (id %in% path) return(list(groups = groups, complete = FALSE))
            branch <- expanded[[id]]
            if (is.null(branch)) {
                if (remaining <= 0L) return(list(groups = groups, complete = FALSE))
                remaining <<- remaining - 1L
                branch <- walk(parent, c(path, id))
                if (branch$complete) expanded[[id]] <- branch
            }
            inherited <- branch$groups
            selectors <- vapply(topic$inherit[sources == ref], `[[`, "", "selector")
            available <- unlist(lapply(inherited, `[[`, "names"), use.names = FALSE)
            selected <- available
            if (all(nzchar(selectors))) {
                selected <- unique(unlist(lapply(selectors, function(s) roxygen_selected_params(available, s)), use.names = FALSE))
            }
            for (group in inherited) {
                if (!any(group$names %in% selected)) next
                alternate <- ifelse(startsWith(needed, "."), substring(needed, 2L), paste0(".", needed))
                if (!all(group$names %in% c(needed, alternate))) next
                names <- union(needed[match(group$names, needed, nomatch = 0L)],
                    needed[match(group$names, alternate, nomatch = 0L)])
                groups[[length(groups) + 1L]] <- list(names = names, origin = group$origin)
                needed <- setdiff(needed, names)
                if (!length(needed)) return(list(groups = groups, complete = TRUE))
            }
            if (!branch$complete) return(list(groups = groups, complete = FALSE))
        }
        list(groups = groups, complete = TRUE)
    }
    record$uri <- uri
    walk(record, paste(uri, record$row, record$col, sep = "\n"))$groups
}

# Resolve documentation groups as a unit. Untyped prose also consumes a
# matching formal, so a later donor cannot silently supply a different type.
roxygen_effective_params <- function(hints, workspace, document, hint_wait = NULL) {
    direct <- hints$params
    if (!isTRUE(attr(hints, "roxygen_inherits", exact = TRUE))) return(direct)
    uri <- attr(hints, "roxygen_uri", exact = TRUE)
    if (is.null(uri)) uri <- document$uri
    owner <- roxygen_hint_document(workspace, uri, document)
    index <- workspace$index
    key <- attr(hints, "roxygen_key", exact = TRUE)
    if (is.null(uri) || is.null(key)) return(direct)
    data <- owner$parse_data$member_data
    record <- member_lookup(attr(data$type_hints, "topics", exact = TRUE), key)
    recovered <- is.null(record) && !is.null(attr(hints, "roxygen_record", exact = TRUE))
    if (recovered) record <- attr(hints, "roxygen_record", exact = TRUE)
    if (is.null(owner)) {
        if (!is.null(workspace$documents) && !is.null(workspace$documents$get(uri, NULL))) {
            if (!is.null(hint_wait)) s3_record_hint_wait(hint_wait, workspace, NULL, character(), uri = uri)
            return(direct)
        }
        if (!isTRUE(index$enabled)) return(direct)
        snapshot <- index$summaries$get(index_canonical_uri(uri), NULL)
        local <- snapshot$roxygen_topics
        if (is.null(record)) for (entries in local) {
            record <- entries[[key]]
            if (!is.null(record)) break
        }
    } else {
        local <- roxygen_document_topics(owner)
    }
    if (!length(record$inherit)) return(direct)

    root <- if (isTRUE(index$enabled)) index$package_root_for_uri(uri)
    versioned <- is.null(root) || (!is.null(index$revision) && !is.null(workspace$s3_revision))
    cache <- if (is.null(hint_wait) && !recovered && !is.null(owner) && versioned) {
        peers <- if (!is.null(root)) c(index$revision, workspace$s3_revision, workspace$documents$size())
        analysis_cache(owner, "inherited-params", list(owner$parse_generation, data$content_key, peers), max_bytes = 512L * 1024L)
    }
    hit <- analysis_cache_get(cache, key)
    if (hit$hit) return(hit$value)
    package <- data$document_package
    catalog <- new.env(parent = emptyenv())
    locate <- function(record, uri) {
        record$uri <- uri
        record
    }
    lookup <- function(name) {
        if (exists(name, catalog, inherits = FALSE)) return(catalog[[name]])
        here <- local[[name]]
        result <- lapply(here, locate, uri = uri)
        if (!is.null(root) && is.function(index$roxygen_topic_uris)) {
            for (peer in setdiff(index$roxygen_topic_uris(name), index_canonical_uri(uri))) {
                summary <- index$summaries$get(peer, NULL)
                if (!identical(summary$package_root, root)) next
                candidate_uri <- if (is.null(summary$roxygen_uri)) peer else summary$roxygen_uri
                doc <- workspace$documents$get(candidate_uri, NULL)
                if (is.null(doc) && !identical(candidate_uri, peer)) doc <- workspace$documents$get(peer, NULL)
                if (!is.null(doc)) {
                    candidate_uri <- doc$uri
                    if (is.null(roxygen_hint_document(workspace, candidate_uri))) {
                        # An open buffer being reparsed can remove or rename a
                        # donor. Its old disk summary cannot prove uniqueness.
                        if (!is.null(hint_wait)) s3_record_hint_wait(hint_wait, workspace, NULL, character(), uri = candidate_uri)
                        result <- NULL
                        break
                    }
                    matches <- roxygen_document_topics(doc)[[name]]
                } else {
                    candidate_uri <- peer
                    matches <- summary$roxygen_topics[[name]]
                }
                result <- c(result, lapply(matches, locate, uri = candidate_uri))
            }
        }
        assign(name, result, catalog)
        result
    }
    donor <- function(ref) {
        parts <- strsplit(ref, "::", fixed = TRUE)[[1L]]
        if (length(parts) == 2L) {
            if (is.null(root) && is.null(package)) return(NULL)
            if (is.null(package)) {
                package <<- tryCatch(as.character(read.dcf(file.path(root, "DESCRIPTION"), fields = "Package")[[1L]]),
                    error = function(e) "")
            }
            if (!identical(parts[[1L]], package)) return(NULL)
        }
        name <- utils::tail(parts, 1L)
        if (!is.null(hint_wait) && !is.null(root)) s3_record_topic_wait(hint_wait, workspace, root)
        matches <- lookup(name)
        owners <- Filter(function(x) identical(x$topic, name), matches)
        if (length(owners) != 1L) return(NULL)
        owner <- owners[[1L]]
        if (!identical(name, owner$page)) matches <- lookup(owner$page)
        if (sum(vapply(matches, function(x) identical(x$page, owner$page), logical(1L))) == 1L) owner
    }

    for (group in roxygen_inherited_groups(record, uri, donor)) {
        add <- setdiff(group$names, record$documented)
        if (length(add) && !is.null(group$origin)) direct[add] <- rep(list(group$origin), length(add))
    }
    direct <- direct[intersect(record$formals, names(direct))]
    analysis_cache_set(cache, key, direct)
    direct
}

# Named donors are top-level source topics. Resolve each type at the physical
# donor declaration, with that document's imports, class definitions and origins.
member_inherited_hint_value <- function(record, workspace, budget, index = NULL) {
    doc <- roxygen_hint_document(workspace, record$uri)
    if (!is.null(budget$hint_wait)) s3_record_hint_wait(budget$hint_wait, workspace, doc, record$types, uri = record$uri)
    if (is.null(doc)) return(member_value(reason = "unavailable_hint_donor"))
    # Several formals can share one documented class. Its declaration scope
    # is fixed for this request, including the member whose origin is wanted.
    key <- paste(record$uri, record$row, record$col, paste(record$types, collapse = "|"),
        budget$origin_name %s3_or% "", sep = "\r")
    memo <- budget$inherited_values
    if (is.null(memo)) budget$inherited_values <- memo <- new.env(parent = emptyenv())
    if (!is.null(memo[[key]])) return(memo[[key]])
    at <- c(record$row, record$col)
    if (is.null(index)) index <- member_context_index(workspace, doc$uri, doc, at, referenced = member_type_dependencies(record$types))
    if (!is.null(budget$origin_name)) {
        previous <- budget$document
        budget$document <- doc
        on.exit(budget$document <- previous)
    }
    value <- member_hinted_value(record$types, index, list(.__member_position__ = at), budget)
    if (!isTRUE(budget$exhausted) && !isTRUE(budget$transient) && !length(budget$resolving_hints) &&
            !identical(value$reason, "recursive_type_hint") && length(memo) < 32L) memo[[key]] <- value
    value
}
