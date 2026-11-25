# The parser identifies the callee token (including quoted names); passing its
# exact range to receiver recovery preserves both the original R call and uri.
class_navigation_symbol <- function(document, parsed, point) {
    s <- parsed$syntax_data
    if (is.null(s)) return(NULL)
    row <- member_navigation_token(parsed, point)
    if (!row || !s$token[[row]] %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL") || s$line1[[row]] != s$line2[[row]]) return(NULL)
    parent <- s$parent_row[[row]]
    if (!parent) return(NULL)
    head <- syntax_children(s, parent)
    qualified <- length(head) == 3L && head[[3L]] == row && s$token[[head[[2L]]]] %in% c("NS_GET", "NS_GET_INT")
    if (length(head) != 1L && !qualified) return(NULL)
    name <- reference_symbol_names(s$text[[row]])
    line <- s$line1[[row]] - 1L
    start <- s$col1[[row]] - 1L
    end <- s$col2[[row]]
    location <- list(point = list(row = line, col = end),
        range = list(start = list(row = line, col = start), end = list(row = line, col = end)),
        cursor = list(operator_row = line, operator = start + 1L, start = start, end = end, token = name,
            before = substr(document$line0(line), 1L, start)))
    if (s$token[[row]] != "SYMBOL_FUNCTION_CALL") return(list(location = location))
    call_row <- s$parent_row[[parent]]
    if (!call_row) return(list(location = location))
    kids <- syntax_children(s, call_row)
    kids <- kids[s$token[kids] != "COMMENT"]
    if (length(kids) < 2L || kids[[1L]] != parent || s$token[[kids[[2L]]]] != "'('") return(list(location = location))
    opening <- kids[[2L]]
    call <- list(token = name, full_token = name,
        package = if (qualified) reference_symbol_names(s$text[[head[[1L]]]]),
        accessor = if (qualified) s$text[[head[[2L]]]] else "",
        opening = list(row = s$line1[[opening]] - 1L, col = s$col1[[opening]] - 1L),
        callee = location$point, symbol_location = location)
    list(location = location, call = call)
}

class_navigation_declaration <- function(uri, workspace, document, point, alternatives = FALSE) {
    if (!isTRUE(lsp_settings$get("member_completion")) || !check_r_region(document, point)) return(NULL)
    parsed <- current_parse_data(uri, workspace, document)
    if (is.null(parsed)) return(NULL)
    location <- member_hover_location(document, point)
    if (is.null(location)) location <- class_navigation_symbol(document, parsed, point)$location
    if (is.null(location)) return(NULL)
    value <- member_resolve_cursor(uri, workspace, document, location$point, location$cursor,
        location$cursor$token, origins = TRUE)$value
    if (alternatives && identical(value$reason, "type_hint_union")) {
        return(unique(Filter(Negate(is.null), lapply(value$possible_returns, class_value_declaration, workspace = workspace))))
    }
    if (isTRUE(value$possible_receiver) || length(value$possible_returns)) return(NULL)
    cls <- class_value_declaration(value, workspace)
    if (alternatives && !is.null(cls)) list(cls) else cls
}

class_value_declaration <- function(value, workspace) {
    cls <- if (isTRUE(value$r6_role %in% c("instance", "generator")) && !is.null(value$r6_class$origin)) {
        list(type = "R6", name = value$r6_class$name, origin = value$r6_class$origin)
    } else if (!is.null(value$s4_class$origin)) {
        list(type = "S4", name = value$s4_class$name, origin = value$s4_class$origin)
    } else if (!is.null(value$s4_generator$recipe$class_origin)) {
        list(type = "S4", name = value$s4_generator$name, origin = value$s4_generator$recipe$class_origin)
    } else if (!is.null(value$refclass_origin)) {
        list(type = "RefClass", name = value$type, origin = value$refclass_origin)
    } else {
        descriptor <- if (!is.null(value$s7_generator)) value$s7_generator else value$s7_descriptor
        if (identical(descriptor$kind, "class") && !is.null(descriptor$origin)) {
            list(type = "S7", name = descriptor$name, origin = descriptor$origin)
        }
    }
    if (is.null(cls)) return(NULL)
    # The constructor call's physical source range distinguishes classes that
    # have the same display name, even if both are assigned in the same file.
    source <- workspace$documents$get(cls$origin$uri, NULL)
    loc <- cls$origin$range
    if (is.null(source) || length(loc) != 3L ||
            !identical(source$version, source$parse_data$version)) return(NULL)
    at <- source$to_lsp_position(loc[[1L]], loc[[2L]])
    candidates <- Filter(function(x) {
        identical(x$type, cls$type) && identical(x$class_name, cls$name) && lsp_range_contains(x$range, at)
    }, source$parse_data$class_data$declarations)
    if (length(candidates) == 1L) candidates[[1L]]
}

type_definition_reply <- function(id, uri, workspace, document, point) {
    classes <- class_navigation_declaration(uri, workspace, document, point, alternatives = TRUE)
    locations <- lapply(classes, function(cls) location(cls$uri, cls$selectionRange))
    Response$new(id, result = if (length(locations) == 1L) locations[[1L]] else if (length(locations)) locations)
}

s3_implementation_target <- function(method, workspace, store) {
    target <- method$source$implementation
    if (!is.null(target)) {
        source <- workspace$documents$get(target$uri, NULL)
        if (!is.null(source)) {
            if (!identical(source$version, source$parse_data$version)) return(NULL)
            target$range <- range(source$to_lsp_position(target$start[[1L]], target$start[[2L]]),
                source$to_lsp_position(target$end[[1L]], target$end[[2L]]))
        } else if (length(target$columns) == 2L) {
            target$range <- range(position(target$start[[1L]], target$columns[[1L]]),
                position(target$end[[1L]], target$columns[[2L]]))
        }
        if (!is.null(target$range)) return(list(uri = target$uri, range = target$range))
        return(NULL)
    }
    if (!is.null(method$source) || is.null(method$body_hash)) return(NULL)
    name <- method$name
    pkg <- method$package
    if (is.null(pkg) || !isTRUE(method$verified_binding)) return(NULL)
    verified <- if (isNamespaceLoaded(pkg)) {
        s3_verified_method_body(pkg, name, method$owner, method$body_hash, materialize = FALSE)
    } else {
        list(deferred = TRUE)
    }
    if (isTRUE(verified$deferred)) return(verified)
    s3_implementation_location(method, workspace, store, verified)
}

s3_implementation_location <- function(method, workspace, store, verified) {
    if (is.null(verified)) return(NULL)
    if (!is.null(verified$origin)) {
        target <- member_file_definition(verified$origin, workspace)
        if (!is.null(target)) return(target)
    }
    package_body_location(method$package, method$name, verified$code, store)
}

s3_verified_method_body <- function(pkg, name, owner, hash, materialize, ns = asNamespace(pkg)) {
    if (!exists(name, ns, inherits = FALSE)) return(NULL)
    binding <- member_binding(ns, name, materialize = materialize)
    if (!materialize && identical(binding[[1L]], "deferred")) return(list(deferred = TRUE))
    if (!identical(binding[[1L]], "value") || typeof(binding[[2L]]) != "closure") return(NULL)
    fn <- binding[[2L]]
    current <- if (isNamespace(environment(fn))) unname(getNamespaceName(environment(fn))) else pkg
    if (!identical(current, owner) || !identical(s3_function_identity(member_function_syntax(fn), current), hash)) return(NULL)
    list(origin = member_function_origin(fn), code = deparse(fn))
}

s3_implementation_unique <- function(result, workspace, document, point) {
    source <- result$method$source
    # The usual hover resolver can choose the first peer with a matching
    # binding. A navigation target needs one visible declaration owner.
    sources <- s3_context_sources(document$uri, workspace, document)
    if (length(sources) < 2L) return(TRUE)
    for (name in unique(c(source$binding, if (is.null(result$location$call$package)) result$location$call$token))) {
        own <- sources[[1L]]$index$histories[[name]]
        if (length(own) && member_history_position(own, c(point$row, point$col))) next
        declares <- vapply(sources, function(view) {
            any(vapply(view$index$histories[[name]], function(record) identical(record$kind, "binding"), logical(1L)))
        }, logical(1L))
        if (sum(declares) > 1L) return(FALSE)
    }
    TRUE
}

implementation_method <- function(workspace, document, point) {
    if (!check_r_region(document, point)) return(NULL)
    parsed <- current_parse_data(document$uri, workspace, document)
    call <- class_navigation_symbol(document, parsed, point)$call
    if (is.null(call)) return(NULL)
    result <- resolve_s3_call(document$uri, workspace, document, point, call = call)
    if (!is.null(result) && s3_implementation_unique(result, workspace, document, point)) result$method
}

class_s7_method_declarations <- function(parsed) {
    Filter(function(cls) identical(cls$type, "S7Method"), parsed$class_data$declarations)
}

s7_implementation_reply <- function(id, workspace, document, point) {
    if (!isTRUE(lsp_settings$get("member_completion")) || !check_r_region(document, point)) return(NULL)
    parsed <- current_parse_data(document$uri, workspace, document)
    declarations <- class_s7_method_declarations(parsed)
    if (!length(declarations)) return(NULL)
    symbol <- class_navigation_symbol(document, parsed, point)
    if (is.null(symbol$call) || !is.null(symbol$call$package)) return(NULL)
    cursor <- symbol$location$cursor
    cursor$s7_implementation <- TRUE
    resolved <- member_resolve_cursor(document$uri, workspace, document, symbol$location$point,
        cursor, cursor$token, origins = TRUE)
    if (is.null(resolved$value$s7_dispatch_args)) return(NULL)
    source <- resolved$value$s7_implementation
    # A type match alone cannot identify a source declaration. Match the
    # selected registration's physical span against this version's catalog.
    if (!identical(source$uri, document$uri) || isTRUE(resolved$budget$exhausted) ||
            isTRUE(resolved$value$possible_receiver)) return(Response$new(id))
    span <- range(document$to_lsp_position(source$start[[1L]], source$start[[2L]]),
        document$to_lsp_position(source$end[[1L]], source$end[[2L]]))
    selected <- Filter(function(cls) equal_range(cls$range, span), declarations)
    if (length(selected) != 1L) return(Response$new(id))
    selected <- selected[[1L]]
    peers <- setdiff(workspace_document_uris(workspace, document$uri), document$uri)
    competing <- class_workspace_declarations(workspace, peers)
    ambiguous <- vapply(competing, function(cls) {
        identical(cls$type, "S7Method") && identical(cls$class_name, selected$class_name)
    }, logical(1L))
    if (any(ambiguous)) return(Response$new(id))
    Response$new(id, result = location(document$uri, selected$selectionRange))
}

implementation_reply <- function(id, uri, workspace, document, point, store = NULL, defer = NULL) {
    s7 <- s7_implementation_reply(id, workspace, document, point)
    if (!is.null(s7)) return(s7)
    method <- implementation_method(workspace, document, point)
    target <- if (!is.null(method)) s3_implementation_target(method, workspace, store)
    if (isTRUE(target$deferred)) {
        if (is.function(defer)) {
            defer(method)
            return(NULL)
        }
        target <- NULL
    }
    Response$new(id, result = target)
}

# Package initialization and lazy method lookup must leave the server free
# to process edits and cancellation. The resolve manager uses isolated workers.
queue_implementation_lookup <- function(self, id, workspace, document, point, method) {
    uri <- document$uri
    key <- request_id_key(id)
    task_key <- paste0("\001implementation-", key)
    version <- document$version
    snapshot <- document$content
    generation <- document$parse_generation
    identity <- c("package", "name", "owner", "body_hash")
    current <- function() {
        identical(workspace, self$get_workspace(uri)) &&
            identical(document, workspace$documents$get(uri, NULL)) &&
            identical(version, document$version) && identical(snapshot, document$content) &&
            identical(generation, document$parse_generation)
    }
    # A late result from a replaced or cancelled request cannot send a second response.
    ticket <- new.env(parent = emptyenv())
    finish <- function(verified) {
        if (!identical(self$implementation_requests$get(key, NULL)$ticket, ticket)) return(invisible(NULL))
        self$implementation_requests$remove(key)
        if (!current()) {
            self$deliver(ResponseErrorMessage$new(id, "RequestCancelled", "Document changed during method lookup"))
            return(invisible(NULL))
        }
        target <- tryCatch({
            selected <- implementation_method(workspace, document, point)
            if (identical(selected[identity], method[identity])) {
                s3_implementation_location(method, workspace, self$text_document_content, verified)
            }
        }, error = function(e) {
            logger$info("S3 implementation lookup: ", e)
            NULL
        })
        self$deliver(Response$new(id, result = target))
    }
    task <- create_task(package_call(s3_verified_method_body),
        args = list(pkg = method$package, name = method$name, owner = method$owner, hash = method$body_hash, materialize = TRUE),
        callback = finish, error = function(e) finish(NULL), profiles = FALSE)
    self$implementation_requests$set(key, list(
        id = id, ticket = ticket, task_key = task_key, current = current, timeout = worker_timeout(3)
    ))
    self$resolve_task_manager$add_task(task_key, task)
    invisible(NULL)
}

check_implementation_requests <- function(self, cancel_id = NULL) {
    requests <- self$implementation_requests
    if (is.null(requests) || !requests$size()) return(invisible(NULL))
    keys <- if (is.null(cancel_id)) requests$keys() else request_id_key(cancel_id)
    for (key in keys) {
        entry <- requests$get(key, NULL)
        if (is.null(entry)) next
        expired <- FALSE
        if (is.null(cancel_id) && entry$current()) {
            task <- self$resolve_task_manager$get_task(entry$task_key)
            start <- task$execution_started_at
            expired <- !is.null(start) && proc.time()[[3L]] - start >= entry$timeout
            if (!expired) next
        }
        requests$remove(key)
        self$resolve_task_manager$cancel(entry$task_key)
        reply <- if (expired) Response$new(entry$id) else
            ResponseErrorMessage$new(entry$id, "RequestCancelled", "Method lookup cancelled")
        self$deliver(reply)
    }
    invisible(NULL)
}

class_navigation_waiting <- function(self, workspace, document, point, method) {
    implementation <- identical(method, "textDocument/implementation")
    members <- isTRUE(lsp_settings$get("member_completion"))
    s3 <- implementation && isTRUE(lsp_settings$get("s3_dispatch"))
    if ((!members && !s3) || !check_r_region(document, point)) return(FALSE)
    if (!identical(document$version, document$parse_data$version)) return(TRUE)
    if (identical(method, "textDocument/implementation")) {
        call <- class_navigation_symbol(document, document$parse_data, point)$call
        if (is.null(call)) return(FALSE)
        s7 <- members && is.null(call$package) && length(class_s7_method_declarations(document$parse_data)) > 0L
        return(((s3 || s7) && index_context_waiting(self, workspace, document, point, call = call)) ||
                (s3 && s3_metadata_waiting(self, document$uri, document, point, call = call)) ||
                (s7 && member_metadata_waiting(self, document$uri, document, point, call = call)))
    }
    location <- member_hover_location(document, point)
    if (is.null(location)) location <- class_navigation_symbol(document, document$parse_data, point)$location
    if (is.null(location)) return(FALSE)
    index_context_waiting(self, workspace, document, point, hover = TRUE) ||
        member_metadata_waiting(self, document$uri, document, point, location$cursor, location$cursor$token)
}

class_navigation_request <- function(self, id, params, method) {
    uri <- uri_escape_unicode(params$textDocument$uri)
    workspace <- self$get_workspace(uri)
    document <- workspace$documents$get(uri, NULL)
    if (is.null(document)) return(self$deliver(Response$new(id)))
    if (reply_parse_failure(self, id, document)) return(invisible(NULL))
    point <- document$from_lsp_position(params$position)
    if (class_navigation_waiting(self, workspace, document, point, method)) {
        enqueue_latest_reply(self, uri, method, list(id = id, version = document$version, params = params))
        manager <- if (identical(method, "textDocument/implementation")) self$resolve_task_manager else self$member_task_manager
        if (is.function(manager$promote)) manager$promote(uri)
        if (identical(method, "textDocument/implementation") && length(class_s7_method_declarations(document$parse_data)) &&
                is.function(self$member_task_manager$promote)) self$member_task_manager$promote(uri)
        return(invisible(NULL))
    }
    reply <- if (identical(method, "textDocument/implementation")) {
        implementation_reply(id, uri, workspace, document, point, self$text_document_content,
            defer = function(method) queue_implementation_lookup(self, id, workspace, document, point, method))
    } else {
        type_definition_reply(id, uri, workspace, document, point)
    }
    if (!is.null(reply)) self$deliver(reply)
}
