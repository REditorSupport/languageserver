prepare_rename_reply <- function(id, uri, workspace, document, point) {
    if (!check_r_region(document, point)) {
        return(ResponseErrorMessage$new(
            id,
            errortype = "RequestCancelled",
            message = "Cannot rename outside an R region"
        ))
    }

    token <- reference_token_at(document, workspace$get_parse_data(uri), point)
    if (!nzchar(token$token) || !check_scope(uri, document, point)) {
        return(ResponseErrorMessage$new(id, "RequestCancelled", "Cannot rename the symbol"))
    }
    defn <- definition_reply(NULL, uri, workspace, document, point)

    logger$info("prepare_rename_reply: ", list(
        token = token,
        defn = defn$result
    ))

    if (length(defn$result)) {
        Response$new(
            id,
            result = range(
                start = document$to_lsp_position(
                    row = token$range$start$row,
                    col = token$range$start$col),
                end = document$to_lsp_position(
                    row = token$range$end$row,
                    col = token$range$end$col)
            )
        )
    } else {
        ResponseErrorMessage$new(
            id,
            errortype = "RequestCancelled",
            message = "Cannot rename the symbol"
        )
    }
}

#' @noRd
rename_reply <- function(id, uri, workspace, document, point, newName) {
    if (!is.character(newName) || length(newName) != 1L || is.na(newName) || !nzchar(newName)) {
        return(ResponseErrorMessage$new(id, "InvalidParams", "The new name must be a non-empty string"))
    }
    if (!check_r_region(document, point)) {
        return(Response$new(id))
    }

    refs <- references_reply(NULL, uri, workspace, document, point)
    uris <- vapply(refs$result, function(ref) ref$uri, character(1L))
    groups <- split(seq_along(uris), factor(uris, levels = unique(uris)))
    replacement <- quote_completion_name(newName)
    result <- lapply(groups, function(indices) {
        lapply(refs$result[indices], function(ref) {
            text_edit(range = ref$range, new_text = replacement)
        })
    })

    logger$info("rename_reply: ", result)

    if (length(result)) {
        Response$new(
            id,
            result = list(
                changes = result
            )
        )
    } else {
        Response$new(
            id
        )
    }
}
