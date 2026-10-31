provider_fixture <- function(content, formals_resolver = function(...) NULL) {
    uri <- "file:///provider-fixture.R"
    document <- Document$new(uri, language = "r", version = 1L, content = content)
    parse_data <- parse_document(uri, content)
    parse_data$version <- document$version
    parse_data$xml_doc <- xml2::read_xml(parse_data$xml_data)
    document$update_parse_data(parse_data)

    documents <- collections::dict()
    documents$set(uri, document)
    workspace <- new.env(parent = baseenv())
    workspace$documents <- documents
    workspace$get_parse_data <- function(request_uri) {
        documents$get(request_uri, NULL)$parse_data
    }
    workspace$get_formals <- formals_resolver

    list(uri = uri, document = document, workspace = workspace)
}

provider_startup_s3 <- local({
    cached <- NULL
    function() {
        if (is.null(cached)) {
            cached <<- stats::setNames(lapply(startup_packages, s3_prepare_namespace), startup_packages)
        }
        cached
    }
})

inlay_hint_edit <- function(fixture, version, changes) {
    document <- fixture$document
    document$apply_content_changes(version, changes)
    parsed <- parse_document(fixture$uri, document$content, document$is_rmarkdown)
    parsed$version <- version
    parsed$xml_doc <- xml2::read_xml(parsed$xml_data)
    document$update_parse_data(parsed)
    parsed
}
