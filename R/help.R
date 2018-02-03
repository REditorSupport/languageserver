get_help <- function(topic, package = NULL) {
    if (is.null(package) || is.na(package)) {
        hfile <- as.character(help((topic)))
    } else {
        hfile <- as.character(help((topic), (package)))
    }
    pkgname <- basename(dirname(dirname(hfile)))
    query <- paste0("/library/", pkgname, "/html/", basename(hfile), ".html")
    html <- suppressWarnings(tools:::httpd(query, NULL, NULL))$payload
    match <- suppressWarnings(regexpr("<body>.*</body>", html))
   if (match < 0) {
      html <- NULL
   } else {
      html <- substring(html, match + 6, match + attr(match, "match.length") - 1 - 7)
   }
    html
}
