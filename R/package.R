# a bit like devtools::load_all()
package_load_all <- function(self) {
    project_root <- self$rootPath
    source_dir <- file.path(project_root, "R")
    files <- list.files(source_dir)
    for (f in files) {
        logger$info("load ", f)
        path <- file.path(source_dir, f)
        uri <- path_to_uri(path)
        doc <- Document$new(uri, NULL, stringi::stri_read_lines(path))
        self$workspace$documents$set(uri, doc)
        self$text_sync(uri, document = doc, parse = TRUE)
    }
    package_imports(self)
}

package_imports <- function(self) {
    project_root <- self$rootPath
    namespace_file <- file.path(project_root, "NAMESPACE")

    if (file.exists(namespace_file)) {
        exprs <- tryCatch(
            parse(namespace_file),
            error = function(e) list())
        for (expr in exprs) {
            if (expr[[1]] == "import") {
                packages <- as.list(expr[-1])
                if (is.null(names(packages))) {
                    packages <- as.character(packages)
                } else {
                    # handle import(foo, except = c(bar))
                    packages <- as.character(packages[names(packages) == ""])
                }
                logger$info("load packages:", packages)
                self$workspace$update_import_packages(packages)
            } else if (expr[[1]] == "importFrom") {
                package <- as.character(expr[[2]])
                objects <- as.character(expr[3:length(expr)])
                logger$info("load package objects:", package, objects)
                self$workspace$update_import_from(package, objects)
            }
        }
    }
}
