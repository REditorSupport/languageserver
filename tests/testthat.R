library(testthat)
library(languageserver)

filter <- Sys.getenv("TESTTHAT_FILTER", "")
if (nzchar(filter)) {
    test_check("languageserver", filter = filter)
} else {
    test_check("languageserver")
}
