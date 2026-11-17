# Give metadata workers more time than the interactive defaults when running
# the full suite: parallel test servers share the machine with their workers.
# CRAN keeps the defaults so a slow machine cannot stretch the check time.
#
# This lives in a setup file, not a helper: testthat creates its teardown
# environment before sourcing setup files but after sourcing helpers, and
# pkgload::load_all() sources helpers only. The value is removed again when the
# test run ends, so running tests from an interactive session does not change
# later worker limits in that session.
if (identical(Sys.getenv("NOT_CRAN"), "true") && !nzchar(Sys.getenv("R_LANGSVR_TIMEOUT_SCALE"))) {
    withr::local_envvar(R_LANGSVR_TIMEOUT_SCALE = "3", .local_envir = testthat::teardown_env())
}
