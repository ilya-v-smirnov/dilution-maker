# ==============================================================================
# run_tests.R
# Runs the test suite (tests/testthat). Tests use temporary folders and never
# modify data/.
#
# Usage (from the project folder): source("run_tests.R")
# ==============================================================================

if (!file.exists("app.R")) {
  stop("Run this script from the dilution-maker project folder.")
}
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is not installed. Run renv::restore() first.")
}

results <- testthat::test_dir("tests/testthat", reporter = "progress",
                              stop_on_failure = FALSE)
df <- as.data.frame(results)
cat(sprintf("\nTests: %d passed, %d failed, %d skipped\n",
            sum(df$passed), sum(df$failed), sum(df$skipped)))
invisible(results)
