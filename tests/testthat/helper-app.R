# ==============================================================================
# helper-app.R
# Loaded by testthat before the tests: sources the app's R/ modules and
# defines small helpers. Tests never touch the real data/ folder; database
# tests work in temporary directories.
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
})

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in list.files(file.path(app_root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE, encoding = "UTF-8")
}

# Units with the micro sign (kept as escapes so the files stay ASCII)
UL   <- "µl"
UM   <- "µM"
UGML <- "µg/ml"

#' Precision settings: pipette mode with the built-in Gilson set
pipette_precision <- function(pipettes = get_builtin_pipettes()) {
  list(mode = "pipette", group = DEFAULT_PIPETTE_GROUP, pipettes = pipettes,
       show_pipettes = TRUE)
}

#' Precision settings: manual mode
manual_precision <- function(stock_sig = 3, diluent_sig = 3) {
  list(mode = "manual", stock_sig = stock_sig, diluent_sig = diluent_sig)
}

#' Empty temporary data directory, removed when the calling test ends
local_data_dir <- function(env = parent.frame()) {
  withr::local_tempdir(.local_envir = env)
}
