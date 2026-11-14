# ==============================================================================
# run_app.R
# Starts Lab Dilution Planner in the default web browser.
#
# Usage:
#   - RStudio: open dilution-maker.Rproj, then source("run_app.R")
#   - Windows: double-click run_app.bat
#
# If any package recorded in renv.lock is missing (e.g. on the first run on a
# new machine), the recorded versions are installed first.
# ==============================================================================

if (!file.exists("app.R")) {
  stop("Run this script from the dilution-maker project folder.")
}

# Packages are locked for a specific R version; on another minor version
# binaries may be unavailable and packages would have to be compiled (Rtools).
check_r_version <- function(lockfile = "renv.lock") {
  locked <- renv::lockfile_read(lockfile)$R$Version
  if (is.null(locked)) return(invisible())
  same_minor <- identical(unclass(package_version(locked))[[1]][1:2],
                          unclass(getRversion())[[1]][1:2])
  if (!same_minor) {
    warning("This project was set up with R ", locked, ", but you are running R ",
            getRversion(), ". If package installation fails, install R ",
            sub("\\.[0-9]+$", ".x", locked),
            " (or Rtools to compile packages from source).",
            call. = FALSE, immediate. = TRUE)
  }
  invisible()
}

if (file.exists("renv.lock")) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv", repos = "https://cloud.r-project.org")
  }
  locked <- names(renv::lockfile_read("renv.lock")$Packages)
  missing <- setdiff(locked, rownames(utils::installed.packages()))
  if (length(missing) > 0) {
    check_r_version()
    message("Installing packages recorded in renv.lock: ",
            paste(missing, collapse = ", "))
    renv::restore(prompt = FALSE)
  }
}

shiny::runApp(".", launch.browser = TRUE)
