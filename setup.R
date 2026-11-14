# ==============================================================================
# setup.R
# One-time setup on a new machine: installs renv (if needed) and the exact
# package versions recorded in renv.lock into the project library.
#
# Usage (from the project folder, e.g. after opening dilution-maker.Rproj):
#   source("setup.R")
# ==============================================================================

if (!file.exists("renv.lock")) {
  stop("renv.lock not found. Run this script from the dilution-maker project folder.")
}

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
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

check_r_version()
renv::restore(prompt = FALSE)

message("\nSetup complete. Start the app with source(\"run_app.R\") ",
        "or by double-clicking run_app.bat (Windows).")
