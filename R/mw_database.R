# ==============================================================================
# mw_database.R
# Two separate MW databases:
#   molecular_weights_substances.xlsx → for Dry Substance tab
#   molecular_weights_proteins.xlsx   → for MW-Based Preparation tab
# ==============================================================================

# --- Built-in fallbacks ------------------------------------------------------

get_builtin_substances <- function() {
  list(
    `Common substances` = data.frame(
      substance = c(
        "NaCl", "KCl", "CaCl2", "MgCl2", "MgSO4",
        "HEPES", "Tris", "MOPS", "MES", "PIPES",
        "EDTA", "EGTA", "SDS", "DTT", "PMSF",
        "Glucose", "Sucrose", "Glycerol", "Urea",
        "ATP", "DMSO", "Ethanol", "Methanol",
        "Tween-20", "Triton X-100", "Beta-mercaptoethanol",
        "Sodium acetate", "Ammonium sulfate", "Sodium phosphate dibasic"
      ),
      MW = c(
        58.44, 74.55, 110.98, 95.21, 120.37,
        238.30, 121.14, 209.26, 195.24, 302.37,
        292.24, 380.35, 288.37, 154.25, 174.19,
        180.16, 342.30, 92.09, 60.06,
        507.18, 78.13, 46.07, 32.04,
        1228, 625, 78.13,
        82.03, 132.14, 141.96
      ),
      stringsAsFactors = FALSE
    ),
    `Custom substances` = data.frame(
      substance = character(0), MW = numeric(0), stringsAsFactors = FALSE
    )
  )
}

get_builtin_proteins <- function() {
  list(
    `Common proteins` = data.frame(
      substance = c(
        "BSA", "IgG (human)", "IgG (mouse)", "IgG (rabbit)", "IgG (rat)",
        "IgM (human)", "IgA (human)",
        "Streptavidin", "Avidin", "Protein A", "Protein G",
        "GFP", "mCherry", "RFP",
        "Insulin (human)", "Lysozyme", "Transferrin",
        "TNF-alpha (human)", "IL-6 (human)", "IL-2 (human)",
        "EGF (human)", "FGF-basic (human)", "VEGF (human)",
        "Herceptin (Trastuzumab)", "Rituximab"
      ),
      MW = c(
        66500, 150000, 150000, 150000, 150000,
        970000, 160000,
        53000, 66000, 42000, 21600,
        26900, 28800, 25900,
        5808, 14300, 79600,
        17400, 21000, 15500,
        6200, 17200, 38200,
        148000, 145000
      ),
      stringsAsFactors = FALSE
    ),
    `Custom proteins` = data.frame(
      substance = character(0), MW = numeric(0), stringsAsFactors = FALSE
    )
  )
}

# --- Generic loader ----------------------------------------------------------

#' Load a MW database from an Excel file
#' @param xlsx_path Path to the Excel file
#' @param builtin_fallback Function returning builtin list of sheets
#' @return Named list: one entry per sheet + $all combined + $sheet_names
load_mw_file <- function(xlsx_path, builtin_fallback) {
  builtin <- builtin_fallback()
  sheet_names <- names(builtin)
  sheets <- builtin

  if (file.exists(xlsx_path)) {
    tryCatch({
      if (requireNamespace("readxl", quietly = TRUE)) {
        available <- readxl::excel_sheets(xlsx_path)
        for (sn in sheet_names) {
          if (sn %in% available) {
            df <- readxl::read_excel(xlsx_path, sheet = sn)
            df <- as.data.frame(df, stringsAsFactors = FALSE)
            if (all(c("substance", "MW") %in% names(df))) {
              sheets[[sn]] <- df
            }
          }
        }
      }
    }, error = function(e) {
      message("Could not read MW file ", xlsx_path, ": ", e$message)
    })
  }

  sheets$all <- do.call(rbind, sheets[sheet_names])
  sheets$sheet_names <- sheet_names
  sheets
}

#' Load the substances database (for Dry Substance tab)
load_substances_db <- function(data_dir = "data") {
  load_mw_file(
    file.path(data_dir, "molecular_weights_substances.xlsx"),
    get_builtin_substances
  )
}

#' Load the proteins database (for MW-Based Preparation tab)
load_proteins_db <- function(data_dir = "data") {
  load_mw_file(
    file.path(data_dir, "molecular_weights_proteins.xlsx"),
    get_builtin_proteins
  )
}

# --- Lookup helpers ----------------------------------------------------------

#' Look up MW in a specific sheet of a database
lookup_mw <- function(substance, db, sheet = NULL) {
  if (!is.null(sheet) && sheet %in% db$sheet_names) {
    df <- db[[sheet]]
  } else {
    df <- db$all
  }
  idx <- match(tolower(substance), tolower(df$substance))
  if (!is.na(idx)) return(df$MW[idx])
  NA_real_
}

#' Get substance names from a specific sheet
get_substance_names_from_sheet <- function(db, sheet) {
  if (sheet %in% db$sheet_names && nrow(db[[sheet]]) > 0) {
    sort(db[[sheet]]$substance)
  } else {
    character(0)
  }
}

#' Get all substance names across all sheets
get_substance_names <- function(db) {
  sort(db$all$substance)
}
