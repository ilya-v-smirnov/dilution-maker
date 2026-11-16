# ==============================================================================
# mw_database.R
# Databases and their files (paths relative to the data folder):
#   common_substances.xlsx           Common substances  -> Dry Substance tab
#   common_proteins.xlsx             Common proteins    -> MW-Based tab
#   private/custom_substances.xlsx   Custom substances  -> Dry Substance tab
#   private/custom_proteins.xlsx     Custom proteins    -> MW-Based tab
#   private/available_stocks.xlsx    Available stocks   -> all dilution tabs
#   private/pipettes.xlsx            Pipettes           -> volume precision
# Common databases ship with the app; everything in private/ is the user's own
# data and is excluded from git. Missing files are created at start-up
# (see init_data_files()).
# ==============================================================================

# Small helper used across UI modules
`%||%` <- function(a, b) if (is.null(a)) b else a

# Sub-folder (inside the data folder) for user-specific databases
PRIVATE_DATA_DIR <- "private"

# Database key -> file path relative to the data folder
DB_FILES <- c(
  common_substances = "common_substances.xlsx",
  common_proteins   = "common_proteins.xlsx",
  custom_substances = file.path(PRIVATE_DATA_DIR, "custom_substances.xlsx"),
  custom_proteins   = file.path(PRIVATE_DATA_DIR, "custom_proteins.xlsx"),
  stocks            = file.path(PRIVATE_DATA_DIR, "available_stocks.xlsx"),
  pipettes          = file.path(PRIVATE_DATA_DIR, "pipettes.xlsx")
)

#' Full path of a database file
db_file_path <- function(data_dir, key) file.path(data_dir, DB_FILES[[key]])

# MW database sheet -> database key
MW_SHEET_KEYS <- c(
  `Common substances` = "common_substances",
  `Custom substances` = "custom_substances",
  `Common proteins`   = "common_proteins",
  `Custom proteins`   = "custom_proteins"
)

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
    `Custom substances` = empty_custom_substances_df()
  )
}

#' Empty Custom substances sheet. Custom substances carry optional
#' supplier / purity (%) / catalogue number on top of substance + MW.
empty_custom_substances_df <- function() {
  data.frame(
    substance = character(0), MW = numeric(0),
    supplier  = character(0), purity = numeric(0), cat = character(0),
    stringsAsFactors = FALSE
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

# --- Generic MW loader -------------------------------------------------------

#' Load an MW database (several sheets, one file per sheet) and fall back
#' to the built-in content for any missing or unreadable file.
load_mw_db <- function(data_dir, builtin_fallback) {
  builtin <- builtin_fallback()
  sheet_names <- names(builtin)
  sheets <- builtin

  for (sn in sheet_names) {
    path <- db_file_path(data_dir, MW_SHEET_KEYS[[sn]])
    df <- read_mw_sheet(path, sn)
    if (!is.null(df)) sheets[[sn]] <- sort_mw_sheet(conform_to_template(df, builtin[[sn]]))
  }

  sheets$sheet_names <- sheet_names
  rebuild_mw_all(sheets)
}

#' Read one MW sheet from its file (sheet named `sheet`, else the first one).
#' @return data.frame, or NULL when the file is missing or unreadable.
read_mw_sheet <- function(path, sheet) {
  if (!file.exists(path) || !requireNamespace("readxl", quietly = TRUE)) return(NULL)
  tryCatch({
    available <- readxl::excel_sheets(path)
    df <- readxl::read_excel(path, sheet = if (sheet %in% available) sheet else 1)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    if (!all(c("substance", "MW") %in% names(df))) return(NULL)
    df[!is.na(df$substance) & nzchar(trimws(as.character(df$substance))), , drop = FALSE]
  }, error = function(e) {
    message("Could not read MW file ", path, ": ", e$message)
    NULL
  })
}

#' Coerce a loaded sheet to the template's columns and types. Columns
#' missing from the file (e.g. an older file without supplier/purity/cat)
#' are added empty; extra columns are dropped.
conform_to_template <- function(df, template) {
  n <- nrow(df)
  out <- lapply(names(template), function(col) {
    proto <- template[[col]]
    if (col %in% names(df)) {
      x <- df[[col]]
      if (is.numeric(proto)) {
        x <- suppressWarnings(as.numeric(x))
      } else {
        x <- as.character(x)
        x[is.na(x)] <- ""
        x <- trimws(x)
      }
    } else {
      x <- if (is.numeric(proto)) rep(NA_real_, n) else rep("", n)
    }
    x
  })
  names(out) <- names(template)
  as.data.frame(out, stringsAsFactors = FALSE)
}

#' Alphabetical order by substance (then supplier, then purity when present).
sort_mw_sheet <- function(df) {
  if (nrow(df) == 0) return(df)
  keys <- list(tolower(df$substance))
  if ("supplier" %in% names(df)) keys <- c(keys, list(tolower(df$supplier)))
  if ("purity" %in% names(df)) keys <- c(keys, list(df$purity))
  df <- df[do.call(order, keys), , drop = FALSE]
  rownames(df) <- NULL
  df
}

#' Recompute db$all (substance + MW across all sheets). Sheets may have
#' different columns, so only the shared ones are combined.
rebuild_mw_all <- function(db) {
  db$all <- do.call(rbind, lapply(db[db$sheet_names], function(df) {
    df[, c("substance", "MW"), drop = FALSE]
  }))
  db
}

load_substances_db <- function(data_dir = "data") {
  load_mw_db(data_dir, get_builtin_substances)
}

load_proteins_db <- function(data_dir = "data") {
  load_mw_db(data_dir, get_builtin_proteins)
}

# --- Generic MW saver --------------------------------------------------------

#' Write a data frame to a single-sheet xlsx file (creating folders).
write_db_sheet <- function(df, path, sheet) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required to save databases.")
  }
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  openxlsx::write.xlsx(stats::setNames(list(df), sheet), path, overwrite = TRUE)
}

#' Save sheets of an MW database, each to its own file, sorted alphabetically.
#' Only the given sheets are written, so editing Custom entries never
#' rewrites the Common file.
save_mw_db <- function(db, data_dir, sheets = db$sheet_names) {
  for (sn in intersect(sheets, db$sheet_names)) {
    write_db_sheet(sort_mw_sheet(db[[sn]]), db_file_path(data_dir, MW_SHEET_KEYS[[sn]]), sn)
  }
  invisible(TRUE)
}

save_substances_db <- function(db, data_dir = "data", sheets = db$sheet_names) {
  save_mw_db(db, data_dir, sheets)
}

save_proteins_db <- function(db, data_dir = "data", sheets = db$sheet_names) {
  save_mw_db(db, data_dir, sheets)
}

# --- MW lookup helpers -------------------------------------------------------

lookup_mw <- function(substance, db, sheet = NULL) {
  if (!is.null(sheet) && sheet %in% db$sheet_names) {
    df <- db[[sheet]]
  } else {
    df <- db$all
  }
  if (is.null(df) || nrow(df) == 0) return(NA_real_)
  idx <- match(tolower(substance), tolower(df$substance))
  if (!is.na(idx)) return(df$MW[idx])
  NA_real_
}

get_substance_names_from_sheet <- function(db, sheet) {
  if (sheet %in% db$sheet_names && nrow(db[[sheet]]) > 0) {
    sort(db[[sheet]]$substance)
  } else {
    character(0)
  }
}

get_substance_names <- function(db) {
  sort(db$all$substance)
}

# --- Substance display labels (supplier / purity) ----------------------------

#' Purity as text: 97 -> "97%", 99.5 -> "99.5%"; "" when missing.
format_purity <- function(purity) {
  purity <- suppressWarnings(as.numeric(purity))
  vapply(purity, function(p) {
    if (is.na(p)) "" else paste0(trimws(format(p, scientific = FALSE, drop0trailing = TRUE)), "%")
  }, character(1), USE.NAMES = FALSE)
}

#' Display label "substance supplier purity%", skipping empty parts,
#' e.g. "EDTA Na2 Merck 97%", "EDTA Na2 97%", "EDTA Na2 Merck".
substance_label <- function(substance, supplier = "", purity = NA_real_) {
  supplier <- ifelse(is.na(supplier), "", trimws(as.character(supplier)))
  pur <- format_purity(purity)
  out <- trimws(as.character(substance))
  out <- ifelse(nzchar(supplier), paste(out, supplier), out)
  ifelse(nzchar(pur), paste(out, pur), out)
}

#' Labels for the rows of a sheet (just the substance name for sheets
#' without supplier/purity columns).
sheet_labels <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(character(0))
  if (!all(c("supplier", "purity") %in% names(df))) return(df$substance)
  substance_label(df$substance, df$supplier, df$purity)
}

#' Display labels for a sheet in display (alphabetical) order.
get_substance_labels_from_sheet <- function(db, sheet) {
  if (!sheet %in% db$sheet_names) return(character(0))
  sheet_labels(sort_mw_sheet(db[[sheet]]))
}

#' Resolve a sheet row by its display label.
#' @return list(substance, label, MW, supplier, purity, cat) or NULL.
lookup_substance_entry <- function(db, sheet, label) {
  if (is.null(label) || !sheet %in% db$sheet_names) return(NULL)
  df <- db[[sheet]]
  idx <- match(label, sheet_labels(df))
  if (is.na(idx)) return(NULL)
  row <- df[idx, , drop = FALSE]
  list(
    substance = row$substance,
    label     = label,
    MW        = row$MW,
    supplier  = if ("supplier" %in% names(row)) row$supplier else "",
    purity    = if ("purity" %in% names(row)) row$purity else NA_real_,
    cat       = if ("cat" %in% names(row)) row$cat else ""
  )
}

#' Validate an optional purity value (%): NULL when OK, else an error message.
validate_purity <- function(purity) {
  if (is.null(purity) || length(purity) == 0 || is.na(purity)) return(NULL)
  if (!is.numeric(purity) || purity <= 0 || purity > 100) {
    return("Purity must be greater than 0 and not more than 100 (%).")
  }
  NULL
}

# ==============================================================================
# Available Stocks database
# ==============================================================================

#' Empty stocks data frame with the canonical column structure.
empty_stocks_df <- function() {
  data.frame(
    substance = character(0),
    date      = character(0),
    conc      = numeric(0),
    units     = character(0),
    comments  = character(0),
    stringsAsFactors = FALSE
  )
}

#' Load the available stocks database from xlsx.
#' @return list with $data (data.frame) and $sheet_name
load_stocks_db <- function(data_dir = "data") {
  xlsx_path <- db_file_path(data_dir, "stocks")
  df <- empty_stocks_df()

  if (file.exists(xlsx_path)) {
    tryCatch({
      if (requireNamespace("readxl", quietly = TRUE)) {
        loaded <- readxl::read_excel(xlsx_path, sheet = 1)
        loaded <- as.data.frame(loaded, stringsAsFactors = FALSE)
        # Ensure expected columns exist
        for (col in c("substance", "date", "conc", "units", "comments")) {
          if (!col %in% names(loaded)) {
            loaded[[col]] <- if (col == "conc") rep(NA_real_, nrow(loaded)) else rep("", nrow(loaded))
          }
        }
        loaded$substance <- as.character(loaded$substance)
        loaded$date      <- as.character(loaded$date)
        loaded$conc      <- suppressWarnings(as.numeric(loaded$conc))
        # Units typed by hand in Excel ("mg/mL", Greek-mu "ug/ml", "mcg/ml")
        # would otherwise not match the app's unit list
        loaded$units     <- canonical_conc_unit(loaded$units)
        loaded$comments  <- as.character(loaded$comments)
        loaded$comments[is.na(loaded$comments)] <- ""
        loaded$date[is.na(loaded$date)]         <- ""
        # Drop rows without a substance name
        loaded <- loaded[!is.na(loaded$substance) & nzchar(trimws(loaded$substance)), , drop = FALSE]
        df <- loaded[, c("substance", "date", "conc", "units", "comments"), drop = FALSE]
        if (nrow(df) > 0) {
          df <- df[order(tolower(df$substance), df$date), , drop = FALSE]
        }
      }
    }, error = function(e) {
      message("Could not read stocks file ", xlsx_path, ": ", e$message)
    })
  }

  list(data = df, sheet_name = "Available stocks")
}

#' Save the stocks database back to xlsx (sorted by substance, then date).
save_stocks_db <- function(stocks_db, data_dir = "data") {
  df <- stocks_db$data
  if (is.null(df)) df <- empty_stocks_df()
  if (nrow(df) > 0) df <- df[order(tolower(df$substance), df$date), , drop = FALSE]
  write_db_sheet(df, db_file_path(data_dir, "stocks"), "Available stocks")
}

# --- Stocks helpers ----------------------------------------------------------

#' Build a label "date - comments" (or just "date" when comments are empty).
stock_variant_label <- function(date, comments) {
  date <- if (is.null(date)) "" else as.character(date)
  comments <- if (is.null(comments)) "" else as.character(comments)
  if (nzchar(comments)) paste0(date, " - ", comments) else date
}

#' Get unique substance names from the stocks DB (alphabetical).
get_unique_stock_substances <- function(stocks_db) {
  df <- stocks_db$data
  if (is.null(df) || nrow(df) == 0) return(character(0))
  sort(unique(df$substance))
}

#' Get stock variant labels for a given substance.
#' @return list(labels = character, data = subset data frame)
get_stock_variants <- function(stocks_db, substance) {
  df <- stocks_db$data
  if (is.null(df) || nrow(df) == 0 || is.null(substance) || !nzchar(substance)) {
    return(list(labels = character(0), data = empty_stocks_df()))
  }
  filtered <- df[df$substance == substance, , drop = FALSE]
  if (nrow(filtered) == 0) {
    return(list(labels = character(0), data = empty_stocks_df()))
  }
  labels <- mapply(stock_variant_label, filtered$date, filtered$comments, USE.NAMES = FALSE)
  list(labels = labels, data = filtered)
}

#' Resolve a single stock row by substance + variant label.
#' @return one-row data.frame, or NULL when not found.
get_stock_by_variant <- function(stocks_db, substance, variant_label) {
  v <- get_stock_variants(stocks_db, substance)
  if (length(v$labels) == 0) return(NULL)
  idx <- which(v$labels == variant_label)
  if (length(idx) == 0) return(NULL)
  v$data[idx[1], , drop = FALSE]
}

#' Build the substance-with-stock-suffix label used in instructions.
augment_name_with_stock <- function(base_name, stock_row) {
  if (is.null(stock_row) || nrow(stock_row) == 0) return(base_name)
  suffix <- stock_variant_label(stock_row$date, stock_row$comments)
  if (!nzchar(suffix)) return(base_name)
  paste0(base_name, "\t\t\t(", suffix, ")")
}

# ==============================================================================
# Start-up: create missing database files
# ==============================================================================

#' Create any missing database file: Common databases and pipettes from the
#' built-in content, Custom databases and stocks empty (headers only).
#' Existing files are never modified.
#' @return character vector of created file paths (invisibly)
init_data_files <- function(data_dir = "data") {
  created <- character(0)
  missing <- function(key) !file.exists(db_file_path(data_dir, key))
  mw_builtin <- c(get_builtin_substances(), get_builtin_proteins())

  for (sn in names(MW_SHEET_KEYS)) {
    key <- MW_SHEET_KEYS[[sn]]
    if (missing(key)) {
      write_db_sheet(mw_builtin[[sn]], db_file_path(data_dir, key), sn)
      created <- c(created, db_file_path(data_dir, key))
    }
  }
  if (missing("stocks")) {
    save_stocks_db(list(data = empty_stocks_df()), data_dir)
    created <- c(created, db_file_path(data_dir, "stocks"))
  }
  if (missing("pipettes")) {
    builtin <- get_builtin_pipettes()
    save_pipettes_db(list(data = builtin, groups = unique(builtin$group)), data_dir)
    created <- c(created, db_file_path(data_dir, "pipettes"))
  }

  if (length(created) > 0) {
    message("Created database file(s): ", paste(created, collapse = ", "))
  }
  invisible(created)
}
