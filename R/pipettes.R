# ==============================================================================
# pipettes.R
# Pipette database (data/pipettes.xlsx) and pipette-based volume precision.
#
# Columns: name, min_volume, max_volume, increment (all volumes in µl), enabled.
# Rows are always kept sorted by min_volume, max_volume, increment, name.
#
# Pipette selection for a volume V (enabled pipettes only):
#   1. pipettes whose range covers V   -> the one with the smallest increment
#   2. V above every range             -> the largest pipette (no warning)
#   3. V below every range             -> the smallest pipette + warning
#   4. V in a gap between two ranges   -> the nearest smaller pipette
#                                         (several aspirations) + warning
# The calculation itself stays exact; only displayed volumes are rounded to
# the selected pipette's increment.
# ==============================================================================

PIPETTE_COLUMNS <- c("name", "min_volume", "max_volume", "increment", "enabled")

empty_pipettes_df <- function() {
  data.frame(name = character(0), min_volume = numeric(0),
             max_volume = numeric(0), increment = numeric(0),
             enabled = logical(0), stringsAsFactors = FALSE)
}

#' Built-in fallback: Gilson PIPETMAN Classic set
get_builtin_pipettes <- function() {
  data.frame(
    name       = c("P2",  "P10", "P20", "P100", "P200", "P1000"),
    min_volume = c(0.2,   1,     2,     10,     20,     100),
    max_volume = c(2,     10,    20,    100,    200,    1000),
    increment  = c(0.002, 0.02,  0.02,  0.2,    0.2,    2),
    enabled    = TRUE,
    stringsAsFactors = FALSE
  )
}

#' Sort pipettes by min_volume, max_volume, increment, then name
sort_pipettes <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(empty_pipettes_df())
  df <- df[order(df$min_volume, df$max_volume, df$increment, tolower(df$name)), ,
           drop = FALSE]
  rownames(df) <- NULL
  df
}

#' Coerce a raw table to the pipette schema (missing `enabled` -> TRUE)
clean_pipettes_df <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!all(c("name", "min_volume", "max_volume", "increment") %in% names(df))) {
    return(NULL)
  }
  if (!"enabled" %in% names(df)) df$enabled <- TRUE
  df$name       <- trimws(as.character(df$name))
  df$min_volume <- as.numeric(df$min_volume)
  df$max_volume <- as.numeric(df$max_volume)
  df$increment  <- as.numeric(df$increment)
  en <- df$enabled
  if (!is.logical(en)) en <- toupper(trimws(as.character(en))) %in% c("TRUE", "T", "1", "YES")
  en[is.na(en)] <- TRUE
  df$enabled <- en
  ok <- !is.na(df$name) & nzchar(df$name) &
        is.finite(df$min_volume) & is.finite(df$max_volume) &
        is.finite(df$increment) & df$increment > 0
  sort_pipettes(df[ok, PIPETTE_COLUMNS, drop = FALSE])
}

#' Load the pipette database; falls back to the built-in set
load_pipettes_db <- function(data_dir = "data") {
  xlsx_path <- file.path(data_dir, "pipettes.xlsx")
  df <- NULL
  if (file.exists(xlsx_path) && requireNamespace("readxl", quietly = TRUE)) {
    df <- tryCatch(clean_pipettes_df(readxl::read_excel(xlsx_path, sheet = 1)),
                   error = function(e) {
                     message("Could not read pipettes file ", xlsx_path, ": ", e$message)
                     NULL
                   })
  }
  if (is.null(df)) df <- sort_pipettes(get_builtin_pipettes())
  list(data = df, sheet_name = "Pipettes")
}

#' Save the pipette database (sorted) to data/pipettes.xlsx
save_pipettes_db <- function(pipettes_db, data_dir = "data") {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required to save the pipettes database.")
  }
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  df <- sort_pipettes(pipettes_db$data)
  openxlsx::write.xlsx(list(Pipettes = df), file.path(data_dir, "pipettes.xlsx"),
                       overwrite = TRUE)
}

#' Enabled pipettes only (sorted)
enabled_pipettes <- function(pipettes_db) {
  df <- pipettes_db$data
  if (is.null(df) || nrow(df) == 0) return(empty_pipettes_df())
  sort_pipettes(df[df$enabled, , drop = FALSE])
}

#' Human-readable pipette label: "P20 (2–20 µl, step 0.02 µl)"
pipette_label <- function(name, min_volume, max_volume, increment) {
  paste0(name, " (", num_plain(min_volume), "–", num_plain(max_volume),
         " µl, step ", num_plain(increment), " µl)")
}

num_plain <- function(x) trimws(format(x, scientific = FALSE, drop0trailing = TRUE))

# --- Selection ---------------------------------------------------------------

#' Select a pipette for a volume (µl) among the given (enabled) pipettes
#' @return list(name, increment, status) with status one of
#'   "in_range", "above_max", "below_min", "gap"; NULL if no pipettes
select_pipette <- function(vol_ul, pipettes) {
  if (is.null(pipettes) || nrow(pipettes) == 0) return(NULL)
  p <- sort_pipettes(pipettes)
  tol <- 1e-9 * max(1, vol_ul)
  pick <- function(i, status) list(name = p$name[i], increment = p$increment[i],
                                   status = status)

  covering <- which(p$min_volume - tol <= vol_ul & vol_ul <= p$max_volume + tol)
  if (length(covering) > 0) {
    # most precise; ties -> narrower range, then sort order
    i <- covering[order(p$increment[covering], p$max_volume[covering])][1]
    return(pick(i, "in_range"))
  }
  if (vol_ul > max(p$max_volume)) {
    largest <- which(p$max_volume == max(p$max_volume))
    return(pick(largest[order(p$increment[largest])][1], "above_max"))
  }
  if (vol_ul < min(p$min_volume)) {
    smallest <- which(p$min_volume == min(p$min_volume))
    return(pick(smallest[order(p$increment[smallest])][1], "below_min"))
  }
  # Gap: nearest pipette whose whole range lies below the volume
  below <- which(p$max_volume < vol_ul)
  nearest <- below[p$max_volume[below] == max(p$max_volume[below])]
  pick(nearest[order(p$increment[nearest])][1], "gap")
}

# --- Formatting --------------------------------------------------------------

#' Round a volume (µl) to a multiple of the pipette increment (µl)
round_to_increment <- function(vol_ul, increment) {
  round(round(vol_ul / increment) * increment, 10)
}

#' Format a volume rounded to a pipette increment, in the auto-chosen unit.
#' Decimals follow the increment; trailing zeros are dropped (app convention).
format_volume_increment <- function(value_L, increment_ul) {
  rounded_ul <- round_to_increment(L_to_volume(value_L, "µl"), increment_ul)
  rounded_L  <- volume_to_L(rounded_ul, "µl")
  unit <- best_volume_unit(rounded_L)
  shown <- L_to_volume(rounded_L, unit)
  inc_in_unit <- L_to_volume(volume_to_L(increment_ul, "µl"), unit)
  digits <- max(0, ceiling(-log10(inc_in_unit) - 1e-9))
  txt <- formatC(shown, format = "f", digits = digits)
  if (digits > 0) txt <- sub("\\.?0+$", "", txt)
  paste0(txt, " ", unit)
}

#' Default precision settings (manual mode, 3 significant figures)
default_precision <- function() {
  list(mode = "manual", stock_sig = 3, diluent_sig = 3, pipettes = NULL)
}

#' Format one volume according to precision settings.
#' @param precision list(mode = "manual"|"pipette", stock_sig, diluent_sig,
#'   pipettes = data frame of enabled pipettes)
#' @param role "stock" or "diluent" (selects significant figures in manual mode)
#' @return list(text, pipette (name or NULL), warning (character, maybe empty))
format_volume_precise <- function(value_L, precision = NULL, role = "stock") {
  if (is.null(precision)) precision <- default_precision()
  role_label <- if (role == "stock") "Stock volume" else "Diluent volume"

  if (identical(precision$mode, "pipette")) {
    if (!is.finite(value_L) || value_L <= 0) {
      return(list(text = format_volume(value_L), pipette = NULL, warning = character(0)))
    }
    vol_ul <- L_to_volume(value_L, "µl")
    sel <- select_pipette(vol_ul, precision$pipettes)
    if (is.null(sel)) {
      return(list(text = format_volume(value_L), pipette = NULL,
                  warning = "No pipettes are enabled: volumes shown with 3 significant figures."))
    }
    warn <- character(0)
    if (sel$status == "below_min") {
      warn <- paste0(role_label, " ", format_volume(value_L),
                     " is below the range of the smallest enabled pipette (",
                     sel$name, "). Consider a multi-step dilution.")
    } else if (sel$status == "gap") {
      warn <- paste0(role_label, " ", format_volume(value_L),
                     " is not covered by any enabled pipette; ", sel$name,
                     " precision used (several aspirations).")
    }
    return(list(text = format_volume_increment(value_L, sel$increment),
                pipette = sel$name, warning = warn))
  }

  sig <- if (role == "stock") precision$stock_sig else precision$diluent_sig
  list(text = format_volume_sig(value_L, sig %||% 3), pipette = NULL,
       warning = character(0))
}

#' Volume text with the pipette name appended, e.g. "3.48 µl (P20)"
volume_with_pipette <- function(fv) {
  if (is.null(fv$pipette)) fv$text else paste0(fv$text, " (", fv$pipette, ")")
}

#' Is pipette-based precision active?
is_pipette_mode <- function(precision) {
  !is.null(precision) && identical(precision$mode, "pipette")
}

#' Instruction text of a result/recipe, with or without pipette names
recipe_text <- function(r, show_pipettes = TRUE) {
  if (isTRUE(show_pipettes) && !is.null(r$instruction_text_pipettes)) {
    r$instruction_text_pipettes
  } else {
    r$instruction_text
  }
}

#' Compact, storable description of precision settings (for metadata)
precision_summary <- function(precision) {
  if (is.null(precision)) precision <- default_precision()
  list(mode        = precision$mode %||% "manual",
       stock_sig   = precision$stock_sig %||% NA_real_,
       diluent_sig = precision$diluent_sig %||% NA_real_,
       pipettes    = if (is_pipette_mode(precision) && !is.null(precision$pipettes))
                       paste(precision$pipettes$name, collapse = ", ") else NA_character_)
}
