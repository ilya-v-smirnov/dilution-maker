# ==============================================================================
# pipettes.R
# Pipette database (data/private/pipettes.xlsx) and pipette-based volume precision.
#
# Columns: group, name, min_volume, max_volume, increment (all volumes in µl),
# enabled. Pipettes belong to named groups (e.g. "Tissue culture",
# "Working bench"); a name is unique within its group. Group order = creation
# order (sheet "Groups"); within a group rows are sorted by min_volume,
# max_volume, increment, name.
#
# Pipette selection for a volume V (enabled pipettes of the chosen group):
#   1. pipettes whose range covers V   -> the one with the smallest increment
#   2. V above every range             -> the largest pipette (no warning)
#   3. V below every range             -> the smallest pipette + warning
#   4. V in a gap between two ranges   -> the nearest smaller pipette
#                                         (several aspirations) + warning
# The calculation itself stays exact; only displayed volumes are rounded to
# the selected pipette's increment.
# ==============================================================================

PIPETTE_COLUMNS <- c("group", "name", "min_volume", "max_volume", "increment", "enabled")

# Group given to pipettes from files saved before groups existed
DEFAULT_PIPETTE_GROUP <- "Working bench"

empty_pipettes_df <- function() {
  data.frame(group = character(0), name = character(0), min_volume = numeric(0),
             max_volume = numeric(0), increment = numeric(0),
             enabled = logical(0), stringsAsFactors = FALSE)
}

#' Built-in fallback: Gilson PIPETMAN Classic set
get_builtin_pipettes <- function() {
  data.frame(
    group      = DEFAULT_PIPETTE_GROUP,
    name       = c("P2",  "P10", "P20", "P100", "P200", "P1000"),
    min_volume = c(0.2,   1,     2,     10,     20,     100),
    max_volume = c(2,     10,    20,    100,    200,    1000),
    increment  = c(0.002, 0.02,  0.02,  0.2,    0.2,    2),
    enabled    = TRUE,
    stringsAsFactors = FALSE
  )
}

#' Sort pipettes: groups in the given (creation) order, then by
#' min_volume, max_volume, increment, name
sort_pipettes <- function(df, groups = NULL) {
  if (is.null(df) || nrow(df) == 0) return(empty_pipettes_df())
  if (!"group" %in% names(df)) df$group <- DEFAULT_PIPETTE_GROUP
  if (is.null(groups)) groups <- unique(df$group)
  group_rank <- match(df$group, unique(c(groups, df$group)))
  df <- df[order(group_rank, df$min_volume, df$max_volume, df$increment,
                 tolower(df$name)), , drop = FALSE]
  rownames(df) <- NULL
  df
}

#' Coerce a raw table to the pipette schema
#' (missing `group` -> DEFAULT_PIPETTE_GROUP, missing `enabled` -> TRUE)
clean_pipettes_df <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!all(c("name", "min_volume", "max_volume", "increment") %in% names(df))) {
    return(NULL)
  }
  if (!"enabled" %in% names(df)) df$enabled <- TRUE
  if (!"group" %in% names(df)) df$group <- DEFAULT_PIPETTE_GROUP
  df$group <- trimws(as.character(df$group))
  df$group[is.na(df$group) | !nzchar(df$group)] <- DEFAULT_PIPETTE_GROUP
  df$name       <- trimws(as.character(df$name))
  df$min_volume <- as.numeric(df$min_volume)
  df$max_volume <- as.numeric(df$max_volume)
  df$increment  <- as.numeric(df$increment)
  en <- df$enabled
  if (!is.logical(en)) en <- toupper(trimws(as.character(en))) %in% c("TRUE", "T", "1", "YES")
  en[is.na(en)] <- TRUE
  df$enabled <- en
  # A range must be positive and non-empty (same rules as the Add Entry form)
  ok <- !is.na(df$name) & nzchar(df$name) &
        is.finite(df$min_volume) & is.finite(df$max_volume) &
        is.finite(df$increment) & df$increment > 0 &
        df$min_volume > 0 & df$max_volume > df$min_volume
  ok[is.na(ok)] <- FALSE
  df[ok, PIPETTE_COLUMNS, drop = FALSE]
}

#' Load the pipette database; falls back to the built-in set.
#' Sheet "Pipettes" holds the pipettes, sheet "Groups" the group names in
#' creation order (also keeps empty groups).
#' @return list(data, groups, sheet_name)
load_pipettes_db <- function(data_dir = "data") {
  xlsx_path <- db_file_path(data_dir, "pipettes")
  df <- NULL
  groups <- character(0)
  if (file.exists(xlsx_path) && requireNamespace("readxl", quietly = TRUE)) {
    sheets <- tryCatch(readxl::excel_sheets(xlsx_path), error = function(e) character(0))
    pip_sheet <- if ("Pipettes" %in% sheets) "Pipettes" else 1
    df <- tryCatch(clean_pipettes_df(readxl::read_excel(xlsx_path, sheet = pip_sheet)),
                   error = function(e) {
                     message("Could not read pipettes file ", xlsx_path, ": ", e$message)
                     NULL
                   })
    if ("Groups" %in% sheets) {
      g <- tryCatch(readxl::read_excel(xlsx_path, sheet = "Groups")$group,
                    error = function(e) NULL)
      g <- trimws(as.character(g %||% character(0)))
      groups <- g[!is.na(g) & nzchar(g)]
    }
  }
  if (is.null(df)) df <- get_builtin_pipettes()
  groups <- unique(c(groups, df$group))
  if (length(groups) == 0) groups <- DEFAULT_PIPETTE_GROUP
  list(data = sort_pipettes(df, groups), groups = groups, sheet_name = "Pipettes")
}

#' Save the pipette database (sorted) to data/private/pipettes.xlsx
save_pipettes_db <- function(pipettes_db, data_dir = "data") {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required to save the pipettes database.")
  }
  path <- db_file_path(data_dir, "pipettes")
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  groups <- pipettes_db$groups %||% unique(pipettes_db$data$group)
  df <- sort_pipettes(pipettes_db$data, groups)
  openxlsx::write.xlsx(
    list(Pipettes = df,
         Groups   = data.frame(group = groups, stringsAsFactors = FALSE)),
    path, overwrite = TRUE)
}

#' All pipettes of one group (sorted)
group_pipettes <- function(pipettes_db, group) {
  df <- pipettes_db$data
  if (is.null(df) || nrow(df) == 0 || is.null(group)) return(empty_pipettes_df())
  sort_pipettes(df[df$group == group, , drop = FALSE])
}

#' Enabled pipettes, optionally of one group only (sorted)
enabled_pipettes <- function(pipettes_db, group = NULL) {
  df <- pipettes_db$data
  if (is.null(df) || nrow(df) == 0) return(empty_pipettes_df())
  keep <- df$enabled
  if (!is.null(group)) keep <- keep & df$group == group
  sort_pipettes(df[keep, , drop = FALSE])
}

#' Human-readable pipette label: "P20 (2–20 µl, step 0.02 µl)"
pipette_label <- function(name, min_volume, max_volume, increment) {
  paste0(name, " (", num_plain(min_volume), "\u2013", num_plain(max_volume),
         " \u00b5l, step ", num_plain(increment), " \u00b5l)")
}

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

#' Round a volume (µl) to a multiple of the pipette increment (µl).
#' Exact halves round up; the small tolerance absorbs floating-point noise so
#' e.g. 96.3 µl with a 0.2 µl increment always gives 96.4 µl.
round_to_increment <- function(vol_ul, increment) {
  round(floor(vol_ul / increment + 0.5 + 1e-9) * increment, 10)
}

#' Format a volume rounded to a pipette increment, in the auto-chosen unit.
#' Decimals follow the increment; trailing zeros are dropped (app convention).
format_volume_increment <- function(value_L, increment_ul) {
  rounded_ul <- round_to_increment(L_to_volume(value_L, "\u00b5l"), increment_ul)
  rounded_L  <- volume_to_L(rounded_ul, "\u00b5l")
  unit <- best_volume_unit(rounded_L)
  shown <- L_to_volume(rounded_L, unit)
  inc_in_unit <- L_to_volume(volume_to_L(increment_ul, "\u00b5l"), unit)
  digits <- decimals_needed(inc_in_unit)
  txt <- formatC(shown, format = "f", digits = digits)
  if (digits > 0) txt <- sub("\\.?0+$", "", txt)
  paste0(txt, " ", unit)
}

#' Decimal places needed to write `x` exactly (0.02 -> 2, 0.25 -> 2, 2.5 -> 1,
#' 20 -> 0). Counting only the order of magnitude would truncate increments
#' such as 0.25 or 2.5 and display a volume the pipette cannot be set to.
decimals_needed <- function(x, max_digits = 9) {
  for (d in 0:max_digits) {
    if (abs(round(x, d) - x) <= 1e-9 * abs(x)) return(d)
  }
  max_digits
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
    vol_ul <- L_to_volume(value_L, "\u00b5l")
    sel <- select_pipette(vol_ul, precision$pipettes)
    if (is.null(sel)) {
      return(list(text = format_volume(value_L), pipette = NULL,
                  warning = "No pipettes are enabled in the selected group: volumes shown with 3 significant figures."))
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
  pip_mode <- is_pipette_mode(precision)
  list(mode        = precision$mode %||% "manual",
       stock_sig   = precision$stock_sig %||% NA_real_,
       diluent_sig = precision$diluent_sig %||% NA_real_,
       group       = if (pip_mode) precision$group %||% NA_character_ else NA_character_,
       pipettes    = if (pip_mode && !is.null(precision$pipettes))
                       paste(precision$pipettes$name, collapse = ", ") else NA_character_)
}
