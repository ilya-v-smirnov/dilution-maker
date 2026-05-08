# ==============================================================================
# formatting.R
# Display formatting: 3 significant figures, volume/concentration display
# ==============================================================================

#' Format a number to 3 significant figures (no trailing zeros)
sig3 <- function(x) {
  if (is.na(x) || is.null(x) || !is.finite(x)) return("NA")
  if (x == 0) return("0")
  s <- signif(x, 3)
  trimws(format(s, scientific = FALSE))
}

#' Format a number to N significant figures (no trailing zeros)
sig_n <- function(x, n = 3) {
  if (is.na(x) || is.null(x) || !is.finite(x)) return("NA")
  if (x == 0) return("0")
  if (is.null(n) || is.na(n) || !is.numeric(n) || n < 1) n <- 3
  n <- as.integer(n)
  s <- signif(x, n)
  trimws(format(s, scientific = FALSE))
}

#' Format volume with auto-chosen unit (from internal L)
format_volume <- function(value_L) {
  unit <- best_volume_unit(value_L)
  display_val <- L_to_volume(value_L, unit)
  paste0(sig3(display_val), " ", unit)
}

#' Format volume with auto-chosen unit and configurable significant figures
format_volume_sig <- function(value_L, sig_digits = 3) {
  unit <- best_volume_unit(value_L)
  display_val <- L_to_volume(value_L, unit)
  paste0(sig_n(display_val, sig_digits), " ", unit)
}

#' Format mass concentration with auto-chosen unit (from internal g/L)
format_mass_conc <- function(value_gL) {
  unit <- best_mass_conc_unit(value_gL)
  display_val <- gL_to_mass_conc(value_gL, unit)
  paste0(sig3(display_val), " ", unit)
}

#' Format molar concentration with auto-chosen unit (from internal mol/L)
format_molar_conc <- function(value_molL) {
  unit <- best_molar_conc_unit(value_molL)
  display_val <- molL_to_molar_conc(value_molL, unit)
  paste0(sig3(display_val), " ", unit)
}

#' Format concentration generically (auto-detect mass vs molar)
format_concentration <- function(value_internal, conc_type) {
  if (conc_type == "mass") {
    format_mass_conc(value_internal)
  } else if (conc_type == "molar") {
    format_molar_conc(value_internal)
  } else {
    paste0(sig3(value_internal), " [unknown]")
  }
}

#' Format volume in a specific user-requested unit
format_volume_in_unit <- function(value_L, unit) {
  display_val <- L_to_volume(value_L, unit)
  paste0(sig3(display_val), " ", unit)
}

#' Format concentration in a specific user-requested unit
format_conc_in_unit <- function(value_internal, unit) {
  if (is_mass_conc(unit)) {
    display_val <- gL_to_mass_conc(value_internal, unit)
  } else if (is_molar_conc(unit)) {
    display_val <- molL_to_molar_conc(value_internal, unit)
  } else {
    return(paste0(sig3(value_internal), " ", unit))
  }
  paste0(sig3(display_val), " ", unit)
}

#' Format dilution factor
format_df <- function(df_value) {
  paste0(sig3(df_value), "x")
}

#' Format mass (in grams) with auto unit
format_mass <- function(value_g) {
  abs_val <- abs(value_g)
  if (abs_val < 1e-6) {
    paste0(sig3(value_g * 1e9), " ng")
  } else if (abs_val < 1e-3) {
    paste0(sig3(value_g * 1e6), " \u00b5g")
  } else if (abs_val < 1) {
    paste0(sig3(value_g * 1e3), " mg")
  } else {
    paste0(sig3(value_g), " g")
  }
}
