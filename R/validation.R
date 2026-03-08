# ==============================================================================
# validation.R
# Input validation and warning generation
# ==============================================================================

#' Validate a positive numeric input
validate_positive <- function(value, field_name) {
  if (is.null(value) || is.na(value)) {
    return(list(ok = FALSE, msg = paste(field_name, "is required.")))
  }
  if (!is.numeric(value)) {
    return(list(ok = FALSE, msg = paste(field_name, "must be a number.")))
  }
  if (value <= 0) {
    return(list(ok = FALSE, msg = paste(field_name, "must be greater than zero.")))
  }
  list(ok = TRUE, msg = NULL)
}

#' Validate that stock > target concentration (same internal units)
validate_dilution_direction <- function(stock_internal, target_internal) {
  if (stock_internal <= target_internal) {
    return(list(ok = FALSE,
                msg = "Stock concentration must be greater than target concentration (dilution is impossible)."))
  }
  list(ok = TRUE, msg = NULL)
}

#' Validate unit compatibility
validate_unit_compatibility <- function(unit1, unit2) {
  if (!are_compatible_units(unit1, unit2)) {
    type1 <- get_conc_type(unit1)
    type2 <- get_conc_type(unit2)
    return(list(ok = FALSE,
                msg = paste0("Incompatible units: ", unit1, " (", type1, ") vs ",
                             unit2, " (", type2, "). ",
                             "Use the MW-Based Preparation tab for mass \u2194 molar conversion.")))
  }
  list(ok = TRUE, msg = NULL)
}

#' Generate pipetting warnings for a volume (in L)
pipetting_warnings <- function(volume_L) {
  vol_ul <- L_to_volume(volume_L, "\u00b5l")
  warnings <- character(0)

  if (vol_ul < 0.5) {
    warnings <- c(warnings,
      paste0("Extremely small volume: ", sig3(vol_ul),
             " \u00b5l. This is below typical pipette accuracy. Consider using a multi-step dilution."))
  } else if (vol_ul < 1) {
    warnings <- c(warnings,
      paste0("Very small volume: ", sig3(vol_ul),
             " \u00b5l. Use a calibrated pipette and verify accuracy."))
  } else if (vol_ul < 2) {
    warnings <- c(warnings,
      paste0("Small volume: ", sig3(vol_ul),
             " \u00b5l. Ensure precise pipetting."))
  }

  warnings
}

#' Validate all inputs for single dilution
validate_single_dilution <- function(stock_val, stock_unit, target_val, target_unit,
                                     final_vol, final_vol_unit) {
  errors <- character(0)
  warnings <- character(0)

  # Positive values
  for (check in list(
    validate_positive(stock_val, "Stock concentration"),
    validate_positive(target_val, "Target concentration"),
    validate_positive(final_vol, "Final volume")
  )) {
    if (!check$ok) errors <- c(errors, check$msg)
  }

  if (length(errors) > 0) return(list(ok = FALSE, errors = errors, warnings = warnings))

  # Unit compatibility
  compat <- validate_unit_compatibility(stock_unit, target_unit)
  if (!compat$ok) {
    errors <- c(errors, compat$msg)
    return(list(ok = FALSE, errors = errors, warnings = warnings))
  }

  # Dilution direction
  stock_norm <- normalize_concentration(stock_val, stock_unit)
  target_norm <- normalize_concentration(target_val, target_unit)
  dir_check <- validate_dilution_direction(stock_norm$value, target_norm$value)
  if (!dir_check$ok) errors <- c(errors, dir_check$msg)

  list(ok = length(errors) == 0, errors = errors, warnings = warnings)
}

#' Validate MW value
validate_mw <- function(mw_value) {
  if (is.null(mw_value) || is.na(mw_value)) {
    return(list(ok = FALSE, msg = "Molecular weight is required for this calculation."))
  }
  if (!is.numeric(mw_value) || mw_value <= 0) {
    return(list(ok = FALSE, msg = "Molecular weight must be a positive number (Da or g/mol)."))
  }
  list(ok = TRUE, msg = NULL)
}
