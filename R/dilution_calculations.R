# ==============================================================================
# dilution_calculations.R
# Single-step dilution: C1V1 = C2V2
# ==============================================================================

#' Calculate single-step dilution
#'
#' @param stock_val Numeric stock concentration value
#' @param stock_unit Unit string for stock
#' @param target_val Numeric target concentration value
#' @param target_unit Unit string for target
#' @param final_vol Numeric final volume value
#' @param final_vol_unit Unit string for volume
#' @param diluent Character name of diluent (e.g. "PBS")
#' @param solution_name Optional name for the solution
#' @param use_approximation Logical; if TRUE, use approximation when DF > 100
#' @param precision Volume precision settings (see format_volume_precise());
#'   NULL = manual mode, 3 significant figures
#' @return Structured result object
calculate_single_dilution <- function(stock_val, stock_unit,
                                      target_val, target_unit,
                                      final_vol, final_vol_unit,
                                      diluent = "PBS",
                                      solution_name = "",
                                      use_approximation = TRUE,
                                      precision = NULL) {
  if (is.null(precision)) precision <- default_precision()

  # Normalize to internal units
  stock_norm <- normalize_concentration(stock_val, stock_unit)
  target_norm <- normalize_concentration(target_val, target_unit)
  vol_norm <- normalize_volume(final_vol, final_vol_unit)

  conc_type <- stock_norm$type  # mass or molar

  # Dilution factor
  df <- stock_norm$value / target_norm$value

  # Volumes
  final_vol_L <- vol_norm$value

  if (use_approximation && df > 100) {
    # Approximation: ignore stock volume in final volume
    stock_vol_L <- final_vol_L / df
    diluent_vol_L <- final_vol_L
    approximation_used <- TRUE
  } else {
    # Exact: V_stock = V_final * C_target / C_stock
    stock_vol_L <- final_vol_L * target_norm$value / stock_norm$value
    diluent_vol_L <- final_vol_L - stock_vol_L
    approximation_used <- FALSE
  }

  # Displayed (precision-rounded) volumes
  stock_fv   <- format_volume_precise(stock_vol_L, precision, "stock")
  diluent_fv <- format_volume_precise(diluent_vol_L, precision, "diluent")

  # Warnings: pipette-range warnings in pipette mode, generic ones otherwise
  warnings <- character(0)
  if (is_pipette_mode(precision)) {
    warnings <- c(warnings, unique(c(stock_fv$warning, diluent_fv$warning)))
  } else {
    warnings <- c(warnings, pipetting_warnings(stock_vol_L))
  }
  if (df > 1000) {
    warnings <- c(warnings, "Very large dilution factor. Consider using the Multi-Step Planner.")
  }
  if (approximation_used) {
    warnings <- c(warnings,
      paste0("Approximation used (DF = ", format_df(df),
             " > 100x): stock volume is negligible relative to final volume."))
  }
  # Negative diluent volume → impossible dilution (stock < target)
  if (diluent_vol_L < 0) {
    warnings <- c(warnings,
      paste0("Negative diluent volume (", format_volume(diluent_vol_L),
             "): stock concentration is lower than target. ",
             "Dilution is impossible \u2014 a more concentrated stock is required."))
  }

  # Build instruction text (plain and with pipette names)
  target_conc_fmt <- format_conc_in_unit(target_norm$value, target_unit)

  label <- if (nzchar(solution_name)) solution_name else "solution"

  build_instruction <- function(stock_vol_fmt, diluent_vol_fmt) {
    paste0(
      target_conc_fmt, " ", label, " in ", diluent, "\n",
      stock_vol_fmt, " stock + ", diluent_vol_fmt, " ", diluent,
      " | DF: ", format_df(df)
    )
  }
  instruction <- build_instruction(stock_fv$text, diluent_fv$text)
  instruction_pip <- build_instruction(volume_with_pipette(stock_fv),
                                       volume_with_pipette(diluent_fv))

  # Result object (spec section 15)
  list(
    scenario_type = "single_dilution",
    solution_name = solution_name,
    instruction_text = instruction,
    instruction_text_pipettes = instruction_pip,
    input_parameters = list(
      stock_concentration = stock_val,
      stock_unit = stock_unit,
      target_concentration = target_val,
      target_unit = target_unit,
      final_volume = final_vol,
      final_volume_unit = final_vol_unit,
      diluent = diluent,
      precision_mode = precision$mode
    ),
    normalized_parameters = list(
      stock_conc_internal = stock_norm$value,
      target_conc_internal = target_norm$value,
      final_vol_L = final_vol_L,
      conc_type = conc_type,
      internal_conc_unit = stock_norm$internal_unit
    ),
    steps = list(
      list(
        step = 1,
        stock_volume_L = stock_vol_L,
        diluent_volume_L = diluent_vol_L,
        stock_volume_fmt = stock_fv$text,
        diluent_volume_fmt = diluent_fv$text,
        stock_pipette = stock_fv$pipette,
        diluent_pipette = diluent_fv$pipette,
        dilution_factor = df,
        result_concentration_internal = target_norm$value
      )
    ),
    warnings = warnings,
    technical_metadata = list(
      approximation_used = approximation_used,
      dilution_factor = df,
      precision = precision_summary(precision),
      timestamp = Sys.time()
    )
  )
}
