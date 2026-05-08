# ==============================================================================
# molarity_calculations.R
# MW-based concentration conversion and dry substance preparation
# ==============================================================================

#' Convert between mass and molar concentrations using MW
#'
#' @param value Concentration value
#' @param from_unit Source unit
#' @param to_unit Target unit
#' @param mw Molecular weight in Da (g/mol)
#' @return List with converted value and unit
convert_with_mw <- function(value, from_unit, to_unit, mw) {
  from_type <- get_conc_type(from_unit)
  to_type <- get_conc_type(to_unit)

  if (from_type == to_type) {
    # Same type: simple unit conversion
    if (from_type == "mass") {
      internal <- mass_conc_to_gL(value, from_unit)
      converted <- gL_to_mass_conc(internal, to_unit)
    } else {
      internal <- molar_conc_to_molL(value, from_unit)
      converted <- molL_to_molar_conc(internal, to_unit)
    }
    return(list(value = converted, unit = to_unit))
  }

  # Cross-type conversion requires MW
  if (from_type == "mass" && to_type == "molar") {
    # mass → molar: (g/L) / (g/mol) = mol/L
    gL <- mass_conc_to_gL(value, from_unit)
    molL <- gL / mw
    converted <- molL_to_molar_conc(molL, to_unit)
  } else if (from_type == "molar" && to_type == "mass") {
    # molar → mass: (mol/L) * (g/mol) = g/L
    molL <- molar_conc_to_molL(value, from_unit)
    gL <- molL * mw
    converted <- gL_to_mass_conc(gL, to_unit)
  } else {
    stop("Unsupported conversion direction")
  }

  list(value = converted, unit = to_unit)
}

#' MW-based preparation: convert stock conc, then dilute
#'
#' @param substance Substance name
#' @param mw Molecular weight
#' @param mw_source How MW was obtained ("database", "manual")
#' @param stock_val Stock concentration value
#' @param stock_unit Stock concentration unit
#' @param target_val Target concentration value
#' @param target_unit Target concentration unit
#' @param final_vol Final volume value
#' @param final_vol_unit Final volume unit
#' @param diluent Diluent name
#' @param stock_precision Significant figures for stock volume display (default 3)
#' @param diluent_precision Significant figures for diluent volume display (default 3)
#' @return Structured result object
calculate_mw_based_preparation <- function(substance, mw, mw_source,
                                           stock_val, stock_unit,
                                           target_val, target_unit,
                                           final_vol, final_vol_unit,
                                           diluent = "PBS",
                                           stock_precision = 3,
                                           diluent_precision = 3) {

  stock_type <- get_conc_type(stock_unit)
  target_type <- get_conc_type(target_unit)

  # Convert stock to same type as target for dilution calc
  if (stock_type != target_type) {
    converted <- convert_with_mw(stock_val, stock_unit, target_unit, mw)
    effective_stock_val <- converted$value
    effective_stock_unit <- converted$unit
  } else {
    effective_stock_val <- stock_val
    effective_stock_unit <- stock_unit
  }

  # Now do a standard dilution
  result <- calculate_single_dilution(
    stock_val = effective_stock_val,
    stock_unit = effective_stock_unit,
    target_val = target_val,
    target_unit = target_unit,
    final_vol = final_vol,
    final_vol_unit = final_vol_unit,
    diluent = diluent,
    solution_name = substance,
    use_approximation = TRUE
  )

  # Override scenario type and add MW info
  result$scenario_type <- "mw_based_preparation"

  # Rebuild instruction text with structured format
  s <- result$steps[[1]]
  stock_vol_fmt <- format_volume_sig(s$stock_volume_L, stock_precision)
  diluent_vol_fmt <- format_volume_sig(s$diluent_volume_L, diluent_precision)
  target_conc_fmt <- format_conc_in_unit(result$normalized_parameters$target_conc_internal, target_unit)
  original_stock_str <- paste0(sig3(stock_val), " ", stock_unit)

  if (stock_type != target_type) {
    converted_stock_str <- paste0(sig3(effective_stock_val), " ", effective_stock_unit)
    result$instruction_text <- paste0(
      substance, "\n",
      "Stock: ", original_stock_str, " = ", converted_stock_str,
      " (MW: ", sig3(mw), " Da)",
      " \u2192 ", target_conc_fmt, " in ", diluent, "\n",
      stock_vol_fmt, " stock + ", diluent_vol_fmt, " ", diluent,
      " | DF: ", format_df(s$dilution_factor)
    )
  } else {
    result$instruction_text <- paste0(
      substance, "\n",
      "Stock: ", original_stock_str,
      " \u2192 ", target_conc_fmt, " in ", diluent, "\n",
      stock_vol_fmt, " stock + ", diluent_vol_fmt, " ", diluent,
      " | DF: ", format_df(s$dilution_factor)
    )
  }

  # Add MW metadata
  result$input_parameters$substance <- substance
  result$input_parameters$mw <- mw
  result$input_parameters$mw_source <- mw_source
  result$input_parameters$original_stock_val <- stock_val
  result$input_parameters$original_stock_unit <- stock_unit
  result$input_parameters$stock_precision <- stock_precision
  result$input_parameters$diluent_precision <- diluent_precision
  result$technical_metadata$mw <- mw
  result$technical_metadata$mw_source <- mw_source
  result$technical_metadata$stock_precision <- stock_precision
  result$technical_metadata$diluent_precision <- diluent_precision

  result
}

#' Calculate mass needed to prepare solution from dry substance
#'
#' @param substance Substance name
#' @param mw Molecular weight (Da)
#' @param target_val Target concentration value
#' @param target_unit Target concentration unit
#' @param final_vol Final volume value
#' @param final_vol_unit Final volume unit
#' @param solvent Solvent name
#' @return Structured result object
calculate_dry_substance <- function(substance, mw,
                                    target_val, target_unit,
                                    final_vol, final_vol_unit,
                                    solvent = "Water") {

  target_type <- get_conc_type(target_unit)
  vol_L <- volume_to_L(final_vol, final_vol_unit)

  # Calculate required mass in grams
  if (target_type == "molar") {
    # mol/L * L * g/mol = g
    molL <- molar_conc_to_molL(target_val, target_unit)
    mass_g <- molL * vol_L * mw
    target_internal <- molL
  } else if (target_type == "mass") {
    # g/L * L = g
    gL <- mass_conc_to_gL(target_val, target_unit)
    mass_g <- gL * vol_L
    target_internal <- gL
  } else {
    stop("Unsupported concentration type for dry substance preparation")
  }

  # Format instruction
  mass_fmt <- format_mass(mass_g)
  vol_fmt <- format_volume(vol_L)
  target_fmt <- paste0(sig3(target_val), " ", target_unit)

  instruction <- paste0(
    target_fmt, " ", substance, " in ", solvent, "\n",
    "Weigh ", mass_fmt, " ", substance, "\n",
    "Dissolve in ", vol_fmt, " ", solvent
  )

  warnings <- character(0)
  if (mass_g < 1e-6) {
    warnings <- c(warnings,
      "Extremely small mass required. Consider preparing a more concentrated stock first.")
  } else if (mass_g < 1e-3) {
    warnings <- c(warnings,
      "Very small mass. Use an analytical balance with adequate precision.")
  }

  list(
    scenario_type = "dry_substance",
    solution_name = substance,
    instruction_text = instruction,
    input_parameters = list(
      substance = substance,
      mw = mw,
      target_concentration = target_val,
      target_unit = target_unit,
      final_volume = final_vol,
      final_volume_unit = final_vol_unit,
      solvent = solvent
    ),
    normalized_parameters = list(
      target_conc_internal = target_internal,
      conc_type = target_type,
      final_vol_L = vol_L,
      mass_g = mass_g
    ),
    steps = list(
      list(
        step = 1,
        action = "weigh",
        mass_g = mass_g,
        volume_L = vol_L,
        solvent = solvent
      )
    ),
    warnings = warnings,
    technical_metadata = list(
      mw = mw,
      mass_g = mass_g,
      timestamp = Sys.time()
    )
  )
}
