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
#' @param precision Volume precision settings (see format_volume_precise());
#'   when NULL, manual mode with stock_precision / diluent_precision is used
#' @return Structured result object
calculate_mw_based_preparation <- function(substance, mw, mw_source,
                                           stock_val, stock_unit,
                                           target_val, target_unit,
                                           final_vol, final_vol_unit,
                                           diluent = "PBS",
                                           stock_precision = 3,
                                           diluent_precision = 3,
                                           precision = NULL) {
  if (is.null(precision)) {
    precision <- list(mode = "manual", stock_sig = stock_precision,
                      diluent_sig = diluent_precision)
  }

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
    use_approximation = TRUE,
    precision = precision
  )

  # Override scenario type and add MW info
  result$scenario_type <- "mw_based_preparation"

  # Rebuild instruction text with structured format (plain and with pipettes)
  s <- result$steps[[1]]
  target_conc_fmt <- format_conc_in_unit(result$normalized_parameters$target_conc_internal, target_unit)
  original_stock_str <- paste0(sig3(stock_val), " ", stock_unit)
  label <- if (nzchar(substance)) substance else "substance"

  stock_line <- if (stock_type != target_type) {
    converted_stock_str <- paste0(sig3(effective_stock_val), " ", effective_stock_unit)
    paste0("Stock: ", original_stock_str, " = ", converted_stock_str,
           " (MW: ", sig3(mw), " Da)")
  } else {
    paste0("Stock: ", original_stock_str)
  }

  build_instruction <- function(stock_vol_fmt, diluent_vol_fmt) {
    paste0(
      label, "\n",
      stock_line, " \u2192 ", target_conc_fmt, " in ", diluent, "\n",
      stock_vol_fmt, " stock + ", diluent_vol_fmt, " ", diluent,
      " | DF: ", format_df(s$dilution_factor)
    )
  }
  with_pip <- function(text, pipette) {
    volume_with_pipette(list(text = text, pipette = pipette))
  }
  result$instruction_text <- build_instruction(s$stock_volume_fmt, s$diluent_volume_fmt)
  result$instruction_text_pipettes <- build_instruction(
    with_pip(s$stock_volume_fmt, s$stock_pipette),
    with_pip(s$diluent_volume_fmt, s$diluent_pipette)
  )

  # Add MW metadata
  result$input_parameters$substance <- substance
  result$input_parameters$mw <- mw
  result$input_parameters$mw_source <- mw_source
  result$input_parameters$original_stock_val <- stock_val
  result$input_parameters$original_stock_unit <- stock_unit
  result$input_parameters$stock_precision <- precision$stock_sig %||% NA_real_
  result$input_parameters$diluent_precision <- precision$diluent_sig %||% NA_real_
  result$technical_metadata$mw <- mw
  result$technical_metadata$mw_source <- mw_source
  result$technical_metadata$stock_precision <- precision$stock_sig %||% NA_real_
  result$technical_metadata$diluent_precision <- precision$diluent_sig %||% NA_real_

  result
}

#' Calculate mass needed to prepare solution from dry substance
#'
#' The weighed mass is corrected for purity: mass = mass_at_100% / (purity/100).
#' A missing purity (NA) is treated as 100%.
#'
#' @param substance Substance name (used in the instruction header)
#' @param mw Molecular weight (Da)
#' @param target_val Target concentration value
#' @param target_unit Target concentration unit
#' @param final_vol Final volume value
#' @param final_vol_unit Final volume unit
#' @param solvent Solvent name
#' @param purity Purity in percent (0 < purity <= 100), NA = 100%
#' @param supplier Supplier name (optional)
#' @param cat Catalogue number (optional)
#' @param label Full display label used in the "Weigh" line
#'   (defaults to substance + supplier + purity)
#' @return Structured result object
calculate_dry_substance <- function(substance, mw,
                                    target_val, target_unit,
                                    final_vol, final_vol_unit,
                                    solvent = "Water",
                                    purity = NA_real_,
                                    supplier = "",
                                    cat = "",
                                    label = NULL) {

  purity <- if (is.null(purity) || length(purity) == 0) NA_real_ else as.numeric(purity)
  supplier <- if (is.null(supplier) || is.na(supplier)) "" else trimws(supplier)
  cat <- if (is.null(cat) || is.na(cat)) "" else trimws(cat)
  purity_err <- validate_purity(purity)
  if (!is.null(purity_err)) stop(purity_err)
  purity_used <- if (is.na(purity)) 100 else purity

  target_type <- get_conc_type(target_unit)
  vol_L <- volume_to_L(final_vol, final_vol_unit)

  # Mass of pure (100%) substance in grams
  if (target_type == "molar") {
    # mol/L * L * g/mol = g
    molL <- molar_conc_to_molL(target_val, target_unit)
    mass_pure_g <- molL * vol_L * mw
    target_internal <- molL
  } else if (target_type == "mass") {
    # g/L * L = g
    gL <- mass_conc_to_gL(target_val, target_unit)
    mass_pure_g <- gL * vol_L
    target_internal <- gL
  } else {
    stop("Unsupported concentration type for dry substance preparation")
  }

  # Purity correction: weigh more of the impure powder
  mass_g <- mass_pure_g / (purity_used / 100)

  # Format instruction
  mass_fmt <- format_mass(mass_g)
  vol_fmt <- format_volume(vol_L)
  target_fmt <- paste0(sig3(target_val), " ", target_unit)
  short_name <- if (nzchar(substance)) substance else "substance"
  if (is.null(label) || !nzchar(label)) {
    label <- if (nzchar(substance)) substance_label(substance, supplier, purity) else "substance"
  }
  purity_note <- if (purity_used < 100) {
    paste0(" (purity-corrected; ", format_mass(mass_pure_g), " at 100%)")
  } else ""

  instruction <- paste0(
    target_fmt, " ", short_name, " in ", solvent, "\n",
    "Weigh ", mass_fmt, " ", label, purity_note, "\n",
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
      substance_label = label,
      supplier = supplier,
      purity = purity,
      cat = cat,
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
      mass_g = mass_g,
      mass_pure_g = mass_pure_g
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
      mass_pure_g = mass_pure_g,
      purity_used = purity_used,
      timestamp = Sys.time()
    )
  )
}
