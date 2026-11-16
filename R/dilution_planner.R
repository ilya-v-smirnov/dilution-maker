# ==============================================================================
# dilution_planner.R
# Multi-step dilution planner: splits large DF into sequential steps
# ==============================================================================

#' Plan a multi-step dilution
#'
#' @param stock_val Stock concentration value
#' @param stock_unit Stock concentration unit
#' @param target_val Target concentration value
#' @param target_unit Target concentration unit
#' @param final_vol Final volume value
#' @param final_vol_unit Final volume unit
#' @param diluent Diluent name
#' @param solution_name Optional label
#' @param max_df_per_step Maximum dilution factor per step (default 500)
#' @param min_pipette_vol_ul Minimum pipetting volume in µl (default 0.5)
#' @param preferred_intermediate_vol_ul Preferred intermediate volume in µl (default 500)
#' @param precision Volume precision settings (see format_volume_precise());
#'   NULL = manual mode, 3 significant figures
#' @return Structured result object with multiple steps
plan_multistep_dilution <- function(stock_val, stock_unit,
                                    target_val, target_unit,
                                    final_vol, final_vol_unit,
                                    diluent = "PBS",
                                    solution_name = "",
                                    max_df_per_step = 500,
                                    min_pipette_vol_ul = 0.5,
                                    preferred_intermediate_vol_ul = 500,
                                    precision = NULL) {
  if (is.null(precision)) precision <- default_precision()
  pipette_mode <- is_pipette_mode(precision)

  # Normalize

  stock_norm <- normalize_concentration(stock_val, stock_unit)
  target_norm <- normalize_concentration(target_val, target_unit)
  vol_norm <- normalize_volume(final_vol, final_vol_unit)

  if (stock_norm$type != target_norm$type) {
    stop("Stock and target concentrations must both be mass or both be molar.")
  }
  if (!is.numeric(max_df_per_step) || !is.finite(max_df_per_step) || max_df_per_step <= 1) {
    stop("Max DF per step must be greater than 1.")
  }

  conc_type <- stock_norm$type
  total_df <- stock_norm$value / target_norm$value
  final_vol_L <- vol_norm$value

  if (!is.finite(total_df) || total_df <= 1) {
    stop("Stock concentration must be greater than target concentration.")
  }

  # Determine number of steps needed (the small tolerance guards against
  # floating-point error when total_df is an exact power of max_df_per_step)
  if (total_df <= max_df_per_step) {
    n_steps <- 1
  } else {
    n_steps <- ceiling(log(total_df) / log(max_df_per_step) - 1e-9)
  }

  intermediate_vol_L <- volume_to_L(preferred_intermediate_vol_ul, "\u00b5l")
  min_pipette_vol_L <- volume_to_L(min_pipette_vol_ul, "\u00b5l")

  # Volume prepared in each step. Working backwards from the final volume,
  # an intermediate must hold at least what the next step takes from it;
  # if the preferred volume does not cover that with a 10% reserve (a tube
  # cannot be emptied to the last drop), it is enlarged.
  plan_step_volumes <- function(n, df_step) {
    vols <- numeric(n)
    vols[n] <- final_vol_L
    for (i in rev(seq_len(n - 1))) {
      needed_L <- vols[i + 1] / df_step
      vols[i] <- if (needed_L * 1.1 > intermediate_vol_L * (1 + 1e-9)) {
        ceiling_signif(needed_L * 1.1, 2)
      } else {
        intermediate_vol_L
      }
    }
    vols
  }

  # Add steps (smaller DF per step -> larger transfer volumes) while any
  # transfer is below the minimum pipetting volume
  max_steps <- 10
  repeat {
    df_per_step <- total_df^(1 / n_steps)
    step_vols_L <- plan_step_volumes(n_steps, df_per_step)
    transfer_ok <- all(step_vols_L / df_per_step >= min_pipette_vol_L * (1 - 1e-9))
    if (transfer_ok || n_steps >= max_steps || final_vol_L < min_pipette_vol_L) break
    n_steps <- n_steps + 1
  }

  # Build steps
  steps <- list()
  warnings <- character(0)
  current_conc <- stock_norm$value

  for (i in seq_len(n_steps)) {
    is_last <- (i == n_steps)
    step_target_conc <- current_conc / df_per_step
    step_final_vol_L <- step_vols_L[i]

    if (!is_last && step_final_vol_L > intermediate_vol_L) {
      warnings <- c(warnings,
        paste0("Intermediate ", i, " volume increased to ", format_volume(step_final_vol_L),
               " (preferred ", format_volume(intermediate_vol_L),
               ") to supply step ", i + 1, "."))
    }

    # Calculate volumes (use approximation if DF > 100)
    stock_vol_L <- step_final_vol_L / df_per_step
    if (df_per_step > 100) {
      diluent_vol_L <- step_final_vol_L
    } else {
      diluent_vol_L <- step_final_vol_L - stock_vol_L
    }

    # Displayed (precision-rounded) volumes
    stock_fv   <- format_volume_precise(stock_vol_L, precision, "stock")
    diluent_fv <- format_volume_precise(diluent_vol_L, precision, "diluent")

    # Pipetting warnings: pipette-range warnings in pipette mode
    # (min pipetting volume then equals the smallest pipette's minimum),
    # generic thresholds otherwise
    if (pipette_mode) {
      step_warnings <- unique(c(stock_fv$warning, diluent_fv$warning))
    } else {
      step_warnings <- pipetting_warnings(stock_vol_L)
    }
    if (!pipette_mode && stock_vol_L < min_pipette_vol_L) {
      step_warnings <- c(step_warnings,
        paste0("Transfer volume ", format_volume(stock_vol_L),
               " is below the minimum pipetting volume (",
               sig3(min_pipette_vol_ul), " \u00b5l)."))
    }
    if (length(step_warnings) > 0) {
      step_warnings <- paste0("Step ", i, ": ", step_warnings)
      warnings <- c(warnings, step_warnings)
    }

    # Source label
    if (i == 1) {
      source_label <- "stock"
    } else {
      source_label <- paste0("intermediate ", i - 1)
    }

    steps[[i]] <- list(
      step = i,
      source_label = source_label,
      source_conc_internal = current_conc,
      result_conc_internal = step_target_conc,
      stock_volume_L = stock_vol_L,
      diluent_volume_L = diluent_vol_L,
      stock_volume_fmt = stock_fv$text,
      diluent_volume_fmt = diluent_fv$text,
      stock_pipette = stock_fv$pipette,
      diluent_pipette = diluent_fv$pipette,
      total_volume_L = step_final_vol_L,
      dilution_factor = df_per_step
    )

    current_conc <- step_target_conc
  }

  # Build instruction text (plain and with pipette names)
  label <- if (nzchar(solution_name)) solution_name else "solution"
  target_conc_fmt <- paste0(num_plain(target_val), " ", target_unit)   # as entered

  build_instruction <- function(show_pipettes) {
    lines <- c(
      paste0(target_conc_fmt, " ", label, " in ", diluent),
      paste0("Total DF: ", format_df(total_df), " in ", n_steps, " step(s)"),
      ""
    )
    for (s in steps) {
      stock_fv   <- list(text = s$stock_volume_fmt, pipette = s$stock_pipette)
      diluent_fv <- list(text = s$diluent_volume_fmt, pipette = s$diluent_pipette)
      stock_vol_fmt   <- if (show_pipettes) volume_with_pipette(stock_fv) else stock_fv$text
      diluent_vol_fmt <- if (show_pipettes) volume_with_pipette(diluent_fv) else diluent_fv$text
      step_conc_fmt <- format_concentration(s$result_conc_internal, conc_type)

      lines <- c(lines,
        paste0("Step ", s$step),
        paste0("  ", stock_vol_fmt, " ", s$source_label, " + ",
               diluent_vol_fmt, " ", diluent,
               " | DF: ", format_df(s$dilution_factor)),
        paste0("  \u2192 ", step_conc_fmt),
        ""
      )
    }
    paste(lines, collapse = "\n")
  }

  instruction_text <- build_instruction(FALSE)
  instruction_text_pipettes <- build_instruction(TRUE)

  list(
    scenario_type = "multistep_dilution",
    solution_name = solution_name,
    instruction_text = instruction_text,
    instruction_text_pipettes = instruction_text_pipettes,
    input_parameters = list(
      stock_concentration = stock_val,
      stock_unit = stock_unit,
      target_concentration = target_val,
      target_unit = target_unit,
      final_volume = final_vol,
      final_volume_unit = final_vol_unit,
      diluent = diluent,
      max_df_per_step = max_df_per_step,
      min_pipette_vol_ul = min_pipette_vol_ul,
      preferred_intermediate_vol_ul = preferred_intermediate_vol_ul,
      precision_mode = precision$mode
    ),
    normalized_parameters = list(
      stock_conc_internal = stock_norm$value,
      target_conc_internal = target_norm$value,
      final_vol_L = final_vol_L,
      conc_type = conc_type,
      internal_conc_unit = stock_norm$internal_unit
    ),
    steps = steps,
    warnings = warnings,
    technical_metadata = list(
      total_dilution_factor = total_df,
      n_steps = n_steps,
      df_per_step = df_per_step,
      precision = precision_summary(precision),
      timestamp = Sys.time()
    )
  )
}

#' Round a positive number up to `digits` significant figures
ceiling_signif <- function(x, digits = 2) {
  magnitude <- 10^(floor(log10(x)) - digits + 1)
  ceiling(x / magnitude) * magnitude
}
