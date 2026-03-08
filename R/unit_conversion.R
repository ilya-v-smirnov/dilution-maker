# ==============================================================================
# unit_conversion.R
# Converts all user-facing units to internal standard units:
#   Mass concentration → g/L
#   Molar concentration → mol/L
#   Volume → L
# ==============================================================================

# --- Unit classification -----------------------------------------------------

MASS_CONC_UNITS <- c("pg/ml", "ng/ml", "\u00b5g/ml", "mg/ml")
MOLAR_CONC_UNITS <- c("pM", "nM", "\u00b5M", "mM", "M")
VOLUME_UNITS <- c("\u00b5l", "ml", "L")
ALL_CONC_UNITS <- c(MASS_CONC_UNITS, MOLAR_CONC_UNITS)

is_mass_conc <- function(unit) {
  unit %in% MASS_CONC_UNITS
}

is_molar_conc <- function(unit) {
  unit %in% MOLAR_CONC_UNITS
}

get_conc_type <- function(unit) {
  if (is_mass_conc(unit)) return("mass")
  if (is_molar_conc(unit)) return("molar")
  return("unknown")
}

are_compatible_units <- function(unit1, unit2) {
  get_conc_type(unit1) == get_conc_type(unit2)
}

# --- Conversion factors to internal units ------------------------------------

mass_conc_to_gL <- function(value, unit) {
  factor <- switch(unit,
    "pg/ml" = 1e-12 * 1e3,   # pg/ml → g/L: 1e-9
    "ng/ml" = 1e-9 * 1e3,    # ng/ml → g/L: 1e-6
    "\u00b5g/ml" = 1e-6 * 1e3, # µg/ml → g/L: 1e-3
    "mg/ml" = 1e-3 * 1e3,    # mg/ml → g/L: 1
    stop(paste("Unknown mass concentration unit:", unit))
  )
  value * factor
}

gL_to_mass_conc <- function(value_gL, unit) {
  factor <- switch(unit,
    "pg/ml" = 1e-9,
    "ng/ml" = 1e-6,
    "\u00b5g/ml" = 1e-3,
    "mg/ml" = 1,
    stop(paste("Unknown mass concentration unit:", unit))
  )
  value_gL / factor
}

molar_conc_to_molL <- function(value, unit) {
  factor <- switch(unit,
    "pM" = 1e-12,
    "nM" = 1e-9,
    "\u00b5M" = 1e-6,
    "mM" = 1e-3,
    "M"  = 1,
    stop(paste("Unknown molar concentration unit:", unit))
  )
  value * factor
}

molL_to_molar_conc <- function(value_molL, unit) {
  factor <- switch(unit,
    "pM" = 1e-12,
    "nM" = 1e-9,
    "\u00b5M" = 1e-6,
    "mM" = 1e-3,
    "M"  = 1,
    stop(paste("Unknown molar concentration unit:", unit))
  )
  value_molL / factor
}

volume_to_L <- function(value, unit) {
  factor <- switch(unit,
    "\u00b5l" = 1e-6,
    "ml" = 1e-3,
    "L"  = 1,
    stop(paste("Unknown volume unit:", unit))
  )
  value * factor
}

L_to_volume <- function(value_L, unit) {
  factor <- switch(unit,
    "\u00b5l" = 1e-6,
    "ml" = 1e-3,
    "L"  = 1,
    stop(paste("Unknown volume unit:", unit))
  )
  value_L / factor
}

# --- Generic normalizer ------------------------------------------------------

normalize_concentration <- function(value, unit) {
  if (is_mass_conc(unit)) {
    list(value = mass_conc_to_gL(value, unit), internal_unit = "g/L", type = "mass")
  } else if (is_molar_conc(unit)) {
    list(value = molar_conc_to_molL(value, unit), internal_unit = "mol/L", type = "molar")
  } else {
    stop(paste("Cannot normalize unknown unit:", unit))
  }
}

normalize_volume <- function(value, unit) {
  list(value = volume_to_L(value, unit), internal_unit = "L")
}

# --- Best display unit chooser ------------------------------------------------

best_volume_unit <- function(value_L) {
  abs_val <- abs(value_L)
  if (abs_val < 1e-4) return("\u00b5l")       # < 0.1 ml → µl

  if (abs_val < 0.01) return("ml")             # < 10 ml → ml (but could also be µl)
  if (abs_val < 1) return("ml")
  return("L")
}

best_mass_conc_unit <- function(value_gL) {
  abs_val <- abs(value_gL)
  if (abs_val < 1e-6) return("pg/ml")
  if (abs_val < 1e-3) return("ng/ml")
  if (abs_val < 1) return("\u00b5g/ml")
  return("mg/ml")
}

best_molar_conc_unit <- function(value_molL) {
  abs_val <- abs(value_molL)
  if (abs_val < 1e-9) return("pM")
  if (abs_val < 1e-6) return("nM")
  if (abs_val < 1e-3) return("\u00b5M")
  if (abs_val < 1) return("mM")
  return("M")
}
