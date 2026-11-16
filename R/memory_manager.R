# ==============================================================================
# memory_manager.R
# Session memory: stores calculation results grouped by scenario
# ==============================================================================

#' Initialize an empty memory store
#' @return reactiveValues-compatible list structure
init_memory <- function() {
  list(
    single_dilution = list(),
    multistep_dilution = list(),
    mw_based_preparation = list(),
    dry_substance = list()
  )
}

#' Save a result object to memory
#' @param memory The current memory list
#' @param result A structured result object from any calculation
#' @return Updated memory list
save_to_memory <- function(memory, result) {
  scenario <- result$scenario_type
  if (!is.character(scenario) || length(scenario) != 1 || !scenario %in% names(memory)) {
    warning(paste("Unknown scenario type:", paste(scenario, collapse = ", ")))
    return(memory)
  }

  # Add unique ID and save timestamp. The ID must stay unique even after
  # deletions and several saves within the same second, because the
  # Saved Recipes tab uses it to identify which recipe to remove.
  result$memory_id <- sprintf("%s_%s_%08x", scenario,
                              format(Sys.time(), "%Y%m%d%H%M%S"),
                              sample.int(.Machine$integer.max, 1))
  result$saved_at <- Sys.time()

  memory[[scenario]] <- c(memory[[scenario]], list(result))
  memory
}

#' Remove a result from memory by scenario and index
#' @param memory Current memory list
#' @param scenario Scenario type string
#' @param index Integer index within that scenario
#' @return Updated memory list
remove_from_memory <- function(memory, scenario, index) {
  if (scenario %in% names(memory) && index >= 1 && index <= length(memory[[scenario]])) {
    memory[[scenario]][[index]] <- NULL
  }
  memory
}

#' Remove a result from memory by its unique memory_id
#' @param memory Current memory list
#' @param memory_id ID assigned by save_to_memory()
#' @return Updated memory list (unchanged if the ID is not found)
remove_from_memory_by_id <- function(memory, memory_id) {
  for (scenario in names(memory)) {
    ids <- vapply(memory[[scenario]], function(r) r$memory_id %||% "", character(1))
    idx <- match(memory_id, ids)
    if (!is.na(idx)) {
      memory[[scenario]][[idx]] <- NULL
      break
    }
  }
  memory
}

#' Clear all results from a specific scenario
clear_scenario_memory <- function(memory, scenario) {
  if (scenario %in% names(memory)) {
    memory[[scenario]] <- list()
  }
  memory
}

#' Clear all memory
clear_all_memory <- function() {
  init_memory()
}

#' Get total count of saved results
count_saved <- function(memory) {
  sum(sapply(memory, length))
}

#' Get all results as a flat list (for export)
get_all_results <- function(memory) {
  unlist(memory, recursive = FALSE)
}

#' Get scenario display name
scenario_display_name <- function(scenario) {
  switch(scenario,
    "single_dilution"      = "Single Dilution",
    "multistep_dilution"   = "Multi-Step Dilution",
    "mw_based_preparation" = "MW-Based Preparation",
    "dry_substance"        = "Dry Substance Preparation",
    scenario
  )
}

# ==============================================================================
# Ordering of saved recipes (manual Up/Down and automatic sorting)
# ==============================================================================

#' Sort criteria available in the Saved Recipes tab
SORT_KEYS <- c("Substance name" = "name",
               "Target conc."   = "target",
               "Final volume"   = "volume")

#' Move a recipe one position up (delta = -1) or down (delta = +1)
#' within its scenario. Out-of-range moves leave memory unchanged.
move_recipe <- function(memory, memory_id, delta) {
  for (scenario in names(memory)) {
    recs <- memory[[scenario]]
    ids <- vapply(recs, function(r) r$memory_id %||% "", character(1))
    idx <- match(memory_id, ids)
    if (is.na(idx)) next
    new_idx <- idx + delta
    if (new_idx < 1 || new_idx > length(recs)) return(memory)
    recs[c(idx, new_idx)] <- recs[c(new_idx, idx)]
    memory[[scenario]] <- recs
    return(memory)
  }
  memory
}

#' Sort-key vectors (one element per recipe) for a single criterion.
#' Returns a list of vectors to pass to order():
#'   name   -> empty names always last, then name (case-insensitive)
#'   target -> molar block first, then mass block, then the normalized value
#'   volume -> final volume in L
recipe_sort_vectors <- function(recipes, key, desc = FALSE) {
  dir <- if (desc) -1 else 1
  if (key == "name") {
    nm <- vapply(recipes, function(r) trimws(r$solution_name %||% ""), character(1))
    empty <- as.integer(!nzchar(nm))
    return(list(empty, dir * xtfrm(tolower(nm))))
  }
  if (key == "target") {
    type <- vapply(recipes, function(r) {
      u <- r$input_parameters$target_unit
      if (is.null(u)) NA_character_ else get_conc_type(u)
    }, character(1))
    block <- match(type, c("molar", "mass"), nomatch = 3L)
    val <- vapply(recipes, function(r)
      as.numeric(r$normalized_parameters$target_conc_internal %||% NA_real_), numeric(1))
    return(list(block, dir * val))
  }
  if (key == "volume") {
    val <- vapply(recipes, function(r)
      as.numeric(r$normalized_parameters$final_vol_L %||% NA_real_), numeric(1))
    return(list(dir * val))
  }
  list()
}

#' Sort the recipes of one scenario by up to several criteria.
#' @param levels list of list(key = "name"|"target"|"volume", desc = logical);
#'   "none" keys are skipped, repeated keys use their first occurrence.
#'   Ties keep their current relative order (stable).
sort_scenario_recipes <- function(memory, scenario, levels) {
  recs <- memory[[scenario]]
  if (length(recs) < 2) return(memory)
  keys <- vapply(levels, function(l) l$key %||% "none", character(1))
  keep <- keys %in% SORT_KEYS & !duplicated(keys)
  levels <- levels[keep]
  if (length(levels) == 0) return(memory)

  vecs <- do.call(c, lapply(levels, function(l)
    recipe_sort_vectors(recs, l$key, isTRUE(l$desc))))
  ord <- do.call(order, c(vecs, list(seq_along(recs), na.last = TRUE)))
  memory[[scenario]] <- recs[ord]
  memory
}
