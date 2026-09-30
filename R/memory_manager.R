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
  if (!scenario %in% names(memory)) {
    warning(paste("Unknown scenario type:", scenario))
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
