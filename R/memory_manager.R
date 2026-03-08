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

  # Add unique ID and save timestamp
  result$memory_id <- paste0(scenario, "_", length(memory[[scenario]]) + 1,
                             "_", format(Sys.time(), "%H%M%S"))
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
