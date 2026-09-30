# ==============================================================================
# ui_saved_recipes.R
# UI and server logic for Saved Recipes tab + Export
# ==============================================================================

saved_recipes_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12,
        wellPanel(
          fluidRow(
            column(4, h4(textOutput(ns("total_count")))),
            column(4, downloadButton(ns("export_word"), "Export All (Word)", class = "btn-info")),
            column(4, actionButton(ns("clear_all"), "Clear All Memory", class = "btn-danger"))
          )
        )
      )
    ),
    fluidRow(
      column(12,
        h4("Single Dilution"),
        uiOutput(ns("single_dilution_recipes")),
        hr(),
        h4("Multi-Step Dilution"),
        uiOutput(ns("multistep_dilution_recipes")),
        hr(),
        h4("MW-Based Preparation"),
        uiOutput(ns("mw_based_recipes")),
        hr(),
        h4("Dry Substance Preparation"),
        uiOutput(ns("dry_substance_recipes"))
      )
    ),
    fluidRow(
      column(12,
        wellPanel(
          h4("Scenario-Specific Excel Export"),
          fluidRow(
            column(3, downloadButton(ns("export_single_xlsx"), "Single Dilution (.xlsx)")),
            column(3, downloadButton(ns("export_multi_xlsx"), "Multi-Step (.xlsx)")),
            column(3, downloadButton(ns("export_mw_xlsx"), "MW-Based (.xlsx)")),
            column(3, downloadButton(ns("export_dry_xlsx"), "Dry Substance (.xlsx)"))
          )
        )
      )
    )
  )
}

saved_recipes_server <- function(id, memory) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$total_count <- renderText({
      paste0("Saved Recipes: ", count_saved(memory()))
    })

    # Render recipe blocks for each scenario
    render_recipe_list <- function(scenario) {
      renderUI({
        mem <- memory()
        recipes <- mem[[scenario]]
        if (length(recipes) == 0) {
          return(tags$p(class = "text-muted", "No saved recipes."))
        }

        recipe_blocks <- lapply(recipes, function(r) {
          wellPanel(
            fluidRow(
              column(10,
                tags$pre(style = "white-space: pre-wrap; background: #f8f9fa; padding: 10px;",
                  r$instruction_text
                )
              ),
              column(2,
                # Every button reports the recipe's unique memory_id to one
                # shared input, so a click always removes exactly that recipe
                tags$button(
                  type = "button",
                  class = "btn btn-sm btn-danger",
                  style = "width: 100%;",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
                    ns("delete_recipe"), r$memory_id
                  ),
                  "Remove"
                )
              )
            ),
            if (length(r$warnings) > 0) {
              tags$small(class = "text-warning",
                paste("Warnings:", paste(r$warnings, collapse = "; "))
              )
            }
          )
        })

        tagList(recipe_blocks)
      })
    }

    output$single_dilution_recipes <- render_recipe_list("single_dilution")
    output$multistep_dilution_recipes <- render_recipe_list("multistep_dilution")
    output$mw_based_recipes <- render_recipe_list("mw_based_preparation")
    output$dry_substance_recipes <- render_recipe_list("dry_substance")

    # Single delete handler: identifies the recipe by memory_id, not by
    # position, so it is unaffected by re-rendering and index shifts
    observeEvent(input$delete_recipe, {
      memory(remove_from_memory_by_id(memory(), input$delete_recipe))
      showNotification("Recipe removed.", type = "message", duration = 2)
    })

    # Clear all
    observeEvent(input$clear_all, {
      showModal(modalDialog(
        title = "Confirm",
        "Are you sure you want to clear all saved recipes?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_clear"), "Clear All", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_clear, {
      memory(clear_all_memory())
      removeModal()
      showNotification("All recipes cleared.", type = "message", duration = 2)
    })

    # --- Word Export ---
    output$export_word <- downloadHandler(
      filename = function() {
        paste0("lab_instructions_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
      },
      content = function(file) {
        export_word_document(memory(), file)
      }
    )

    # --- Excel Exports ---
    make_xlsx_handler <- function(scenario, filename_base) {
      downloadHandler(
        filename = function() {
          paste0(filename_base, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
        },
        content = function(file) {
          export_scenario_excel(memory(), scenario, file)
        }
      )
    }

    output$export_single_xlsx <- make_xlsx_handler("single_dilution", "single_dilution_instructions")
    output$export_multi_xlsx <- make_xlsx_handler("multistep_dilution", "multistep_dilution_plan")
    output$export_mw_xlsx <- make_xlsx_handler("mw_based_preparation", "mw_based_preparation")
    output$export_dry_xlsx <- make_xlsx_handler("dry_substance", "dry_substance_preparation")
  })
}

# ==============================================================================
# Export functions (Word and Excel)
# ==============================================================================

export_word_document <- function(memory, filepath) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    # Fail the download explicitly instead of delivering an empty file
    stop("Package 'officer' is required for Word export.")
  }

  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Laboratory Instructions", style = "heading 1")
  doc <- officer::body_add_par(doc, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")))
  doc <- officer::body_add_par(doc, "")

  # Style for dilution/preparation lines (containing "| DF:")
  fp_dilution <- officer::fp_text(
    font.family = "Calibri Light",
    font.size = 14
  )

  # Style for the first line of each recipe (bold)
  fp_first_line <- officer::fp_text(
    font.family = "Calibri",
    font.size = 12,
    bold = TRUE
  )

  for (scenario in names(memory)) {
    recipes <- memory[[scenario]]
    if (length(recipes) == 0) next

    doc <- officer::body_add_par(doc, scenario_display_name(scenario), style = "heading 2")
    # Empty line after the level-2 heading
    doc <- officer::body_add_par(doc, "")

    for (r in recipes) {
      lines <- strsplit(r$instruction_text, "\n")[[1]]
      first_done <- FALSE

      for (line in lines) {
        line <- trimws(line)
        if (!nzchar(line)) next

        if (!first_done) {
          # First non-empty line of the recipe → bold
          doc <- officer::body_add_fpar(doc,
            officer::fpar(officer::ftext(line, fp_first_line))
          )
          first_done <- TRUE
        } else if (grepl("\\| DF:", line, fixed = FALSE)) {
          # Dilution line → Calibri Light 14pt
          doc <- officer::body_add_fpar(doc,
            officer::fpar(officer::ftext(line, fp_dilution))
          )
        } else {
          doc <- officer::body_add_par(doc, line, style = "Normal")
        }
      }

      if (length(r$warnings) > 0) {
        doc <- officer::body_add_par(doc,
          paste("Warnings:", paste(r$warnings, collapse = "; ")),
          style = "Normal"
        )
      }

      doc <- officer::body_add_par(doc, "")
    }
  }

  print(doc, target = filepath)
}

export_scenario_excel <- function(memory, scenario, filepath) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for Excel export.")
  }

  recipes <- memory[[scenario]]
  if (length(recipes) == 0) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Instructions")
    openxlsx::writeData(wb, "Instructions", data.frame(Note = "No saved recipes for this scenario."))
    openxlsx::saveWorkbook(wb, filepath, overwrite = TRUE)
    return()
  }

  rows <- lapply(recipes, function(r) {
    base <- data.frame(
      Solution = r$solution_name,
      Instruction = r$instruction_text,
      Warnings = paste(r$warnings, collapse = "; "),
      Timestamp = format(r$saved_at, "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )

    ip <- r$input_parameters
    if (scenario == "single_dilution") {
      base$Stock <- paste(ip$stock_concentration, ip$stock_unit)
      base$Target <- paste(ip$target_concentration, ip$target_unit)
      base$Volume <- paste(ip$final_volume, ip$final_volume_unit)
      base$Diluent <- ip$diluent
      base$DF <- r$technical_metadata$dilution_factor
    } else if (scenario == "multistep_dilution") {
      base$Stock <- paste(ip$stock_concentration, ip$stock_unit)
      base$Target <- paste(ip$target_concentration, ip$target_unit)
      base$Volume <- paste(ip$final_volume, ip$final_volume_unit)
      base$Steps <- r$technical_metadata$n_steps
      base$Total_DF <- r$technical_metadata$total_dilution_factor
    } else if (scenario == "mw_based_preparation") {
      base$Substance <- ip$substance
      base$MW <- ip$mw
      base$MW_Source <- ip$mw_source
      base$Stock <- paste(ip$original_stock_val, ip$original_stock_unit)
      base$Target <- paste(ip$target_concentration, ip$target_unit)
      base$Volume <- paste(ip$final_volume, ip$final_volume_unit)
      base$Diluent <- ip$diluent
      base$DF <- r$technical_metadata$dilution_factor
    } else if (scenario == "dry_substance") {
      base$Substance <- ip$substance
      base$MW <- ip$mw
      base$Mass_g <- r$technical_metadata$mass_g
      base$Target <- paste(ip$target_concentration, ip$target_unit)
      base$Volume <- paste(ip$final_volume, ip$final_volume_unit)
      base$Solvent <- ip$solvent
    }

    base
  })

  df <- do.call(rbind, rows)

  wb <- openxlsx::createWorkbook()
  sheet_name <- scenario_display_name(scenario)
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, df)
  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(df)), widths = "auto")
  openxlsx::saveWorkbook(wb, filepath, overwrite = TRUE)
}
