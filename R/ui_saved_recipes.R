# ==============================================================================
# ui_saved_recipes.R
# UI and server logic for Saved Recipes tab + Export
# ==============================================================================

# Recipe types in display order: memory slot -> heading
RECIPE_SCENARIOS <- c(
  "Single Dilution"           = "single_dilution",
  "Multi-Step Dilution"       = "multistep_dilution",
  "MW-Based Preparation"      = "mw_based_preparation",
  "Dry Substance Preparation" = "dry_substance"
)

SORT_LEVEL_LABELS <- c("Sort by", "then by", "then by")

# Short recipe-type labels for the sorting summary
RECIPE_SHORT_LABELS <- c(
  single_dilution      = "Single",
  multistep_dilution   = "Multi-Step",
  mw_based_preparation = "MW-Based",
  dry_substance        = "Dry Substance"
)

saved_recipes_ui <- function(id) {
  ns <- NS(id)

  sort_level_row <- function(i) {
    fluidRow(
      column(8, selectInput(ns(paste0("sort_by_", i)), SORT_LEVEL_LABELS[i],
                            choices = c("(none)" = "none", SORT_KEYS),
                            selected = "none")),
      column(4, class = "sort-dir ps-0", radioButtons(ns(paste0("sort_dir_", i)), "\u00a0",
                             choices = c("\u2191" = "asc", "\u2193" = "desc"),
                             selected = "asc", inline = TRUE))
    )
  }

  tagList(
    # wider gap between the inline sort-direction options
    tags$style(HTML(".sort-dir .radio-inline + .radio-inline { margin-left: 0.75rem; }")),
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
    fluidRow(class = "mt-3",
      # ----- Left panel: automatic sorting -----------------------------------
      column(3,
        wellPanel(
          h4("Sorting"),
          selectInput(ns("sort_type"), "Recipe type", choices = RECIPE_SCENARIOS),
          lapply(seq_along(SORT_LEVEL_LABELS), sort_level_row),
          actionButton(ns("sort_btn"), "Sort", class = "btn-primary", width = "100%"),
          uiOutput(ns("sort_summary")),
          tags$small(class = "text-muted d-block mt-2",
            "Criteria are set per recipe type; Sort applies all of them. ",
            "Molar concentrations go before mass ones; unnamed recipes go last.")
        ),
        # Display / Word export options
        wellPanel(
          h4("Display"),
          checkboxInput(ns("show_pipettes"), "Show pipette in instruction",
                        value = FALSE),
          checkboxInput(ns("keep_warnings"), "Keep warnings", value = TRUE)
        )
      ),
      # ----- Right panel: recipe lists ---------------------------------------
      column(9,
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

    # Button that reports `value` to one shared module input; buttons are
    # identified by the recipe's unique memory_id, not by position
    recipe_button <- function(input_id, value, label, class, disabled = FALSE) {
      tags$button(
        type = "button",
        class = paste("btn btn-sm", class),
        style = "width: 100%;",
        disabled = if (disabled) NA else NULL,
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'});",
                          ns(input_id), value),
        label
      )
    }

    # Render recipe blocks for each scenario
    render_recipe_list <- function(scenario) {
      renderUI({
        mem <- memory()
        recipes <- mem[[scenario]]
        show_pip <- isTRUE(input$show_pipettes)
        keep_warn <- isTRUE(input$keep_warnings)
        if (length(recipes) == 0) {
          return(tags$p(class = "text-muted", "No saved recipes."))
        }
        n <- length(recipes)

        recipe_blocks <- lapply(seq_len(n), function(i) {
          r <- recipes[[i]]
          wellPanel(
            fluidRow(
              column(9,
                tags$pre(style = "white-space: pre-wrap; background: #f8f9fa; padding: 10px;",
                  recipe_text(r, show_pip)
                )
              ),
              column(3,
                recipe_button("delete_recipe", r$memory_id, "Remove", "btn-danger"),
                fluidRow(class = "g-1 mt-1",
                  column(6, recipe_button("move_recipe", paste0(r$memory_id, "|up"),
                                          "\u2191 Up", "btn-outline-secondary",
                                          disabled = i == 1)),
                  column(6, recipe_button("move_recipe", paste0(r$memory_id, "|down"),
                                          "\u2193 Down", "btn-outline-secondary",
                                          disabled = i == n))
                )
              )
            ),
            if (keep_warn && length(r$warnings) > 0) {
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

    # Up / Down: value is "<memory_id>|up" or "<memory_id>|down"
    observeEvent(input$move_recipe, {
      parts <- strsplit(input$move_recipe, "|", fixed = TRUE)[[1]]
      delta <- if (identical(parts[2], "up")) -1 else 1
      memory(move_recipe(memory(), parts[1], delta))
    })

    # ---- Sorting: settings kept per recipe type (session only) -------------
    n_levels <- length(SORT_LEVEL_LABELS)
    blank_levels <- function() lapply(seq_len(n_levels), function(i) list(key = "none", desc = FALSE))
    stored_settings <- reactiveVal(
      setNames(lapply(RECIPE_SCENARIOS, function(x) blank_levels()), RECIPE_SCENARIOS))
    shown_type <- RECIPE_SCENARIOS[[1]]   # type whose settings the widgets show

    read_widgets <- function() {
      lapply(seq_len(n_levels), function(i) list(
        key  = input[[paste0("sort_by_", i)]] %||% "none",
        desc = identical(input[[paste0("sort_dir_", i)]], "desc")
      ))
    }
    store_widgets <- function(type) {
      st <- isolate(stored_settings())
      st[[type]] <- isolate(read_widgets())
      stored_settings(st)
    }

    # On type change: store the widgets into the previously shown type,
    # then show the newly selected type's settings
    observeEvent(input$sort_type, {
      store_widgets(shown_type)
      shown_type <<- input$sort_type
      lv <- stored_settings()[[shown_type]]
      for (i in seq_len(n_levels)) {
        updateSelectInput(session, paste0("sort_by_", i), selected = lv[[i]]$key)
        updateRadioButtons(session, paste0("sort_dir_", i),
                           selected = if (lv[[i]]$desc) "desc" else "asc")
      }
    }, ignoreInit = TRUE)

    # Current settings of all types (widgets for the type on screen)
    current_settings <- reactive({
      st <- stored_settings()
      type <- input$sort_type %||% shown_type
      st[[type]] <- read_widgets()
      st
    })

    describe_levels <- function(levels) {
      parts <- vapply(levels, function(l) {
        if (!l$key %in% SORT_KEYS) return(NA_character_)
        paste0(names(SORT_KEYS)[SORT_KEYS == l$key], if (l$desc) " \u2193" else " \u2191")
      }, character(1))
      parts <- unique(parts[!is.na(parts)])
      if (length(parts) == 0) NA_character_ else paste(parts, collapse = ", ")
    }

    # Summary of what Sort will do, per recipe type
    output$sort_summary <- renderUI({
      st <- current_settings()
      items <- lapply(names(RECIPE_SCENARIOS), function(lbl) {
        d <- describe_levels(st[[RECIPE_SCENARIOS[[lbl]]]])
        tags$li(tags$strong(paste0(RECIPE_SHORT_LABELS[[RECIPE_SCENARIOS[[lbl]]]], ": ")),
                if (is.na(d)) tags$span(class = "text-muted", "\u2014") else d)
      })
      tags$ul(class = "small ps-3 mt-2 mb-0", items)
    })

    observeEvent(input$sort_btn, {
      store_widgets(shown_type)
      st <- stored_settings()
      mem <- memory()
      sorted <- character(0)
      for (lbl in names(RECIPE_SCENARIOS)) {
        sc <- RECIPE_SCENARIOS[[lbl]]
        if (is.na(describe_levels(st[[sc]])) || length(mem[[sc]]) < 2) next
        mem <- sort_scenario_recipes(mem, sc, st[[sc]])
        sorted <- c(sorted, lbl)
      }
      memory(mem)
      if (length(sorted) > 0) {
        showNotification(paste("Sorted:", paste(sorted, collapse = ", ")),
                         type = "message", duration = 3)
      } else {
        showNotification(paste("Nothing to sort: set criteria for a recipe type",
                               "that has at least two saved recipes."),
                         type = "warning", duration = 5)
      }
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
        export_word_document(memory(), file,
                             show_pipettes = isTRUE(input$show_pipettes),
                             keep_warnings = isTRUE(input$keep_warnings))
      }
    )

    # --- Excel Exports ---
    make_xlsx_handler <- function(scenario, filename_base) {
      downloadHandler(
        filename = function() {
          paste0(filename_base, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
        },
        content = function(file) {
          export_scenario_excel(memory(), scenario, file,
                                show_pipettes = isTRUE(input$show_pipettes))
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

export_word_document <- function(memory, filepath, show_pipettes = TRUE,
                                 keep_warnings = TRUE) {
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
      lines <- strsplit(recipe_text(r, show_pipettes), "\n")[[1]]
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

      if (keep_warnings && length(r$warnings) > 0) {
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

export_scenario_excel <- function(memory, scenario, filepath, show_pipettes = TRUE) {
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
      Instruction = recipe_text(r, show_pipettes),
      Warnings = paste(r$warnings, collapse = "; "),
      Timestamp = format(r$saved_at, "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )

    # Volume precision used for displayed volumes (absent for dry substance)
    prec <- r$technical_metadata$precision
    if (!is.null(prec)) {
      base$Volume_Precision <- if (identical(prec$mode, "pipette")) {
        paste0("Pipette-based (", prec$pipettes, ")")
      } else {
        paste0("Manual (stock ", prec$stock_sig, ", diluent ", prec$diluent_sig, " sig. fig.)")
      }
    }

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
