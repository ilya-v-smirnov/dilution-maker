# ==============================================================================
# ui_dry_substance.R
# UI and server logic for Dry Substance Preparation tab
# Uses molecular_weights_substances.xlsx
# ==============================================================================

dry_substance_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4,
        wellPanel(
          h4("Substance"),
          # MW source choices will be updated dynamically from sheet names
          radioButtons(ns("mw_source"), "MW source:",
            choices = c("Common substances" = "Common substances",
                        "Custom substances" = "Custom substances",
                        "Manual entry" = "manual"),
            selected = "Common substances", inline = TRUE
          ),
          conditionalPanel(
            condition = paste0("input['", ns("mw_source"), "'] != 'manual'"),
            selectizeInput(ns("substance_db"), "Select substance",
              choices = NULL, options = list(placeholder = "Type to search..."))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("mw_source"), "'] == 'manual'"),
            textInput(ns("substance_manual"), "Substance name", placeholder = "e.g. NaCl"),
            numericInput(ns("mw_manual"), "Molecular weight (Da)", value = NA, min = 0, step = 0.01),
            numericInput(ns("purity_manual"), "Purity, % (optional; empty = 100%)",
                         value = NA, min = 0, max = 100, step = 0.1)
          ),
          uiOutput(ns("mw_display"))
        ),
        wellPanel(id = ns("target_panel"),
          h4("Target Solution"),
          fluidRow(
            column(9, numericInput(ns("target_val"), "Target concentration", value = 100, min = 0, step = 0.1)),
            column(3, selectInput(ns("target_unit"), " ", choices = ALL_CONC_UNITS, selected = "mM"))
          ),
          fluidRow(
            column(9, numericInput(ns("final_vol"), "Final volume", value = 10, min = 0, step = 0.1)),
            column(3, selectInput(ns("final_vol_unit"), " ", choices = VOLUME_UNITS, selected = "ml"))
          ),
          textInput(ns("solvent"), "Solvent", value = "Water")
        )
      ),
      column(8,
        wellPanel(
          h4("Preparation Instruction"),
          uiOutput(ns("warnings_ui")),
          verbatimTextOutput(ns("instruction_text")),
          hr(),
          fluidRow(
            column(4, actionButton(ns("save_btn"), "Save to Memory", class = "btn-success", width = "100%")),
            column(4, actionButton(ns("clear_btn"), "Clear", class = "btn-default", width = "100%"))
          )
        ),
        wellPanel(
          h4("Details"),
          tableOutput(ns("details_table"))
        )
      )
    )
  )
}

dry_substance_server <- function(id, memory, mw_db) {
  moduleServer(id, function(input, output, session) {

    # Update radio button labels from actual sheet names
    observe({
      db <- mw_db()
      sheet_names <- db$sheet_names
      choices <- setNames(sheet_names, sheet_names)
      choices <- c(choices, "Manual entry" = "manual")
      # Keep the user's current source when the DB is edited in another tab
      current <- isolate(input$mw_source)
      selected <- if (!is.null(current) && current %in% choices) current else sheet_names[1]
      updateRadioButtons(session, "mw_source", choices = choices, selected = selected, inline = TRUE)
    })

    # Update substance dropdown based on selected sheet
    observe({
      db <- mw_db()
      src <- input$mw_source
      if (is.null(src) || src == "manual") return()
      # Custom substances are listed as "substance supplier purity%"
      choices <- get_substance_labels_from_sheet(db, src)
      # Keep the current substance if it is still listed (e.g. after a DB edit)
      current <- isolate(input$substance_db)
      selected <- if (!is.null(current) && current %in% choices) {
        current
      } else if (length(choices) > 0) {
        choices[1]
      } else {
        NULL
      }
      updateSelectizeInput(session, "substance_db", choices = choices, selected = selected)
    })

    current_mw <- reactive({
      src <- input$mw_source
      empty <- list(substance = "", label = "", mw = NA, source = "",
                    supplier = "", purity = NA_real_, cat = "")
      if (is.null(src)) return(empty)

      if (src == "manual") {
        name <- trimws(input$substance_manual %||% "")
        purity <- input$purity_manual
        if (is.null(purity)) purity <- NA_real_
        list(substance = name,
             label = if (nzchar(name)) substance_label(name, "", purity) else "",
             mw = input$mw_manual, source = "manual",
             supplier = "", purity = purity, cat = "")
      } else {
        req(input$substance_db)
        req(nzchar(input$substance_db))
        entry <- lookup_substance_entry(mw_db(), src, input$substance_db)
        req(entry)
        list(substance = entry$substance, label = entry$label, mw = entry$MW,
             source = src, supplier = entry$supplier, purity = entry$purity,
             cat = entry$cat)
      }
    })

    output$mw_display <- renderUI({
      info <- current_mw()
      if (is.null(info$mw) || is.na(info$mw)) {
        return(tags$div(class = "alert alert-info", "Select a substance or enter MW manually."))
      }
      purity_err <- validate_purity(info$purity)
      if (!is.null(purity_err)) {
        return(tags$div(class = "alert alert-danger", purity_err))
      }
      extras <- c(
        if (!is.na(info$purity)) paste0("Purity = ", format_purity(info$purity)),
        if (nzchar(info$cat)) paste0("Cat# ", info$cat)
      )
      tags$div(class = "alert alert-success",
        tags$strong(if (nzchar(info$label)) info$label else info$substance),
        ": MW = ", format(info$mw, big.mark = ","), " Da",
        if (length(extras) > 0) tagList(tags$br(), paste(extras, collapse = "; "))
      )
    })

    result <- reactive({
      info <- current_mw()
      req(info$mw)
      req(!is.na(info$mw), info$mw > 0)
      req(input$target_val, input$final_vol)
      req(input$target_val > 0, input$final_vol > 0)
      req(is.null(validate_purity(info$purity)))

      calculate_dry_substance(
        substance = info$substance,
        mw = info$mw,
        target_val = input$target_val,
        target_unit = input$target_unit,
        final_vol = input$final_vol,
        final_vol_unit = input$final_vol_unit,
        solvent = input$solvent,
        purity = info$purity,
        supplier = info$supplier,
        cat = info$cat,
        label = info$label
      )
    })

    output$instruction_text <- renderText({
      req(result())
      result()$instruction_text
    })

    output$warnings_ui <- renderUI({
      req(result())
      w <- result()$warnings
      if (length(w) == 0) return(NULL)
      tags$div(class = "alert alert-warning",
        tags$strong("Warnings:"),
        tags$ul(lapply(w, tags$li))
      )
    })

    output$details_table <- renderTable({
      req(result())
      res <- result()
      ip <- res$input_parameters
      tm <- res$technical_metadata
      dash <- function(x) if (is.null(x) || is.na(x) || !nzchar(x)) "\u2014" else x
      data.frame(
        Parameter = c("Substance", "Supplier", "Purity", "Cat#", "MW (Da)",
                      "Required Mass", "Mass at 100% purity",
                      "Target Concentration", "Final Volume", "Solvent"),
        Value = c(
          ip$substance,
          dash(ip$supplier),
          if (is.na(ip$purity)) "100% (assumed)" else format_purity(ip$purity),
          dash(ip$cat),
          format(tm$mw, big.mark = ","),
          format_mass(tm$mass_g),
          format_mass(tm$mass_pure_g),
          paste0(sig3(ip$target_concentration), " ", ip$target_unit),
          format_volume(res$normalized_parameters$final_vol_L),
          ip$solvent
        ),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, hover = TRUE, width = "100%")

    observeEvent(input$save_btn, {
      req(result())
      memory(save_to_memory(memory(), result()))
      showNotification("Saved to memory!", type = "message", duration = 2)
    })

    # Restore target/volume/solvent and manual inputs to their defaults;
    # the selected MW source and substance are kept
    observeEvent(input$clear_btn, {
      for (id in c("target_panel", "substance_manual", "mw_manual", "purity_manual")) {
        shinyjs::reset(id)
      }
    })
  })
}
