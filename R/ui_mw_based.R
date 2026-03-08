# ==============================================================================
# ui_mw_based.R
# UI and server logic for MW-Based Preparation tab
# Uses molecular_weights_proteins.xlsx
# ==============================================================================

mw_based_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4,
        wellPanel(
          h4("Substance & Molecular Weight"),
          # MW source choices will be updated dynamically from sheet names
          radioButtons(ns("mw_source"), "MW source:",
            choices = c("Common proteins" = "Common proteins",
                        "Custom proteins" = "Custom proteins",
                        "Manual entry" = "manual"),
            selected = "Common proteins", inline = TRUE
          ),
          conditionalPanel(
            condition = paste0("input['", ns("mw_source"), "'] != 'manual'"),
            selectizeInput(ns("substance_db"), "Select substance",
              choices = NULL, options = list(placeholder = "Type to search..."))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("mw_source"), "'] == 'manual'"),
            textInput(ns("substance_manual"), "Substance name", placeholder = "e.g. My protein"),
            numericInput(ns("mw_manual"), "Molecular weight (Da)", value = NA, min = 0, step = 0.01)
          ),
          uiOutput(ns("mw_display"))
        ),
        wellPanel(
          h4("Concentration & Volume"),
          fluidRow(
            column(9, numericInput(ns("stock_val"), "Stock concentration", value = 1, min = 0, step = 0.1)),
            column(3, selectInput(ns("stock_unit"), " ", choices = ALL_CONC_UNITS, selected = "mg/ml"))
          ),
          fluidRow(
            column(9, numericInput(ns("target_val"), "Target concentration", value = 10, min = 0, step = 0.1)),
            column(3, selectInput(ns("target_unit"), " ", choices = ALL_CONC_UNITS, selected = "nM"))
          ),
          fluidRow(
            column(9, numericInput(ns("final_vol"), "Final volume", value = 10, min = 0, step = 0.1)),
            column(3, selectInput(ns("final_vol_unit"), " ", choices = VOLUME_UNITS, selected = "ml"))
          ),
          textInput(ns("diluent"), "Diluent", value = "PBS")
        )
      ),
      column(8,
        wellPanel(
          h4("Result"),
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

mw_based_server <- function(id, memory, mw_db) {
  moduleServer(id, function(input, output, session) {

    # Update radio button labels from actual sheet names
    observe({
      db <- mw_db()
      sheet_names <- db$sheet_names
      choices <- setNames(sheet_names, sheet_names)
      choices <- c(choices, "Manual entry" = "manual")
      updateRadioButtons(session, "mw_source", choices = choices, selected = sheet_names[1], inline = TRUE)
    })

    # Update substance dropdown based on selected sheet
    observe({
      db <- mw_db()
      src <- input$mw_source
      if (is.null(src) || src == "manual") return()
      choices <- get_substance_names_from_sheet(db, src)
      selected <- if (length(choices) > 0) choices[1] else NULL
      updateSelectizeInput(session, "substance_db", choices = choices, selected = selected)
    })

    # Current MW value
    current_mw <- reactive({
      src <- input$mw_source
      if (is.null(src)) return(list(substance = "", mw = NA, source = ""))

      if (src == "manual") {
        list(substance = input$substance_manual, mw = input$mw_manual, source = "manual")
      } else {
        req(input$substance_db)
        req(nzchar(input$substance_db))
        db <- mw_db()
        mw <- lookup_mw(input$substance_db, db, sheet = src)
        list(substance = input$substance_db, mw = mw, source = src)
      }
    })

    output$mw_display <- renderUI({
      info <- current_mw()
      if (is.na(info$mw) || is.null(info$mw)) {
        tags$div(class = "alert alert-info", "Select a substance or enter MW manually.")
      } else {
        tags$div(class = "alert alert-success",
          tags$strong(info$substance), ": MW = ", format(info$mw, big.mark = ","), " Da"
        )
      }
    })

    result <- reactive({
      info <- current_mw()
      req(info$mw)
      req(!is.na(info$mw), info$mw > 0)
      req(input$stock_val, input$target_val, input$final_vol)
      req(input$stock_val > 0, input$target_val > 0, input$final_vol > 0)

      calculate_mw_based_preparation(
        substance = info$substance,
        mw = info$mw,
        mw_source = info$source,
        stock_val = input$stock_val,
        stock_unit = input$stock_unit,
        target_val = input$target_val,
        target_unit = input$target_unit,
        final_vol = input$final_vol,
        final_vol_unit = input$final_vol_unit,
        diluent = input$diluent
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
      s <- res$steps[[1]]
      data.frame(
        Parameter = c("Substance", "MW (Da)", "MW Source",
                       "Dilution Factor", "Stock Volume", "Diluent Volume"),
        Value = c(
          res$input_parameters$substance,
          format(res$technical_metadata$mw, big.mark = ","),
          res$technical_metadata$mw_source,
          format_df(s$dilution_factor),
          format_volume(s$stock_volume_L),
          format_volume(s$diluent_volume_L)
        ),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, hover = TRUE, width = "100%")

    observeEvent(input$save_btn, {
      req(result())
      memory(save_to_memory(memory(), result()))
      showNotification("Saved to memory!", type = "message", duration = 2)
    })

    observeEvent(input$clear_btn, {
      # placeholder
    })
  })
}
