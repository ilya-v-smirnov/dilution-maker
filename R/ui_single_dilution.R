# ==============================================================================
# ui_single_dilution.R
# UI and server logic for Single Dilution tab
# ==============================================================================

single_dilution_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4,
        wellPanel(
          h4("Input Parameters"),
          textInput(ns("solution_name"), "Solution name (optional)", placeholder = "e.g. Anti-CD3 antibody"),
          hr(),
          fluidRow(
            column(9, numericInput(ns("stock_val"), "Stock concentration", value = 1, min = 0, step = 0.1)),
            column(3, selectInput(ns("stock_unit"), "\u00a0", choices = ALL_CONC_UNITS, selected = "mg/ml"))
          ),
          fluidRow(
            column(9, numericInput(ns("target_val"), "Target concentration", value = 10, min = 0, step = 0.1)),
            column(3, selectInput(ns("target_unit"), "\u00a0", choices = ALL_CONC_UNITS, selected = "\u00b5g/ml"))
          ),
          fluidRow(
            column(9, numericInput(ns("final_vol"), "Final volume", value = 10, min = 0, step = 0.1)),
            column(3, selectInput(ns("final_vol_unit"), "\u00a0", choices = VOLUME_UNITS, selected = "ml"))
          ),
          textInput(ns("diluent"), "Diluent", value = "PBS"),
          checkboxInput(ns("use_approx"), "Use approximation when DF > 100", value = TRUE)
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

single_dilution_server <- function(id, memory) {
  moduleServer(id, function(input, output, session) {

    result <- reactive({
      # Require all numeric inputs
      req(input$stock_val, input$target_val, input$final_vol)
      req(input$stock_val > 0, input$target_val > 0, input$final_vol > 0)

      # Validate
      val <- validate_single_dilution(
        input$stock_val, input$stock_unit,
        input$target_val, input$target_unit,
        input$final_vol, input$final_vol_unit
      )
      req(val$ok)

      calculate_single_dilution(
        stock_val = input$stock_val,
        stock_unit = input$stock_unit,
        target_val = input$target_val,
        target_unit = input$target_unit,
        final_vol = input$final_vol,
        final_vol_unit = input$final_vol_unit,
        diluent = input$diluent,
        solution_name = input$solution_name,
        use_approximation = input$use_approx
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
        Parameter = c("Dilution Factor", "Stock Volume", "Diluent Volume",
                       "Final Volume", "Approximation"),
        Value = c(
          format_df(s$dilution_factor),
          format_volume(s$stock_volume_L),
          format_volume(s$diluent_volume_L),
          format_volume(res$normalized_parameters$final_vol_L),
          if (res$technical_metadata$approximation_used) "Yes" else "No"
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
      # nothing to clear with reactive calc; kept for UX consistency
    })
  })
}
