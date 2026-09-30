# ==============================================================================
# ui_multistep_dilution.R
# UI and server logic for Multi-Step Dilution Planner tab.
# Supports "Use available stock" (substance + variant from stocks DB).
# ==============================================================================

multistep_dilution_ui <- function(id) {
  ns <- NS(id)

  use_stock_off <- paste0("!input['", ns("use_stock"), "']")
  use_stock_on  <- paste0("input['", ns("use_stock"), "']")

  tagList(
    fluidRow(
      column(4, id = ns("inputs_panel"),
        wellPanel(
          h4("Input Parameters"),

          conditionalPanel(
            condition = use_stock_off,
            textInput(ns("solution_name"), "Solution name (optional)",
                      placeholder = "e.g. Serial dilution of Ab")
          ),
          conditionalPanel(
            condition = use_stock_on,
            selectInput(ns("stock_substance"), "Substance (from stocks)",
                        choices = character(0)),
            selectInput(ns("stock_variant"), "Stock variant",
                        choices = character(0))
          ),

          checkboxInput(ns("use_stock"), "Use available stock", value = FALSE),
          hr(),

          fluidRow(
            column(9, numericInput(ns("stock_val"), "Stock concentration",
                                   value = 10, min = 0, step = 0.1)),
            column(3, selectInput(ns("stock_unit"), "\u00a0",
                                  choices = ALL_CONC_UNITS, selected = "mg/ml"))
          ),
          fluidRow(
            column(9, numericInput(ns("target_val"), "Target concentration",
                                   value = 1, min = 0, step = 0.001)),
            column(3, selectInput(ns("target_unit"), "\u00a0",
                                  choices = ALL_CONC_UNITS, selected = "ng/ml"))
          ),
          fluidRow(
            column(9, numericInput(ns("final_vol"), "Final volume",
                                   value = 10, min = 0, step = 0.1)),
            column(3, selectInput(ns("final_vol_unit"), "\u00a0",
                                  choices = VOLUME_UNITS, selected = "ml"))
          ),
          textInput(ns("diluent"), "Diluent", value = "PBS"),
          hr(),
          h5("Planner Settings"),
          numericInput(ns("max_df"), "Max DF per step",
                       value = 500, min = 2, max = 10000, step = 10),
          numericInput(ns("min_pipette"), "Min pipetting volume (\u00b5l)",
                       value = 0.5, min = 0.1, max = 10, step = 0.1),
          numericInput(ns("intermediate_vol"), "Preferred intermediate volume (\u00b5l)",
                       value = 500, min = 50, max = 50000, step = 50)
        )
      ),
      column(8,
        wellPanel(
          h4("Dilution Plan"),
          uiOutput(ns("errors_ui")),
          uiOutput(ns("warnings_ui")),
          verbatimTextOutput(ns("instruction_text")),
          hr(),
          fluidRow(
            column(4, actionButton(ns("save_btn"), "Save to Memory",
                                   class = "btn-success", width = "100%")),
            column(4, actionButton(ns("clear_btn"), "Clear",
                                   class = "btn-default", width = "100%"))
          )
        ),
        wellPanel(
          h4("Step Details"),
          tableOutput(ns("steps_table"))
        )
      )
    )
  )
}

multistep_dilution_server <- function(id, memory, stocks_db) {
  moduleServer(id, function(input, output, session) {

    # --- Populate substance dropdown ---
    observe({
      if (!isTRUE(input$use_stock)) return()
      subs <- get_unique_stock_substances(stocks_db())
      sel  <- if (length(subs) > 0) {
        if (!is.null(input$stock_substance) && input$stock_substance %in% subs) {
          input$stock_substance
        } else {
          subs[1]
        }
      } else character(0)
      updateSelectInput(session, "stock_substance", choices = subs, selected = sel)
    })

    # --- Populate variant dropdown ---
    observe({
      if (!isTRUE(input$use_stock)) return()
      req(input$stock_substance)
      if (!nzchar(input$stock_substance)) return()
      v <- get_stock_variants(stocks_db(), input$stock_substance)
      sel <- if (length(v$labels) > 0) v$labels[1] else character(0)
      updateSelectInput(session, "stock_variant",
                        choices = v$labels, selected = sel)
    })

    selected_stock <- reactive({
      if (!isTRUE(input$use_stock)) return(NULL)
      if (is.null(input$stock_substance) || !nzchar(input$stock_substance)) return(NULL)
      if (is.null(input$stock_variant) || !nzchar(input$stock_variant)) return(NULL)
      get_stock_by_variant(stocks_db(), input$stock_substance, input$stock_variant)
    })

    observeEvent(list(input$use_stock, selected_stock()), {
      if (isTRUE(input$use_stock)) {
        s <- selected_stock()
        if (!is.null(s)) {
          updateNumericInput(session, "stock_val", value = s$conc)
          updateSelectInput(session, "stock_unit", selected = s$units)
        }
        shinyjs::disable("stock_val")
        shinyjs::disable("stock_unit")
      } else {
        shinyjs::enable("stock_val")
        shinyjs::enable("stock_unit")
      }
    }, ignoreInit = FALSE)

    effective_solution_name <- reactive({
      if (isTRUE(input$use_stock)) {
        s <- selected_stock()
        base <- input$stock_substance %||% ""
        if (is.null(s)) return(base)
        augment_name_with_stock(base, s)
      } else {
        input$solution_name %||% ""
      }
    })

    # --- Input validation (errors are shown to the user) ---
    validation <- reactive({
      val <- validate_single_dilution(
        input$stock_val, input$stock_unit,
        input$target_val, input$target_unit,
        input$final_vol, input$final_vol_unit
      )
      errors <- val$errors
      if (is.null(input$max_df) || is.na(input$max_df) || input$max_df <= 1) {
        errors <- c(errors, "Max DF per step must be greater than 1.")
      }
      for (check in list(
        validate_positive(input$min_pipette, "Min pipetting volume"),
        validate_positive(input$intermediate_vol, "Preferred intermediate volume")
      )) {
        if (!check$ok) errors <- c(errors, check$msg)
      }
      list(ok = length(errors) == 0, errors = errors, warnings = val$warnings)
    })

    result <- reactive({
      val <- validation()
      req(val$ok)

      if (isTRUE(input$use_stock)) {
        req(selected_stock())
      }

      plan_multistep_dilution(
        stock_val      = input$stock_val,
        stock_unit     = input$stock_unit,
        target_val     = input$target_val,
        target_unit    = input$target_unit,
        final_vol      = input$final_vol,
        final_vol_unit = input$final_vol_unit,
        diluent        = input$diluent,
        solution_name  = effective_solution_name(),
        max_df_per_step             = input$max_df,
        min_pipette_vol_ul          = input$min_pipette,
        preferred_intermediate_vol_ul = input$intermediate_vol
      )
    })

    output$instruction_text <- renderText({
      req(result())
      result()$instruction_text
    })

    # Show why nothing is calculated (e.g. stock below target, incompatible units)
    output$errors_ui <- renderUI({
      val <- validation()
      if (isTRUE(val$ok)) return(NULL)
      tags$div(class = "alert alert-danger",
        tags$strong("Cannot calculate:"),
        tags$ul(lapply(val$errors, tags$li))
      )
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

    output$steps_table <- renderTable({
      req(result())
      steps <- result()$steps
      conc_type <- result()$normalized_parameters$conc_type

      do.call(rbind, lapply(steps, function(s) {
        data.frame(
          Step = s$step,
          Source = s$source_label,
          `Take` = format_volume(s$stock_volume_L),
          `Add Diluent` = format_volume(s$diluent_volume_L),
          `Step DF` = format_df(s$dilution_factor),
          `Result Conc` = format_concentration(s$result_conc_internal, conc_type),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }))
    }, striped = TRUE, hover = TRUE, width = "100%")

    observeEvent(input$save_btn, {
      req(result())
      memory(save_to_memory(memory(), result()))
      showNotification("Saved to memory!", type = "message", duration = 2)
    })

    # Restore all inputs to their defaults
    observeEvent(input$clear_btn, {
      shinyjs::reset("inputs_panel")
    })
  })
}
