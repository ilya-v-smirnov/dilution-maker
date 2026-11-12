# ==============================================================================
# ui_single_dilution.R
# UI and server logic for Single Dilution tab.
# Supports "Use available stock": pulls a substance + variant from the
# stocks DB, autofills and locks Stock concentration & unit.
# ==============================================================================

single_dilution_ui <- function(id) {
  ns <- NS(id)

  use_stock_off <- paste0("!input['", ns("use_stock"), "']")
  use_stock_on  <- paste0("input['", ns("use_stock"), "']")

  tagList(
    fluidRow(
      column(4, id = ns("inputs_panel"),
        wellPanel(
          h4("Input Parameters"),

          # Solution name (default) OR substance + variant selector (when checked)
          conditionalPanel(
            condition = use_stock_off,
            textInput(ns("solution_name"), "Solution name (optional)",
                      placeholder = "e.g. Anti-CD3 antibody")
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
                                   value = 1, min = 0, step = 0.1)),
            column(3, selectInput(ns("stock_unit"), "\u00a0",
                                  choices = ALL_CONC_UNITS, selected = "mg/ml"))
          ),
          fluidRow(
            column(9, numericInput(ns("target_val"), "Target concentration",
                                   value = 10, min = 0, step = 0.1)),
            column(3, selectInput(ns("target_unit"), "\u00a0",
                                  choices = ALL_CONC_UNITS, selected = "\u00b5g/ml"))
          ),
          fluidRow(
            column(9, numericInput(ns("final_vol"), "Final volume",
                                   value = 10, min = 0, step = 0.1)),
            column(3, selectInput(ns("final_vol_unit"), "\u00a0",
                                  choices = VOLUME_UNITS, selected = "ml"))
          ),
          textInput(ns("diluent"), "Diluent", value = "PBS"),
          checkboxInput(ns("use_approx"), "Use approximation when DF > 100", value = TRUE)
        ),
        volume_precision_ui(ns)
      ),
      column(8,
        wellPanel(
          h4("Result"),
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
          h4("Details"),
          tableOutput(ns("details_table"))
        )
      )
    )
  )
}

single_dilution_server <- function(id, memory, stocks_db, pipettes_db) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Volume precision settings (shared panel) ---
    precision <- volume_precision_server(input, output, pipettes_db)

    # --- Populate substance dropdown when stocks change or checkbox toggles ---
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

    # --- Resolve currently-selected stock row (or NULL) ---
    selected_stock <- reactive({
      if (!isTRUE(input$use_stock)) return(NULL)
      if (is.null(input$stock_substance) || !nzchar(input$stock_substance)) return(NULL)
      if (is.null(input$stock_variant) || !nzchar(input$stock_variant)) return(NULL)
      get_stock_by_variant(stocks_db(), input$stock_substance, input$stock_variant)
    })

    # --- Push stock values into the locked fields, toggle disabled state ---
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

    # --- Effective solution name passed into calculation ---
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
      validate_single_dilution(
        input$stock_val, input$stock_unit,
        input$target_val, input$target_unit,
        input$final_vol, input$final_vol_unit
      )
    })

    # --- Reactive calculation ---
    result <- reactive({
      val <- validation()
      req(val$ok)

      # When using a stock, we need at least one valid variant selected
      if (isTRUE(input$use_stock)) {
        req(selected_stock())
      }

      calculate_single_dilution(
        stock_val      = input$stock_val,
        stock_unit     = input$stock_unit,
        target_val     = input$target_val,
        target_unit    = input$target_unit,
        final_vol      = input$final_vol,
        final_vol_unit = input$final_vol_unit,
        diluent        = input$diluent,
        solution_name  = effective_solution_name(),
        use_approximation = input$use_approx,
        precision      = precision()
      )
    })

    output$instruction_text <- renderText({
      req(result())
      recipe_text(result(), precision()$show_pipettes)
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

    output$details_table <- renderTable({
      req(result())
      res <- result()
      s <- res$steps[[1]]
      show_pip <- isTRUE(precision()$show_pipettes)
      vol_txt <- function(text, pipette) {
        if (show_pip) volume_with_pipette(list(text = text, pipette = pipette)) else text
      }
      data.frame(
        Parameter = c("Dilution Factor", "Stock Volume", "Diluent Volume",
                       "Final Volume", "Approximation"),
        Value = c(
          format_df(s$dilution_factor),
          vol_txt(s$stock_volume_fmt, s$stock_pipette),
          vol_txt(s$diluent_volume_fmt, s$diluent_pipette),
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

    # Restore all inputs to their defaults
    observeEvent(input$clear_btn, {
      shinyjs::reset("inputs_panel")
    })
  })
}
