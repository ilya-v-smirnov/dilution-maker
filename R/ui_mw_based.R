# ==============================================================================
# ui_mw_based.R
# UI and server logic for MW-Based Preparation tab.
# Uses molecular_weights_proteins.xlsx for MW lookup.
# Supports "Use available stock": variant dropdown filtered by the
# currently-selected substance; autofills and locks Stock concentration.
# ==============================================================================

mw_based_ui <- function(id) {
  ns <- NS(id)

  use_stock_on <- paste0("input['", ns("use_stock"), "']")

  tagList(
    fluidRow(
      column(4,
        wellPanel(
          h4("Substance & Molecular Weight"),
          radioButtons(ns("mw_source"), "MW source:",
            choices = c("Common proteins" = "Common proteins",
                        "Custom proteins" = "Custom proteins",
                        "Manual entry"    = "manual"),
            selected = "Common proteins", inline = TRUE
          ),
          conditionalPanel(
            condition = paste0("input['", ns("mw_source"), "'] != 'manual'"),

            selectizeInput(ns("substance_db"), "Select substance",
              choices = NULL,
              options = list(placeholder = "Type to search..."))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("mw_source"), "'] == 'manual'"),

            textInput(ns("substance_manual"), "Substance name",
                      placeholder = "e.g. My protein"),
            numericInput(ns("mw_manual"), "Molecular weight (Da)",
                         value = NA, min = 0, step = 0.01)
          ),
          uiOutput(ns("mw_display")),

          hr(),
          checkboxInput(ns("use_stock"), "Use available stock", value = FALSE),
          conditionalPanel(
            condition = use_stock_on,
            selectInput(ns("stock_variant"), "Stock variant",
                        choices = character(0)),
            uiOutput(ns("stock_match_msg"))
          )
        ),
        wellPanel(
          h4("Concentration & Volume"),
          fluidRow(
            column(9, numericInput(ns("stock_val"), "Stock concentration",
                                   value = 1, min = 0, step = 0.1)),
            column(3, selectInput(ns("stock_unit"), "\u00a0",
                                  choices = ALL_CONC_UNITS, selected = "mg/ml"))
          ),
          fluidRow(
            column(9, numericInput(ns("target_val"), "Target concentration",
                                   value = 600, min = 0, step = 0.1)),
            column(3, selectInput(ns("target_unit"), "\u00a0",
                                  choices = ALL_CONC_UNITS, selected = "nM"))
          ),
          fluidRow(
            column(9, numericInput(ns("final_vol"), "Final volume",
                                   value = 100, min = 0, step = 0.1)),
            column(3, selectInput(ns("final_vol_unit"), "\u00a0",
                                  choices = VOLUME_UNITS, selected = "\u00b5l"))
          ),
          textInput(ns("diluent"), "Diluent", value = "PBS")
        ),
        wellPanel(
            h4("Volume Precision"),
            tags$small(class = "text-muted",
                       "Significant figures used to round stock and diluent volumes."),
            br(), br(),
            fluidRow(
                column(6, numericInput(ns("stock_precision"),  "Stock volume",   value = 3, min = 1, max = 10, step = 1)),
                column(6, numericInput(ns("diluent_precision"), "Diluent volume", value = 3, min = 1, max = 10, step = 1))
            )
        )
      ),
      column(8,
        wellPanel(
          h4("Result"),
          uiOutput(ns("negative_diluent_ui")),
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

mw_based_server <- function(id, memory, mw_db, stocks_db) {
  moduleServer(id, function(input, output, session) {

    # Update MW source radio buttons from real sheet names
    observe({
      db <- mw_db()
      sheet_names <- db$sheet_names
      choices <- setNames(sheet_names, sheet_names)
      choices <- c(choices, "Manual entry" = "manual")
      updateRadioButtons(session, "mw_source",
                         choices = choices, selected = sheet_names[1], inline = TRUE)
    })

    # Update substance dropdown when DB or selected sheet changes
    observe({
      db <- mw_db()
      src <- input$mw_source
      if (is.null(src) || src == "manual") return()
      choices <- get_substance_names_from_sheet(db, src)
      selected <- if (length(choices) > 0) choices[1] else NULL
      updateSelectizeInput(session, "substance_db", choices = choices, selected = selected)
    })

    # Substance/MW info from the MW database (or manual entry)
    current_mw <- reactive({
      src <- input$mw_source
      if (is.null(src)) return(list(substance = "", mw = NA, source = ""))

      if (src == "manual") {
        list(substance = input$substance_manual %||% "",
             mw = input$mw_manual,
             source = "manual")
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

    # --- Stock variant dropdown filtered by current substance ---
    observe({
      if (!isTRUE(input$use_stock)) return()
      info <- current_mw()
      if (is.null(info$substance) || !nzchar(info$substance)) {
        updateSelectInput(session, "stock_variant",
                          choices = character(0), selected = character(0))
        return()
      }
      v <- get_stock_variants(stocks_db(), info$substance)
      sel <- if (length(v$labels) > 0) v$labels[1] else character(0)
      updateSelectInput(session, "stock_variant",
                        choices = v$labels, selected = sel)
    })

    output$stock_match_msg <- renderUI({
      if (!isTRUE(input$use_stock)) return(NULL)
      info <- current_mw()
      if (is.null(info$substance) || !nzchar(info$substance)) {
        return(tags$div(class = "alert alert-info text-muted small",
          "Select a substance first."))
      }
      v <- get_stock_variants(stocks_db(), info$substance)
      if (length(v$labels) == 0) {
        return(tags$div(class = "alert alert-warning small",
          paste0("No stocks found for '", info$substance,
                 "'. Add one in the MW Database tab.")))
      }
      NULL
    })

    selected_stock <- reactive({
      if (!isTRUE(input$use_stock)) return(NULL)
      info <- current_mw()
      if (is.null(info$substance) || !nzchar(info$substance)) return(NULL)
      if (is.null(input$stock_variant) || !nzchar(input$stock_variant)) return(NULL)
      get_stock_by_variant(stocks_db(), info$substance, input$stock_variant)
    })

    # --- Lock / unlock stock fields, push values from selected stock ---
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

    # --- Effective substance label (with stock suffix when applicable) ---
    effective_substance_label <- reactive({
      info <- current_mw()
      base <- info$substance %||% ""
      if (isTRUE(input$use_stock)) {
        s <- selected_stock()
        if (!is.null(s)) return(augment_name_with_stock(base, s))
      }
      base
    })

    result <- reactive({
      info <- current_mw()
      req(info$mw)
      req(!is.na(info$mw), info$mw > 0)
      req(input$stock_val, input$target_val, input$final_vol)
      req(input$stock_val > 0, input$target_val > 0, input$final_vol > 0)
      req(input$stock_precision, input$diluent_precision)
      req(input$stock_precision >= 1, input$diluent_precision >= 1)

      if (isTRUE(input$use_stock)) {
        # Only proceed when a stock is actually resolved
        req(selected_stock())
      }

      calculate_mw_based_preparation(
        substance         = effective_substance_label(),
        mw                = info$mw,
        mw_source         = info$source,
        stock_val         = input$stock_val,
        stock_unit        = input$stock_unit,
        target_val        = input$target_val,
        target_unit       = input$target_unit,
        final_vol         = input$final_vol,
        final_vol_unit    = input$final_vol_unit,
        diluent           = input$diluent,
        stock_precision   = input$stock_precision,
        diluent_precision = input$diluent_precision
      )
    })

    output$instruction_text <- renderText({
      req(result())
      result()$instruction_text
    })
    
    # Prominent visual alert if diluent volume is negative
    output$negative_diluent_ui <- renderUI({
        req(result())
        s <- result()$steps[[1]]
        if (is.null(s$diluent_volume_L) || s$diluent_volume_L >= 0) return(NULL)
        tags$div(class = "alert alert-danger",
                 tags$strong("Impossible dilution: "),
                 "diluent volume is negative (",
                 format_volume(s$diluent_volume_L),
                 "). Stock concentration is lower than target \u2014 a more concentrated stock is required."
        )
    })

    output$warnings_ui <- renderUI({
      req(result())
      w <- result()$warnings
      # Skip the negative-diluent warning here (already shown in red alert above)
      w <- w[!grepl("^Negative diluent volume", w)]
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
      sp <- res$technical_metadata$stock_precision
      dp <- res$technical_metadata$diluent_precision
      data.frame(
        Parameter = c("Substance", "MW (Da)", "MW Source",
                       "Dilution Factor", "Stock Volume", "Diluent Volume"),
        Value = c(
          res$input_parameters$substance,
          format(res$technical_metadata$mw, big.mark = ","),
          res$technical_metadata$mw_source,
          format_df(s$dilution_factor),
          format_volume_sig(s$stock_volume_L, sp),
          format_volume_sig(s$diluent_volume_L, dp)
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
