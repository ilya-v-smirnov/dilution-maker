# ==============================================================================
# ui_volume_precision.R
# Shared "Volume Precision" panel for Single Dilution, Multi-Step Planner and
# MW-Based Preparation. Two modes:
#   - Pipette-based (default): volumes rounded to the increment of the
#     best-matching enabled pipette of the selected pipette group
#     (Databases -> Pipettes); each tab keeps its own group choice for the
#     session (first group by default)
#   - Manual: significant figures for stock and diluent volumes
# Only displayed volumes are rounded; calculations stay exact.
# ==============================================================================

#' Volume Precision panel (call inside a module UI)
volume_precision_ui <- function(ns, panel_id = "precision_panel") {
  mode_is <- function(m) paste0("input['", ns("precision_mode"), "'] == '", m, "'")

  wellPanel(id = ns(panel_id),
    h4("Volume Precision"),
    # extra horizontal gap between the two inline mode options
    tags$style(HTML(paste0("#", ns("precision_mode"),
                           " .radio-inline + .radio-inline { margin-left: 1.5rem; }"))),
    radioButtons(ns("precision_mode"), label = NULL,
                 choices = c("Pipette-based" = "pipette", "Manual" = "manual"),
                 selected = "pipette", inline = TRUE),
    conditionalPanel(
      condition = mode_is("pipette"),
      tags$small(class = "text-muted",
        "Volumes are rounded to the increment of the best-matching enabled pipette ",
        "(Databases \u2192 Pipettes)."),
      radioButtons(ns("pipette_group"), "Pipette group",
                   choices = character(0), selected = character(0)),
      uiOutput(ns("enabled_pipettes_info")),
      checkboxInput(ns("show_pipettes"), "Show pipette in instruction", value = FALSE)
    ),
    conditionalPanel(
      condition = mode_is("manual"),
      tags$small(class = "text-muted",
                 "Significant figures used to round stock and diluent volumes."),
      br(), br(),
      fluidRow(
        column(6, numericInput(ns("stock_precision"), "Stock volume",
                               value = 3, min = 1, max = 10, step = 1)),
        column(6, numericInput(ns("diluent_precision"), "Diluent volume",
                               value = 3, min = 1, max = 10, step = 1))
      )
    )
  )
}

#' Server side of the Volume Precision panel (call inside moduleServer).
#' @return reactive precision list:
#'   list(mode, stock_sig, diluent_sig, group, pipettes, show_pipettes)
volume_precision_server <- function(input, output, pipettes_db) {
  session <- getDefaultReactiveDomain()

  # Group in use: the selected one if it still exists, else the first group
  current_group <- reactive({
    groups <- pipettes_db()$groups
    g <- input$pipette_group
    if (!is.null(g) && g %in% groups) g else if (length(groups) > 0) groups[1] else NULL
  })

  # Keep the group radio buttons in sync with the database (creation order);
  # also re-selects a valid group after Clear resets the panel
  shown_groups <- reactiveVal(NULL)
  observe({
    groups <- pipettes_db()$groups
    g <- input$pipette_group
    valid <- !is.null(g) && g %in% groups
    if (!identical(groups, isolate(shown_groups())) || (!valid && length(groups) > 0)) {
      updateRadioButtons(session, "pipette_group",
                         choices = if (length(groups) > 0) groups else character(0),
                         selected = if (valid) g else if (length(groups) > 0) groups[1] else character(0))
      shown_groups(groups)
    }
  })

  output$enabled_pipettes_info <- renderUI({
    g <- current_group()
    if (is.null(g)) {
      return(tags$div(class = "alert alert-warning py-1 px-2 mt-2 mb-1 small",
        "No pipette groups \u2014 volumes shown with 3 significant figures."))
    }
    p <- enabled_pipettes(pipettes_db(), g)
    if (nrow(p) == 0) {
      return(tags$div(class = "alert alert-warning py-1 px-2 mt-2 mb-1 small",
        "No pipettes enabled \u2014 volumes shown with 3 significant figures."))
    }
    tags$div(class = "small mt-1", tags$strong("Enabled: "),
             paste(p$name, collapse = ", "))
  })

  reactive({
    mode <- input$precision_mode %||% "pipette"
    if (identical(mode, "pipette")) {
      g <- current_group()
      list(mode = "pipette",
           group = g,
           pipettes = if (is.null(g)) empty_pipettes_df() else enabled_pipettes(pipettes_db(), g),
           show_pipettes = isTRUE(input$show_pipettes))
    } else {
      sp <- input$stock_precision
      dp <- input$diluent_precision
      req(is.numeric(sp), is.numeric(dp), !is.na(sp), !is.na(dp), sp >= 1, dp >= 1)
      list(mode = "manual", stock_sig = sp, diluent_sig = dp,
           show_pipettes = FALSE)
    }
  })
}
