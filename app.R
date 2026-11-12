# ==============================================================================
# app.R
# Laboratory Dilution & Solution Preparation Application
# ==============================================================================

library(shiny)
library(shinyjs)

# Source all modules
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}

# ==============================================================================
# UI
# ==============================================================================

ui <- tagList(
  useShinyjs(),
  navbarPage(
    title = "Lab Dilution Planner",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),

    # --- Tab 1: Single Dilution ---
    tabPanel("Single Dilution",
      icon = icon("flask"),
      single_dilution_ui("single_dil")
    ),

    # --- Tab 2: Multi-Step Planner ---
    tabPanel("Multi-Step Planner",
      icon = icon("layer-group"),
      multistep_dilution_ui("multi_dil")
    ),

    # --- Tab 3: MW-Based Preparation ---
    tabPanel("MW-Based Preparation",
      icon = icon("atom"),
      mw_based_ui("mw_based")
    ),

    # --- Tab 4: Dry Substance ---
    tabPanel("Dry Substance",
      icon = icon("weight-hanging"),
      dry_substance_ui("dry_sub")
    ),

    # --- Tab 5: Saved Recipes ---
    tabPanel("Saved Recipes",
      icon = icon("bookmark"),
      saved_recipes_ui("recipes")
    ),

    # --- Tab 6: Databases (MW, stocks; module in R/ui_mw_database.R) ---
    tabPanel("Databases",
      icon = icon("database"),
      mw_database_ui("mw_db")
    )
  )
)

# ==============================================================================
# Server
# ==============================================================================

server <- function(input, output, session) {

  # --- Databases as reactives (so updates propagate everywhere) ---
  proteins_db   <- reactiveVal(load_proteins_db("data"))
  substances_db <- reactiveVal(load_substances_db("data"))
  stocks_db     <- reactiveVal(load_stocks_db("data"))
  pipettes_db   <- reactiveVal(load_pipettes_db("data"))

  # --- Session memory ---
  memory <- reactiveVal(init_memory())

  # --- Module servers ---
  single_dilution_server("single_dil", memory, stocks_db, pipettes_db)
  multistep_dilution_server("multi_dil", memory, stocks_db, pipettes_db)
  mw_based_server("mw_based", memory, proteins_db, stocks_db, pipettes_db)
  dry_substance_server("dry_sub", memory, substances_db)
  saved_recipes_server("recipes", memory)
  mw_database_server("mw_db", proteins_db, substances_db, stocks_db, pipettes_db,
                     data_dir = "data")
}

# ==============================================================================
# Run
# ==============================================================================

shinyApp(ui = ui, server = server)
