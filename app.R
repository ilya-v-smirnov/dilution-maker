# ==============================================================================
# app.R
# Laboratory Dilution & Solution Preparation Application
# ==============================================================================

library(shiny)

# Source all modules
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}

# ==============================================================================
# UI
# ==============================================================================

ui <- navbarPage(
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

  # --- Tab 6: MW Database ---
  tabPanel("MW Database",
    icon = icon("database"),
    fluidRow(
      column(6,
        wellPanel(
          h4("Proteins Database"),
          p(class = "text-muted", "Used in MW-Based Preparation tab."),
          uiOutput("proteins_tables")
        )
      ),
      column(6,
        wellPanel(
          h4("Substances Database"),
          p(class = "text-muted", "Used in Dry Substance tab."),
          uiOutput("substances_tables")
        )
      )
    )
  )
)

# ==============================================================================
# Server
# ==============================================================================

server <- function(input, output, session) {

  # --- Load two separate MW databases ---
  proteins_db <- reactiveVal(load_proteins_db("data"))
  substances_db <- reactiveVal(load_substances_db("data"))

  # --- Session memory ---
  memory <- reactiveVal(init_memory())

  # --- Module servers ---
  single_dilution_server("single_dil", memory)
  multistep_dilution_server("multi_dil", memory)
  mw_based_server("mw_based", memory, proteins_db)
  dry_substance_server("dry_sub", memory, substances_db)
  saved_recipes_server("recipes", memory)

  # --- MW Database display: Proteins ---
  output$proteins_tables <- renderUI({
    db <- proteins_db()
    tables <- lapply(db$sheet_names, function(sn) {
      df <- db[[sn]]
      tagList(
        h5(sn),
        if (nrow(df) == 0) {
          tags$p(class = "text-muted", "No entries.")
        } else {
          df$MW <- format(df$MW, big.mark = ",")
          tableOutput(paste0("prot_tbl_", gsub(" ", "_", sn)))
        },
        hr()
      )
    })
    tagList(tables)
  })

  # Render individual protein tables
  observe({
    db <- proteins_db()
    for (sn in db$sheet_names) {
      local({
        local_sn <- sn
        output_id <- paste0("prot_tbl_", gsub(" ", "_", local_sn))
        output[[output_id]] <- renderTable({
          df <- db[[local_sn]]
          if (nrow(df) == 0) return(data.frame(Note = "No entries."))
          df$MW <- format(df$MW, big.mark = ",")
          df
        }, striped = TRUE, hover = TRUE, width = "100%")
      })
    }
  })

  # --- MW Database display: Substances ---
  output$substances_tables <- renderUI({
    db <- substances_db()
    tables <- lapply(db$sheet_names, function(sn) {
      df <- db[[sn]]
      tagList(
        h5(sn),
        if (nrow(df) == 0) {
          tags$p(class = "text-muted", "No entries.")
        } else {
          df$MW <- format(df$MW, big.mark = ",")
          tableOutput(paste0("subst_tbl_", gsub(" ", "_", sn)))
        },
        hr()
      )
    })
    tagList(tables)
  })

  observe({
    db <- substances_db()
    for (sn in db$sheet_names) {
      local({
        local_sn <- sn
        output_id <- paste0("subst_tbl_", gsub(" ", "_", local_sn))
        output[[output_id]] <- renderTable({
          df <- db[[local_sn]]
          if (nrow(df) == 0) return(data.frame(Note = "No entries."))
          df$MW <- format(df$MW, big.mark = ",")
          df
        }, striped = TRUE, hover = TRUE, width = "100%")
      })
    }
  })
}

# ==============================================================================
# Run
# ==============================================================================

shinyApp(ui = ui, server = server)
