# ==============================================================================
# ui_mw_database.R
# MW Database tab: left panel selector + add/remove forms,
# right panel shows the currently selected database table.
# Operates on three reactives: proteins_db, substances_db, stocks_db.
# Saves changes back to the corresponding xlsx files.
# ==============================================================================

mw_database_ui <- function(id) {
  ns <- NS(id)

  is_stocks_js <- paste0("input['", ns("db_selector"), "'] == 'stocks'")
  is_mw_js     <- paste0("input['", ns("db_selector"), "'] != 'stocks'")

  fluidRow(
    # ----- Left panel: navigation + add/remove -------------------------------
    column(4,
      wellPanel(
        h4("Database"),
        radioButtons(ns("db_selector"), label = NULL,
          choices = c(
            "Common substances" = "subst_common",
            "Custom substances" = "subst_custom",
            "Common proteins"   = "prot_common",
            "Custom proteins"   = "prot_custom",
            "Available stocks"  = "stocks"
          ),
          selected = "subst_common"
        )
      ),

      wellPanel(
        h4("Add Entry"),
        textInput(ns("new_substance"), "Substance name", placeholder = "e.g. NaCl"),

        # MW field for protein/substance databases
        conditionalPanel(
          condition = is_mw_js,
          numericInput(ns("new_mw"), "Molecular weight (Da)",
                       value = NA_real_, min = 0, step = 0.01)
        ),

        # Stock-specific fields
        conditionalPanel(
          condition = is_stocks_js,
          textInput(ns("new_date"), "Date", placeholder = "e.g. 12.10.2025"),
          fluidRow(
            column(8, numericInput(ns("new_conc"), "Concentration",
                                   value = NA_real_, min = 0, step = 0.01)),
            column(4, selectInput(ns("new_units"), "\u00a0",
                                  choices = ALL_CONC_UNITS, selected = "mg/ml"))
          ),
          textInput(ns("new_comments"), "Comments (optional)",
                    placeholder = "e.g. AF647 DOL 1.67")
        ),

        actionButton(ns("add_btn"), "Add Entry",
                     class = "btn-success", width = "100%")
      ),

      wellPanel(
        h4("Remove Entry"),
        selectInput(ns("remove_substance"), "Substance",
                    choices = character(0)),
        # Variant selector for stocks (multiple variants per substance)
        conditionalPanel(
          condition = is_stocks_js,
          selectInput(ns("remove_variant"), "Variant",
                      choices = character(0))
        ),
        actionButton(ns("remove_btn"), "Remove Entry",
                     class = "btn-danger", width = "100%")
      )
    ),

    # ----- Right panel: current table ---------------------------------------
    column(8,
      wellPanel(
        h3(textOutput(ns("current_db_title"))),
        tableOutput(ns("current_db_table"))
      )
    )
  )
}

mw_database_server <- function(id, proteins_db, substances_db, stocks_db,
                               data_dir = "data") {
  moduleServer(id, function(input, output, session) {

    # Map the selector value to (type, sheet) pair
    db_info <- reactive({
      switch(input$db_selector,
        "subst_common" = list(type = "substances", sheet = "Common substances"),
        "subst_custom" = list(type = "substances", sheet = "Custom substances"),
        "prot_common"  = list(type = "proteins",   sheet = "Common proteins"),
        "prot_custom"  = list(type = "proteins",   sheet = "Custom proteins"),
        "stocks"       = list(type = "stocks",     sheet = "Available stocks"),
        list(type = "substances", sheet = "Common substances")
      )
    })

    # Get the current dataframe
    current_df <- reactive({
      info <- db_info()
      if (info$type == "substances") {
        substances_db()[[info$sheet]]
      } else if (info$type == "proteins") {
        proteins_db()[[info$sheet]]
      } else {
        stocks_db()$data
      }
    })

    output$current_db_title <- renderText({
      db_info()$sheet
    })

    output$current_db_table <- renderTable({
      df <- current_df()
      if (is.null(df) || nrow(df) == 0) return(data.frame(Note = "No entries."))
      out <- df
      if ("MW" %in% names(out)) out$MW <- format(out$MW, big.mark = ",")
      if ("conc" %in% names(out)) out$conc <- format(out$conc)
      out
    }, striped = TRUE, hover = TRUE, width = "100%")

    # ---- Update Remove dropdown when DB or contents change -----------------
    observe({
      df <- current_df()
      if (is.null(df) || nrow(df) == 0) {
        updateSelectInput(session, "remove_substance",
                          choices = character(0), selected = character(0))
        return()
      }
      substances <- sort(unique(df$substance))
      updateSelectInput(session, "remove_substance",
                        choices = substances,
                        selected = if (length(substances) > 0) substances[1] else character(0))
    })

    # Variant dropdown only for stocks
    observe({
      info <- db_info()
      if (info$type != "stocks") return()
      req(input$remove_substance)
      v <- get_stock_variants(stocks_db(), input$remove_substance)
      updateSelectInput(session, "remove_variant",
                        choices = v$labels,
                        selected = if (length(v$labels) > 0) v$labels[1] else character(0))
    })

    # ---- Add entry ---------------------------------------------------------
    observeEvent(input$add_btn, {
      info <- db_info()
      name <- trimws(input$new_substance %||% "")

      if (!nzchar(name)) {
        showNotification("Substance name is required.", type = "error", duration = 3)
        return()
      }

      if (info$type == "stocks") {
        # ---- Adding to stocks ----
        date_val <- trimws(input$new_date %||% "")
        comments_val <- trimws(input$new_comments %||% "")
        conc_val <- input$new_conc
        units_val <- input$new_units

        if (!nzchar(date_val)) {
          showNotification("Date is required for a stock entry.",
                           type = "error", duration = 3)
          return()
        }
        if (is.null(conc_val) || is.na(conc_val) || conc_val <= 0) {
          showNotification("Concentration must be a positive number.",
                           type = "error", duration = 3)
          return()
        }

        df <- stocks_db()$data
        # Duplicate check: same substance + date + comments
        if (nrow(df) > 0) {
          dup <- which(
            tolower(df$substance) == tolower(name) &
            df$date == date_val &
            df$comments == comments_val
          )
          if (length(dup) > 0) {
            showNotification(
              "A stock with this substance, date and comments already exists. Remove it first.",
              type = "error", duration = 5
            )
            return()
          }
        }

        new_row <- data.frame(
          substance = name,
          date      = date_val,
          conc      = as.numeric(conc_val),
          units     = as.character(units_val),
          comments  = comments_val,
          stringsAsFactors = FALSE
        )
        df <- rbind(df, new_row)
        df <- df[order(tolower(df$substance), df$date), , drop = FALSE]

        new_db <- list(data = df, sheet_name = "Available stocks")
        ok <- try(save_stocks_db(new_db, data_dir), silent = TRUE)
        if (inherits(ok, "try-error")) {
          showNotification(paste("Could not save stocks file:", attr(ok, "condition")$message),
                           type = "error", duration = 6)
          return()
        }
        stocks_db(new_db)

        # Reset add form
        updateTextInput(session, "new_substance", value = "")
        updateTextInput(session, "new_date", value = "")
        updateTextInput(session, "new_comments", value = "")
        updateNumericInput(session, "new_conc", value = NA_real_)

        showNotification("Stock entry added.", type = "message", duration = 2)
        return()
      }

      # ---- Adding to proteins / substances ----
      mw_val <- input$new_mw
      if (is.null(mw_val) || is.na(mw_val) || mw_val <= 0) {
        showNotification("MW must be a positive number.", type = "error", duration = 3)
        return()
      }

      db <- if (info$type == "substances") substances_db() else proteins_db()
      df <- db[[info$sheet]]
      if (nrow(df) > 0) {
        dup <- which(tolower(df$substance) == tolower(name))
        if (length(dup) > 0) {
          showNotification(
            paste0("'", name, "' already exists in ", info$sheet,
                   ". Remove the existing entry first."),
            type = "error", duration = 5
          )
          return()
        }
      }

      new_row <- data.frame(substance = name, MW = as.numeric(mw_val),
                            stringsAsFactors = FALSE)
      df <- rbind(df, new_row)
      df <- df[order(tolower(df$substance)), , drop = FALSE]
      db[[info$sheet]] <- df
      db$all <- do.call(rbind, db[db$sheet_names])

      ok <- try({
        if (info$type == "substances") save_substances_db(db, data_dir)
        else save_proteins_db(db, data_dir)
      }, silent = TRUE)
      if (inherits(ok, "try-error")) {
        showNotification(paste("Could not save file:", attr(ok, "condition")$message),
                         type = "error", duration = 6)
        return()
      }

      if (info$type == "substances") substances_db(db) else proteins_db(db)

      updateTextInput(session, "new_substance", value = "")
      updateNumericInput(session, "new_mw", value = NA_real_)

      showNotification(paste0("Added '", name, "' to ", info$sheet, "."),
                       type = "message", duration = 2)
    })

    # ---- Remove entry ------------------------------------------------------
    observeEvent(input$remove_btn, {
      info <- db_info()
      target <- input$remove_substance
      if (is.null(target) || !nzchar(target)) {
        showNotification("Select an entry to remove.", type = "error", duration = 3)
        return()
      }

      if (info$type == "stocks") {
        variant <- input$remove_variant
        if (is.null(variant) || !nzchar(variant)) {
          showNotification("Select a stock variant to remove.",
                           type = "error", duration = 3)
          return()
        }
        df <- stocks_db()$data
        labels <- mapply(stock_variant_label, df$date, df$comments, USE.NAMES = FALSE)
        idx <- which(df$substance == target & labels == variant)
        if (length(idx) == 0) {
          showNotification("Stock entry not found.", type = "error", duration = 3)
          return()
        }
        df <- df[-idx[1], , drop = FALSE]
        if (nrow(df) > 0) df <- df[order(tolower(df$substance), df$date), , drop = FALSE]

        new_db <- list(data = df, sheet_name = "Available stocks")
        ok <- try(save_stocks_db(new_db, data_dir), silent = TRUE)
        if (inherits(ok, "try-error")) {
          showNotification(paste("Could not save stocks file:", attr(ok, "condition")$message),
                           type = "error", duration = 6)
          return()
        }
        stocks_db(new_db)
        showNotification("Stock entry removed.", type = "message", duration = 2)
        return()
      }

      db <- if (info$type == "substances") substances_db() else proteins_db()
      df <- db[[info$sheet]]
      idx <- which(df$substance == target)
      if (length(idx) == 0) {
        showNotification("Entry not found.", type = "error", duration = 3)
        return()
      }
      df <- df[-idx[1], , drop = FALSE]
      db[[info$sheet]] <- df
      db$all <- do.call(rbind, db[db$sheet_names])

      ok <- try({
        if (info$type == "substances") save_substances_db(db, data_dir)
        else save_proteins_db(db, data_dir)
      }, silent = TRUE)
      if (inherits(ok, "try-error")) {
        showNotification(paste("Could not save file:", attr(ok, "condition")$message),
                         type = "error", duration = 6)
        return()
      }

      if (info$type == "substances") substances_db(db) else proteins_db(db)
      showNotification(paste0("Removed '", target, "' from ", info$sheet, "."),
                       type = "message", duration = 2)
    })
  })
}
