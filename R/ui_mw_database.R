# ==============================================================================
# ui_mw_database.R
# Databases tab: left panel selector + add/remove forms,
# right panel shows the currently selected database table.
# Operates on four reactives: proteins_db, substances_db, stocks_db,
# pipettes_db. Saves changes back to the corresponding xlsx files.
# ==============================================================================

# Database selector groups: group title -> c(label = value).
# Add a new group/entry here to make it appear in the selector.
DB_SELECTOR_GROUPS <- list(
  "Dry substances" = c(
    "Common substances" = "subst_common",
    "Custom substances" = "subst_custom"
  ),
  "Proteins" = c(
    "Common proteins"   = "prot_common",
    "Custom proteins"   = "prot_custom",
    "Available stocks"  = "stocks"
  ),
  "Equipment" = c(
    "Pipettes"          = "pipettes"
  )
)

# A single Shiny radio-button input whose options are visually split into
# titled groups. Markup mirrors shiny::radioButtons(), so the standard
# radio input binding reads it and input[[inputId]] behaves as usual.
grouped_radio_buttons <- function(inputId, groups, selected = NULL) {
  all_values <- unlist(groups, use.names = FALSE)
  if (is.null(selected)) selected <- all_values[1]

  option_tag <- function(label, value) {
    tags$div(class = "radio ms-3",
      tags$label(
        tags$input(type = "radio", name = inputId, value = value,
                   checked = if (identical(value, selected)) "checked" else NULL),
        tags$span(label)
      )
    )
  }

  group_tags <- lapply(seq_along(groups), function(i) {
    opts <- groups[[i]]
    tagList(
      tags$div(class = paste("fw-bold text-muted small text-uppercase",
                             if (i > 1) "mt-3" else ""),
               names(groups)[i]),
      mapply(option_tag, names(opts), unname(opts), SIMPLIFY = FALSE)
    )
  })

  tags$div(id = inputId,
           class = "form-group shiny-input-radiogroup shiny-input-container",
           role = "radiogroup",
           tags$div(class = "shiny-options-group", group_tags))
}

mw_database_ui <- function(id) {
  ns <- NS(id)

  sel_js <- paste0("input['", ns("db_selector"), "']")
  is_stocks_js     <- paste0(sel_js, " == 'stocks'")
  is_pipettes_js   <- paste0(sel_js, " == 'pipettes'")
  not_pipettes_js  <- paste0(sel_js, " != 'pipettes'")
  is_mw_js         <- paste0(sel_js, " != 'stocks' && ", sel_js, " != 'pipettes'")
  is_subst_custom_js <- paste0(sel_js, " == 'subst_custom'")

  fluidRow(
    # ----- Left panel: navigation + add/remove -------------------------------
    column(4,
      wellPanel(
        h4("Database"),
        grouped_radio_buttons(ns("db_selector"), DB_SELECTOR_GROUPS,
                              selected = "subst_common")
      ),

      # Pipette groups: choose / create / delete (pipettes only)
      conditionalPanel(
        condition = is_pipettes_js,
        wellPanel(
          h4("Pipette Group"),
          selectInput(ns("pip_group"), label = NULL, choices = character(0)),
          fluidRow(class = "g-2",
            column(8, textInput(ns("new_group_name"), label = NULL,
                                placeholder = "New group name")),
            column(4, actionButton(ns("create_group"), "Create", width = "100%"))
          ),
          actionButton(ns("delete_group"), "Delete group",
                       class = "btn-outline-danger btn-sm", width = "100%"),
          tags$small(class = "text-muted d-block mt-2",
            "Add, remove and enable/disable below apply to the selected group.")
        )
      ),

      wellPanel(
        h4("Add Entry"),
        conditionalPanel(
          condition = not_pipettes_js,
          textInput(ns("new_substance"), "Substance name", placeholder = "e.g. NaCl")
        ),

        # Pipette-specific fields (all volumes in µl)
        conditionalPanel(
          condition = is_pipettes_js,
          textInput(ns("new_pip_name"), "Pipette name", placeholder = "e.g. P20"),
          fluidRow(
            column(6, numericInput(ns("new_pip_min"), "Min volume (\u00b5l)",
                                   value = NA_real_, min = 0, step = 0.1)),
            column(6, numericInput(ns("new_pip_max"), "Max volume (\u00b5l)",
                                   value = NA_real_, min = 0, step = 0.1))
          ),
          numericInput(ns("new_pip_inc"), "Increment (\u00b5l)",
                       value = NA_real_, min = 0, step = 0.001)
        ),

        # MW field for protein/substance databases
        conditionalPanel(
          condition = is_mw_js,
          numericInput(ns("new_mw"), "Molecular weight (Da)",
                       value = NA_real_, min = 0, step = 0.01)
        ),

        # Optional supplier / purity / catalogue number (Custom substances)
        conditionalPanel(
          condition = is_subst_custom_js,
          textInput(ns("new_supplier"), "Supplier (optional)", placeholder = "e.g. Merck"),
          fluidRow(
            column(6, numericInput(ns("new_purity"), "Purity, % (optional)",
                                   value = NA_real_, min = 0, max = 100, step = 0.1)),
            column(6, textInput(ns("new_cat"), "Cat# (optional)",
                                placeholder = "e.g. E5134"))
          ),
          tags$small(class = "text-muted d-block mb-2",
            "Empty purity is treated as 100%. The mass to weigh is corrected for purity.")
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

      # Enable / disable pipettes without deleting them (saved to the file)
      conditionalPanel(
        condition = is_pipettes_js,
        wellPanel(
          h4("Enabled Pipettes"),
          tags$small(class = "text-muted",
            "Only enabled pipettes are used for pipette-based volume precision. ",
            "Changes are saved immediately."),
          checkboxGroupInput(ns("enabled_pipettes"), label = NULL,
                             choices = character(0))
        )
      ),

      wellPanel(
        h4("Remove Entry"),
        selectInput(ns("remove_substance"),
                    label = textOutput(ns("remove_label"), inline = TRUE),
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
                               pipettes_db, data_dir = "data") {
  moduleServer(id, function(input, output, session) {

    # Map the selector value to (type, sheet) pair
    db_info <- reactive({
      switch(input$db_selector,
        "subst_common" = list(type = "substances", sheet = "Common substances"),
        "subst_custom" = list(type = "substances", sheet = "Custom substances"),
        "prot_common"  = list(type = "proteins",   sheet = "Common proteins"),
        "prot_custom"  = list(type = "proteins",   sheet = "Custom proteins"),
        "stocks"       = list(type = "stocks",     sheet = "Available stocks"),
        "pipettes"     = list(type = "pipettes",   sheet = "Pipettes"),
        list(type = "substances", sheet = "Common substances")
      )
    })

    # Get the current dataframe
    # ---- Pipette groups --------------------------------------------------------
    # Selected group if it exists, else the first one (creation order)
    current_pip_group <- reactive({
      groups <- pipettes_db()$groups
      g <- input$pip_group
      if (!is.null(g) && g %in% groups) g else if (length(groups) > 0) groups[1] else NULL
    })

    # Group selector follows the database. A handler that wants a specific
    # group selected after its save (create/delete) sets `pending_group`
    # before committing; the sync below applies it once.
    pending_group <- NULL
    observeEvent(pipettes_db()$groups, {
      groups <- pipettes_db()$groups
      g <- pending_group %||% isolate(input$pip_group)
      pending_group <<- NULL
      if (is.null(g) || !g %in% groups) g <- if (length(groups) > 0) groups[1] else character(0)
      updateSelectInput(session, "pip_group",
                        choices = if (length(groups) > 0) groups else character(0),
                        selected = g)
    }, ignoreNULL = FALSE)

    current_df <- reactive({
      info <- db_info()
      if (info$type == "substances") {
        substances_db()[[info$sheet]]
      } else if (info$type == "proteins") {
        proteins_db()[[info$sheet]]
      } else if (info$type == "pipettes") {
        df <- group_pipettes(pipettes_db(), current_pip_group())
        df[, setdiff(names(df), "group"), drop = FALSE]
      } else {
        stocks_db()$data
      }
    })

    output$current_db_title <- renderText({
      info <- db_info()
      if (info$type == "pipettes") {
        g <- current_pip_group()
        if (is.null(g)) "Pipettes (no groups)" else paste0("Pipettes \u2014 ", g)
      } else {
        info$sheet
      }
    })

    output$current_db_table <- renderTable({
      df <- current_df()
      if (is.null(df) || nrow(df) == 0) return(data.frame(Note = "No entries."))
      out <- df
      if (db_info()$type == "pipettes") {
        for (col in c("min_volume", "max_volume", "increment")) {
          out[[col]] <- vapply(out[[col]], num_plain, character(1))
        }
        out$enabled <- ifelse(out$enabled, "Yes", "No")
        names(out) <- c("name", "min_volume, \u00b5l", "max_volume, \u00b5l",
                        "increment, \u00b5l", "enabled")
        return(out)
      }
      if ("MW" %in% names(out)) out$MW <- format(out$MW, big.mark = ",")
      if ("purity" %in% names(out)) {
        out$purity <- sub("%$", "", format_purity(out$purity))
        names(out)[names(out) == "purity"] <- "purity, %"
        names(out)[names(out) == "cat"] <- "cat#"
      }
      if ("conc" %in% names(out)) out$conc <- format(out$conc)
      out
    }, striped = TRUE, hover = TRUE, width = "100%")

    output$remove_label <- renderText({
      if (db_info()$type == "pipettes") "Pipette" else "Substance"
    })

    # ---- Update Remove dropdown when DB or contents change -----------------
    observe({
      df <- current_df()
      is_pip <- db_info()$type == "pipettes"
      if (is.null(df) || nrow(df) == 0) {
        updateSelectInput(session, "remove_substance",
                          choices = character(0), selected = character(0))
        return()
      }
      # Pipettes keep their volume order; Custom substances are listed by their
      # full label; other databases are alphabetical
      substances <- if (is_pip) {
        df$name
      } else if ("purity" %in% names(df)) {
        sheet_labels(sort_mw_sheet(df))
      } else {
        sort(unique(df$substance))
      }
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
    # Save helper for the pipettes database
    commit_pipettes <- function(df, msg, groups = isolate(pipettes_db()$groups)) {
      new_db <- list(data = sort_pipettes(df, groups), groups = groups,
                     sheet_name = "Pipettes")
      ok <- try(save_pipettes_db(new_db, data_dir), silent = TRUE)
      if (inherits(ok, "try-error")) {
        showNotification(paste("Could not save pipettes file:",
                               attr(ok, "condition")$message),
                         type = "error", duration = 6)
        return(FALSE)
      }
      pipettes_db(new_db)
      if (!is.null(msg)) showNotification(msg, type = "message", duration = 2)
      TRUE
    }

    observeEvent(input$add_btn, {
      info <- db_info()

      if (info$type == "pipettes") {
        # ---- Adding a pipette ----
        pname <- trimws(input$new_pip_name %||% "")
        vmin <- input$new_pip_min
        vmax <- input$new_pip_max
        inc  <- input$new_pip_inc
        bad <- function(x) is.null(x) || is.na(x) || x <= 0
        err <- if (!nzchar(pname)) "Pipette name is required."
          else if (bad(vmin) || bad(vmax) || bad(inc))
            "Min volume, max volume and increment must be positive numbers."
          else if (vmax <= vmin) "Max volume must be greater than min volume."
          else if (inc > vmax) "Increment cannot exceed the max volume."
          else NULL
        df <- pipettes_db()$data
        grp <- current_pip_group()
        if (is.null(err) && is.null(grp)) err <- "Create a pipette group first."
        if (is.null(err) && any(df$group == grp & tolower(df$name) == tolower(pname))) {
          err <- paste0("Pipette '", pname, "' already exists in '", grp,
                        "'. Remove it first.")
        }
        if (!is.null(err)) {
          showNotification(err, type = "error", duration = 4)
          return()
        }
        new_row <- data.frame(group = grp, name = pname,
                              min_volume = vmin, max_volume = vmax,
                              increment = inc, enabled = TRUE,
                              stringsAsFactors = FALSE)
        if (commit_pipettes(rbind(df, new_row), paste0("Pipette '", pname, "' added to '", grp, "'."))) {
          updateTextInput(session, "new_pip_name", value = "")
          for (id in c("new_pip_min", "new_pip_max", "new_pip_inc")) {
            updateNumericInput(session, id, value = NA_real_)
          }
        }
        return()
      }

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
      has_extras <- "purity" %in% names(df)

      if (has_extras) {
        # ---- Custom substances: substance + supplier + purity identify an entry
        supplier_val <- trimws(input$new_supplier %||% "")
        cat_val <- trimws(input$new_cat %||% "")
        purity_val <- input$new_purity
        if (is.null(purity_val)) purity_val <- NA_real_
        purity_err <- validate_purity(purity_val)
        if (!is.null(purity_err)) {
          showNotification(purity_err, type = "error", duration = 4)
          return()
        }
        new_label <- substance_label(name, supplier_val, purity_val)
        if (tolower(new_label) %in% tolower(sheet_labels(df))) {
          showNotification(
            paste0("'", new_label, "' already exists in ", info$sheet,
                   ". Remove the existing entry first."),
            type = "error", duration = 5
          )
          return()
        }
        new_row <- data.frame(substance = name, MW = as.numeric(mw_val),
                              supplier = supplier_val, purity = as.numeric(purity_val),
                              cat = cat_val, stringsAsFactors = FALSE)
      } else {
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
        new_label <- name
        new_row <- data.frame(substance = name, MW = as.numeric(mw_val),
                              stringsAsFactors = FALSE)
      }

      df <- sort_mw_sheet(rbind(df, new_row))
      db[[info$sheet]] <- df
      db <- rebuild_mw_all(db)

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
      if (has_extras) {
        updateTextInput(session, "new_supplier", value = "")
        updateTextInput(session, "new_cat", value = "")
        updateNumericInput(session, "new_purity", value = NA_real_)
      }

      showNotification(paste0("Added '", new_label, "' to ", info$sheet, "."),
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

      if (info$type == "pipettes") {
        df <- pipettes_db()$data
        grp <- current_pip_group()
        idx <- which(df$group %in% grp & df$name == target)
        if (length(idx) == 0) {
          showNotification("Pipette not found.", type = "error", duration = 3)
          return()
        }
        commit_pipettes(df[-idx[1], , drop = FALSE],
                        paste0("Pipette '", target, "' removed from '", grp, "'."))
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
      # Custom substances are selected by their full label
      idx <- which(sheet_labels(df) == target)
      if (length(idx) == 0) {
        showNotification("Entry not found.", type = "error", duration = 3)
        return()
      }
      df <- df[-idx[1], , drop = FALSE]
      db[[info$sheet]] <- df
      db <- rebuild_mw_all(db)

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

    # ---- Create / delete pipette groups -------------------------------------
    observeEvent(input$create_group, {
      gname <- trimws(input$new_group_name %||% "")
      groups <- pipettes_db()$groups
      if (!nzchar(gname)) {
        showNotification("Group name is required.", type = "error", duration = 3)
        return()
      }
      if (tolower(gname) %in% tolower(groups)) {
        showNotification(paste0("Group '", gname, "' already exists."),
                         type = "error", duration = 3)
        return()
      }
      pending_group <<- gname
      if (commit_pipettes(pipettes_db()$data, paste0("Group '", gname, "' created."),
                          groups = c(groups, gname))) {
        updateTextInput(session, "new_group_name", value = "")
      } else {
        pending_group <<- NULL
      }
    })

    observeEvent(input$delete_group, {
      grp <- current_pip_group()
      if (is.null(grp)) return()
      n <- sum(pipettes_db()$data$group == grp)
      showModal(modalDialog(
        title = "Delete pipette group",
        paste0("Delete group '", grp, "'",
               if (n > 0) paste0(" and its ", n, " pipette(s)") else "",
               "? This cannot be undone."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_delete_group"), "Delete", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_delete_group, {
      removeModal()
      grp <- current_pip_group()
      if (is.null(grp)) return()
      df <- pipettes_db()$data
      groups <- setdiff(pipettes_db()$groups, grp)
      pending_group <<- if (length(groups) > 0) groups[1] else NULL
      commit_pipettes(df[df$group != grp, , drop = FALSE],
                      paste0("Group '", grp, "' deleted."), groups = groups)
    })

    # ---- Enabled pipettes (selected group) ----------------------------------
    # Checkbox values are "<group>::<name>" so a late event from a
    # previously shown group can be recognised and ignored
    pip_key <- function(group, name) paste0(group, "::", name)
    shown_enabled_group <- NULL

    # DB -> checkboxes
    observe({
      grp <- current_pip_group()
      df <- group_pipettes(pipettes_db(), grp)
      shown_enabled_group <<- grp
      labels <- pipette_label(df$name, df$min_volume, df$max_volume, df$increment)
      updateCheckboxGroupInput(session, "enabled_pipettes",
                               choices = setNames(pip_key(df$group, df$name), labels),
                               selected = pip_key(df$group, df$name)[df$enabled])
    })

    # Checkboxes -> DB (saved immediately; no-op when already in sync)
    observeEvent(input$enabled_pipettes, {
      grp <- shown_enabled_group
      if (is.null(grp)) return()
      sel <- input$enabled_pipettes %||% character(0)
      if (!all(startsWith(sel, paste0(grp, "::")))) return()   # stale event
      df <- pipettes_db()$data
      in_grp <- df$group == grp
      new_enabled <- pip_key(df$group[in_grp], df$name[in_grp]) %in% sel
      if (identical(new_enabled, df$enabled[in_grp])) return()
      df$enabled[in_grp] <- new_enabled
      commit_pipettes(df, NULL)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  })
}
