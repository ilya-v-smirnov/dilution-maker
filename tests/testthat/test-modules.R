# Tab modules end-to-end with shiny::testServer: inputs -> instruction ->
# memory, and Databases add/remove (saved to a temporary data folder).

module_args <- function(d = local_data_dir(parent.frame())) {
  list(memory      = reactiveVal(init_memory()),
       stocks_db   = reactiveVal(load_stocks_db(d)),
       pipettes_db = reactiveVal(load_pipettes_db(d)),
       proteins_db = reactiveVal(load_proteins_db(d)),
       substances_db = reactiveVal(load_substances_db(d)),
       data_dir    = d)
}

test_that("Single Dilution tab calculates and saves to memory", {
  a <- module_args()
  testServer(single_dilution_server,
             args = list(memory = a$memory, stocks_db = a$stocks_db, pipettes_db = a$pipettes_db), {
    session$setInputs(stock_val = 1.7, stock_unit = "mg/ml", target_val = 10, target_unit = UGML,
                      final_vol = 10, final_vol_unit = "ml", diluent = "PBS", solution_name = "Ab",
                      use_approx = TRUE, use_stock = FALSE,
                      precision_mode = "manual", stock_precision = 3, diluent_precision = 3)
    expect_equal(output$instruction_text,
                 paste0("10 ", UGML, " Ab in PBS\n58.8 ", UL, " stock + 10 ml PBS | DF: 170x"))

    # pipette mode with pipette names shown
    session$setInputs(precision_mode = "pipette", pipette_group = DEFAULT_PIPETTE_GROUP,
                      show_pipettes = TRUE)
    expect_match(output$instruction_text, "(P100)", fixed = TRUE)

    session$setInputs(save_btn = 1)
    expect_equal(length(a$memory()$single_dilution), 1)

    # impossible dilution: no instruction, error shown
    session$setInputs(target_val = 5, target_unit = "mg/ml")
    expect_error(output$instruction_text)
    expect_match(as.character(output$errors_ui$html), "dilution is impossible")
  })
})

test_that("Single Dilution tab uses an available stock", {
  a <- module_args()
  a$stocks_db(list(data = data.frame(substance = "IgG", date = "01.01.2026", conc = 2,
                                     units = "mg/ml", comments = "lot 7",
                                     stringsAsFactors = FALSE),
                   sheet_name = "Available stocks"))
  testServer(single_dilution_server,
             args = list(memory = a$memory, stocks_db = a$stocks_db, pipettes_db = a$pipettes_db), {
    session$setInputs(use_stock = TRUE, stock_substance = "IgG", stock_variant = "01.01.2026 - lot 7",
                      stock_val = 2, stock_unit = "mg/ml", target_val = 20, target_unit = UGML,
                      final_vol = 1, final_vol_unit = "ml", diluent = "PBS", use_approx = TRUE,
                      precision_mode = "manual", stock_precision = 3, diluent_precision = 3)
    # 2 mg/ml -> 20 µg/ml in 1 ml: DF 100, 10 µl + 0.99 ml
    expect_equal(output$instruction_text, paste0(
      "20 ", UGML, " IgG\t\t\t(01.01.2026 - lot 7) in PBS\n10 ", UL, " stock + 0.99 ml PBS | DF: 100x"))
  })
})

test_that("Multi-Step Planner tab builds the plan", {
  a <- module_args()
  testServer(multistep_dilution_server,
             args = list(memory = a$memory, stocks_db = a$stocks_db, pipettes_db = a$pipettes_db), {
    session$setInputs(stock_val = 10, stock_unit = "mg/ml", target_val = 1, target_unit = "ng/ml",
                      final_vol = 10, final_vol_unit = "ml", diluent = "PBS", solution_name = "",
                      use_stock = FALSE, max_df = 500, min_pipette = 0.5, intermediate_vol = 500,
                      precision_mode = "manual", stock_precision = 3, diluent_precision = 3)
    expect_match(output$instruction_text, "Total DF: 10000000x in 3 step(s)", fixed = TRUE)
    session$setInputs(save_btn = 1)
    expect_equal(length(a$memory()$multistep_dilution), 1)
  })
})

test_that("MW-Based tab converts with a database MW", {
  a <- module_args()
  testServer(mw_based_server,
             args = list(memory = a$memory, mw_db = a$proteins_db, stocks_db = a$stocks_db,
                         pipettes_db = a$pipettes_db), {
    session$setInputs(mw_source = "Common proteins", substance_db = "IgG (human)",
                      use_stock = FALSE, stock_val = 1, stock_unit = "mg/ml",
                      target_val = 10, target_unit = "nM", final_vol = 1, final_vol_unit = "ml",
                      diluent = "PBS", precision_mode = "manual",
                      stock_precision = 3, diluent_precision = 3)
    expect_match(output$instruction_text, "6670 nM (MW: 150000 Da)", fixed = TRUE)
    expect_match(output$instruction_text, paste0("1.5 ", UL, " stock + 1 ml PBS | DF: 667x"), fixed = TRUE)
  })
})

test_that("Dry Substance tab applies purity for custom and manual entries", {
  a <- module_args()
  db <- isolate(a$substances_db())
  db[["Custom substances"]] <- data.frame(substance = "EDTA Na2", MW = 372.24, supplier = "Merck",
                                          purity = 99, cat = "E5134", stringsAsFactors = FALSE)
  a$substances_db(rebuild_mw_all(db))
  testServer(dry_substance_server, args = list(memory = a$memory, mw_db = a$substances_db), {
    session$setInputs(mw_source = "Custom substances", substance_db = "EDTA Na2 Merck 99%",
                      target_val = 0.5, target_unit = "M", final_vol = 100, final_vol_unit = "ml",
                      solvent = "Water")
    expect_match(output$instruction_text,
                 "Weigh 18.8 g EDTA Na2 Merck 99% (purity-corrected; 18.6 g at 100%)", fixed = TRUE)

    # manual entry: 100 mM NaCl, 100 ml, 99% -> 584.4 mg / 0.99 = 590 mg
    session$setInputs(mw_source = "manual", substance_manual = "NaCl", mw_manual = 58.44,
                      purity_manual = 99, target_val = 100, target_unit = "mM")
    expect_match(output$instruction_text, "Weigh 590 mg NaCl 99%", fixed = TRUE)
    session$setInputs(purity_manual = 120)                          # invalid -> no result
    expect_error(output$instruction_text)
    session$setInputs(purity_manual = NA, save_btn = 1)
    expect_equal(length(a$memory()$dry_substance), 1)
  })
})

test_that("Databases tab adds and removes custom substances and saves the file", {
  a <- module_args()
  testServer(mw_database_server,
             args = list(proteins_db = a$proteins_db, substances_db = a$substances_db,
                         stocks_db = a$stocks_db, pipettes_db = a$pipettes_db,
                         data_dir = a$data_dir), {
    session$setInputs(db_selector = "subst_custom", new_substance = "EDTA Na2", new_mw = 372.24,
                      new_supplier = "Merck", new_purity = 97, new_cat = "E5134", add_btn = 1)
    expect_equal(sheet_labels(a$substances_db()[["Custom substances"]]), "EDTA Na2 Merck 97%")

    # duplicate (case-insensitive) and invalid purity are rejected
    session$setInputs(new_substance = "edta na2", new_mw = 372.24, new_supplier = "merck",
                      new_purity = 97, add_btn = 2)
    session$setInputs(new_substance = "X", new_mw = 10, new_supplier = "", new_purity = 150, add_btn = 3)
    expect_equal(nrow(a$substances_db()[["Custom substances"]]), 1)

    # saved to the (temporary) data folder
    on_disk <- load_substances_db(a$data_dir)[["Custom substances"]]
    expect_equal(on_disk$cat, "E5134")

    session$setInputs(remove_substance = "EDTA Na2 Merck 97%", remove_btn = 1)
    expect_equal(nrow(a$substances_db()[["Custom substances"]]), 0)
  })
})

test_that("Databases tab adds stocks and pipettes", {
  a <- module_args()
  testServer(mw_database_server,
             args = list(proteins_db = a$proteins_db, substances_db = a$substances_db,
                         stocks_db = a$stocks_db, pipettes_db = a$pipettes_db,
                         data_dir = a$data_dir), {
    session$setInputs(db_selector = "stocks", new_substance = "IgG", new_date = "01.01.2026",
                      new_conc = 2, new_units = "mg/ml", new_comments = "", add_btn = 1)
    expect_equal(nrow(load_stocks_db(a$data_dir)$data), 1)

    session$setInputs(db_selector = "pipettes", pip_group = DEFAULT_PIPETTE_GROUP,
                      new_pip_name = "P5000", new_pip_min = 500, new_pip_max = 5000,
                      new_pip_inc = 10, add_btn = 2)
    expect_true("P5000" %in% load_pipettes_db(a$data_dir)$data$name)

    # max <= min is rejected
    session$setInputs(new_pip_name = "Pbad", new_pip_min = 10, new_pip_max = 5,
                      new_pip_inc = 1, add_btn = 3)
    expect_false("Pbad" %in% a$pipettes_db()$data$name)
  })
})
