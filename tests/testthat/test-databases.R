# Databases: load/save round-trips in temporary folders (never data/)

test_that("missing MW files fall back to the built-in databases", {
  d <- local_data_dir()
  s <- load_substances_db(d)
  expect_equal(s$sheet_names, c("Common substances", "Custom substances"))
  expect_equal(lookup_mw("NaCl", s, "Common substances"), 58.44)
  expect_equal(nrow(s[["Custom substances"]]), 0)
  expect_named(s[["Custom substances"]], c("substance", "MW", "supplier", "purity", "cat"))
  p <- load_proteins_db(d)
  expect_equal(lookup_mw("bsa", p), 66500)                     # case-insensitive
  expect_true(is.na(lookup_mw("unobtainium", p)))
})

test_that("custom substances round-trip with supplier, purity and cat#", {
  d <- local_data_dir()
  db <- load_substances_db(d)
  db[["Custom substances"]] <- data.frame(
    substance = c("EDTA Na2", "EDTA Na2"), MW = 372.24,
    supplier = c("Sigma", "Merck"), purity = c(NA, 97), cat = c("", "E5134"),
    stringsAsFactors = FALSE)
  db <- rebuild_mw_all(db)
  save_substances_db(db, d)

  re <- load_substances_db(d)
  expect_equal(get_substance_labels_from_sheet(re, "Custom substances"),
               c("EDTA Na2 Merck 97%", "EDTA Na2 Sigma"))
  e <- lookup_substance_entry(re, "Custom substances", "EDTA Na2 Merck 97%")
  expect_equal(e$purity, 97)
  expect_equal(e$cat, "E5134")
  expect_null(lookup_substance_entry(re, "Custom substances", "nope"))
  expect_equal(nrow(re$all), nrow(re[["Common substances"]]) + 2)
})

test_that("a custom substances file without the new columns still loads", {
  d <- local_data_dir()
  write_db_sheet(data.frame(substance = "EDTA Na2", MW = 372.24),
                 db_file_path(d, "custom_substances"), "Custom substances")
  db <- load_substances_db(d)
  cs <- db[["Custom substances"]]
  expect_equal(cs$supplier, "")
  expect_true(is.na(cs$purity))
  expect_equal(sheet_labels(cs), "EDTA Na2")
  expect_equal(lookup_mw("NaCl", db, "Common substances"), 58.44)   # common file missing -> built-in
})

test_that("substance labels skip empty parts", {
  expect_equal(substance_label(c("A", "A", "A", "A"), c("Merck", "", "Merck", NA),
                               c(97, 99.5, NA, NA)),
               c("A Merck 97%", "A 99.5%", "A Merck", "A"))
})

test_that("available stocks round-trip and resolve variants", {
  d <- local_data_dir()
  expect_equal(nrow(load_stocks_db(d)$data), 0)                 # missing file -> empty
  stocks <- list(data = data.frame(
    substance = c("IgG", "IgG", "BSA"), date = c("02.01.2026", "01.01.2026", "05.01.2026"),
    conc = c(1.2, 2, 10), units = "mg/ml", comments = c("AF647", "", ""),
    stringsAsFactors = FALSE), sheet_name = "Available stocks")
  save_stocks_db(stocks, d)
  re <- load_stocks_db(d)
  expect_equal(get_unique_stock_substances(re), c("BSA", "IgG"))
  v <- get_stock_variants(re, "IgG")
  expect_equal(v$labels, c("01.01.2026", "02.01.2026 - AF647"))
  row <- get_stock_by_variant(re, "IgG", "02.01.2026 - AF647")
  expect_equal(row$conc, 1.2)
  expect_null(get_stock_by_variant(re, "IgG", "missing"))
  expect_equal(augment_name_with_stock("IgG", row), "IgG\t\t\t(02.01.2026 - AF647)")
})

test_that("pipettes round-trip with groups (incl. an empty group)", {
  d <- local_data_dir()
  def <- load_pipettes_db(d)                                    # missing file -> built-in
  expect_equal(nrow(def$data), 6)
  expect_equal(def$groups, DEFAULT_PIPETTE_GROUP)

  db <- def
  db$groups <- c(DEFAULT_PIPETTE_GROUP, "Tissue culture")
  db$data$enabled[db$data$name == "P2"] <- FALSE
  save_pipettes_db(db, d)
  re <- load_pipettes_db(d)
  expect_equal(re$groups, c(DEFAULT_PIPETTE_GROUP, "Tissue culture"))
  expect_false(re$data$enabled[re$data$name == "P2"])
  expect_equal(nrow(enabled_pipettes(re, DEFAULT_PIPETTE_GROUP)), 5)
  expect_equal(nrow(group_pipettes(re, "Tissue culture")), 0)
})
