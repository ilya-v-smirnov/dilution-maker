# Single-step dilution, C1V1 = C2V2. Expected values are calculated by hand.

test_that("DF = 100 is diluted exactly (approximation only above 100x)", {
  # 1 mg/ml -> 10 µg/ml in 1 ml: DF 100; stock = 1 ml / 100 = 10 µl; diluent = 990 µl
  r <- calculate_single_dilution(1, "mg/ml", 10, UGML, 1, "ml",
                                 diluent = "PBS", solution_name = "IgG")
  s <- r$steps[[1]]
  expect_equal(s$dilution_factor, 100)
  expect_equal(s$stock_volume_L, 1e-5)
  expect_equal(s$diluent_volume_L, 9.9e-4)
  expect_false(r$technical_metadata$approximation_used)
  expect_equal(r$instruction_text,
               paste0("10 ", UGML, " IgG in PBS\n10 ", UL, " stock + 0.99 ml PBS | DF: 100x"))
  expect_length(r$warnings, 0)
})

test_that("approximation ignores the stock volume when DF > 100 (spec example)", {
  # 1.7 mg/ml -> 10 µg/ml in 10 ml: DF 170; stock = 10 ml / 170 = 58.8235 µl; diluent = 10 ml
  r <- calculate_single_dilution(1.7, "mg/ml", 10, UGML, 10, "ml", solution_name = "antibody")
  s <- r$steps[[1]]
  expect_equal(s$dilution_factor, 170)
  expect_equal(s$stock_volume_L, 0.01 / 170)
  expect_equal(s$diluent_volume_L, 0.01)
  expect_true(r$technical_metadata$approximation_used)
  expect_equal(r$instruction_text,
               paste0("10 ", UGML, " antibody in PBS\n58.8 ", UL, " stock + 10 ml PBS | DF: 170x"))
  expect_true(any(grepl("Approximation used", r$warnings)))
})

test_that("exact mode subtracts the stock volume from the final volume", {
  # diluent = 10 ml - 0.0588235 ml = 9.94118 ml
  r <- calculate_single_dilution(1.7, "mg/ml", 10, UGML, 10, "ml", use_approximation = FALSE)
  expect_equal(r$steps[[1]]$diluent_volume_L, 0.01 - 0.01 / 170)
  expect_equal(r$steps[[1]]$diluent_volume_fmt, "9.94 ml")
  expect_false(any(grepl("Approximation", r$warnings)))
})

test_that("molar dilution: 1 M NaCl -> 150 mM in 50 ml", {
  # DF = 6.667; stock = 50 ml * 0.15 = 7.5 ml; diluent = 42.5 ml
  r <- calculate_single_dilution(1, "M", 150, "mM", 50, "ml", diluent = "water")
  s <- r$steps[[1]]
  expect_equal(s$stock_volume_L, 0.0075)
  expect_equal(s$diluent_volume_L, 0.0425)
  expect_equal(r$instruction_text, "150 mM solution in water\n7.5 ml stock + 42.5 ml water | DF: 6.67x")
})

test_that("very large DF and tiny stock volume produce warnings", {
  # 10 mg/ml -> 1 µg/ml in 1 ml: DF 10000; stock = 0.1 µl
  r <- calculate_single_dilution(10, "mg/ml", 1, UGML, 1, "ml")
  expect_equal(r$steps[[1]]$stock_volume_L, 1e-7)
  expect_true(any(grepl("Extremely small volume", r$warnings)))
  expect_true(any(grepl("Multi-Step Planner", r$warnings)))
})

test_that("stock weaker than target gives a negative-diluent warning", {
  # 1 mg/ml -> 2 mg/ml in 1 ml: stock = 2 ml, diluent = -1 ml
  r <- calculate_single_dilution(1, "mg/ml", 2, "mg/ml", 1, "ml")
  expect_equal(r$steps[[1]]$diluent_volume_L, -0.001)
  expect_true(any(grepl("Negative diluent volume", r$warnings)))
})

test_that("manual precision controls significant figures of each volume", {
  r <- calculate_single_dilution(1.7, "mg/ml", 10, UGML, 10, "ml", use_approximation = FALSE,
                                 precision = manual_precision(stock_sig = 4, diluent_sig = 4))
  expect_equal(r$steps[[1]]$stock_volume_fmt, paste("58.82", UL))
  expect_equal(r$steps[[1]]$diluent_volume_fmt, "9.941 ml")
})

test_that("pipette precision rounds to the selected pipette's increment", {
  # stock 58.8235 µl -> P100 (10-100 µl, step 0.2) -> 58.8 µl
  # diluent 10 ml -> above every range -> P1000 (step 2 µl) -> 10 ml
  r <- calculate_single_dilution(1.7, "mg/ml", 10, UGML, 10, "ml",
                                 precision = pipette_precision())
  s <- r$steps[[1]]
  expect_equal(s$stock_pipette, "P100")
  expect_equal(s$diluent_pipette, "P1000")
  expect_match(r$instruction_text_pipettes,
               paste0("58.8 ", UL, " \\(P100\\) stock \\+ 10 ml \\(P1000\\) PBS"))
  expect_false(grepl("P100", r$instruction_text))   # plain text has no pipettes
})

test_that("a volume below the smallest pipette is flagged in pipette mode", {
  r <- calculate_single_dilution(10, "mg/ml", 1, UGML, 1, "ml", precision = pipette_precision())
  expect_true(any(grepl("below the range of the smallest enabled pipette \\(P2\\)", r$warnings)))
})
