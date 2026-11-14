# MW-based conversion and preparation. Expected values are calculated by hand.

test_that("mass <-> molar conversion uses the molecular weight", {
  # 1 mg/ml IgG (150 kDa) = 1 g/L / 150000 g/mol = 6.667 µM
  expect_equal(convert_with_mw(1, "mg/ml", UM, 150000)$value, 1 / 150000 * 1e6)
  # 10 mM NaCl (58.44) = 0.01 mol/L * 58.44 g/mol = 0.5844 mg/ml
  expect_equal(convert_with_mw(10, "mM", "mg/ml", 58.44)$value, 0.5844)
  # same type: plain unit conversion, MW ignored
  expect_equal(convert_with_mw(1, "mg/ml", UGML, NA)$value, 1000)
})

test_that("IgG 1 mg/ml -> 10 nM in 1 ml", {
  # stock = 6666.67 nM; DF = 666.67 (> 100: approximation); stock = 1 ml / 666.67 = 1.5 µl
  r <- calculate_mw_based_preparation("IgG (human)", 150000, "Common proteins",
                                      1, "mg/ml", 10, "nM", 1, "ml")
  s <- r$steps[[1]]
  expect_equal(r$scenario_type, "mw_based_preparation")
  expect_equal(s$dilution_factor, 1e6 / 150000 / 10 * 1000)
  expect_equal(s$stock_volume_L, 1.5e-6)
  expect_equal(r$instruction_text, paste0(
    "IgG (human)\n",
    "Stock: 1 mg/ml = 6670 nM (MW: 150000 Da) → 10 nM in PBS\n",
    "1.5 ", UL, " stock + 1 ml PBS | DF: 667x"))
  expect_equal(r$input_parameters$mw_source, "Common proteins")
})

test_that("molar stock -> mass target: 1 M NaCl -> 1 mg/ml in 10 ml", {
  # 1 M = 58.44 mg/ml; DF = 58.44; stock = 10 ml / 58.44 = 0.1711 ml; diluent = 9.829 ml
  r <- calculate_mw_based_preparation("NaCl", 58.44, "manual", 1, "M", 1, "mg/ml", 10, "ml",
                                      diluent = "water")
  expect_equal(r$steps[[1]]$stock_volume_L, 0.01 / 58.44)
  expect_match(r$instruction_text, "Stock: 1 M = 58.4 mg/ml (MW: 58.4 Da)", fixed = TRUE)
  expect_match(r$instruction_text, "0.171 ml stock + 9.83 ml water | DF: 58.4x", fixed = TRUE)
})

test_that("same-type stock and target need no conversion line", {
  # 2 mg/ml -> 50 µg/ml in 2 ml: DF 40, stock 50 µl, diluent 1.95 ml
  r <- calculate_mw_based_preparation("BSA", 66500, "Common proteins", 2, "mg/ml", 50, UGML, 2, "ml")
  expect_match(r$instruction_text, paste0("Stock: 2 mg/ml → 50 ", UGML, " in PBS"), fixed = TRUE)
  expect_match(r$instruction_text, paste0("50 ", UL, " stock + 1.95 ml PBS | DF: 40x"), fixed = TRUE)
})
