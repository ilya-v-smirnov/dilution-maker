# Preparation from dry substance, incl. purity correction.
# Expected values are calculated by hand: mass = C * V * MW / (purity / 100).

test_that("100 mM NaCl, 10 ml: 0.1 mol/L * 0.01 L * 58.44 g/mol = 58.44 mg", {
  r <- calculate_dry_substance("NaCl", 58.44, 100, "mM", 10, "ml")
  expect_equal(r$technical_metadata$mass_g, 0.05844)
  expect_equal(r$instruction_text, "100 mM NaCl in Water\nWeigh 58.4 mg NaCl\nDissolve in 10 ml Water")
  expect_length(r$warnings, 0)
})

test_that("1 M Tris, 100 ml: 1 * 0.1 * 121.14 = 12.114 g", {
  r <- calculate_dry_substance("Tris", 121.14, 1, "M", 100, "ml")
  expect_equal(r$technical_metadata$mass_g, 12.114)
  expect_match(r$instruction_text, "Weigh 12.1 g Tris", fixed = TRUE)
})

test_that("mass concentration target does not use MW: 10 mg/ml BSA, 5 ml = 50 mg", {
  r <- calculate_dry_substance("BSA", 66500, 10, "mg/ml", 5, "ml", solvent = "PBS")
  expect_equal(r$technical_metadata$mass_g, 0.05)
  expect_equal(r$instruction_text, "10 mg/ml BSA in PBS\nWeigh 50 mg BSA\nDissolve in 5 ml PBS")
})

test_that("purity correction: 0.5 M EDTA Na2 (372.24), 100 ml, 99%", {
  # pure mass = 0.5 * 0.1 * 372.24 = 18.612 g; weigh 18.612 / 0.99 = 18.8 g
  r <- calculate_dry_substance("EDTA Na2", 372.24, 0.5, "M", 100, "ml",
                               purity = 99, supplier = "Merck", cat = "E5134")
  tm <- r$technical_metadata
  expect_equal(tm$mass_pure_g, 18.612)
  expect_equal(tm$mass_g, 18.612 / 0.99)
  expect_equal(tm$purity_used, 99)
  expect_equal(r$instruction_text, paste0(
    "0.5 M EDTA Na2 in Water\n",
    "Weigh 18.8 g EDTA Na2 Merck 99% (purity-corrected; 18.6 g at 100%)\n",
    "Dissolve in 100 ml Water"))
  expect_equal(r$input_parameters$cat, "E5134")
})

test_that("purity correction also applies to mass targets: 10 mg/ml at 98%, 1 ml = 10.2 mg", {
  r <- calculate_dry_substance("BSA", 66500, 10, "mg/ml", 1, "ml", purity = 98)
  expect_equal(r$technical_metadata$mass_g, 0.01 / 0.98)
  expect_match(r$instruction_text, "Weigh 10.2 mg BSA 98% (purity-corrected; 10 mg at 100%)", fixed = TRUE)
})

test_that("missing or 100% purity means no correction and no note", {
  na <- calculate_dry_substance("NaCl", 58.44, 100, "mM", 10, "ml", purity = NA)
  expect_equal(na$technical_metadata$purity_used, 100)
  expect_equal(na$technical_metadata$mass_g, na$technical_metadata$mass_pure_g)
  full <- calculate_dry_substance("NaCl", 58.44, 100, "mM", 10, "ml", purity = 100)
  expect_false(grepl("purity-corrected", full$instruction_text))
})

test_that("invalid purity is rejected", {
  expect_error(calculate_dry_substance("X", 100, 1, "mM", 1, "ml", purity = 0), "Purity")
  expect_error(calculate_dry_substance("X", 100, 1, "mM", 1, "ml", purity = 101), "Purity")
  expect_null(validate_purity(NA))
  expect_null(validate_purity(100))
  expect_match(validate_purity(-5), "Purity")
})

test_that("small masses produce balance warnings", {
  # 1 mM, 1 ml, MW 100 -> 0.1 mg; 1 µM, 1 ml, MW 100 -> 0.1 µg
  expect_match(calculate_dry_substance("X", 100, 1, "mM", 1, "ml")$warnings, "analytical balance")
  expect_match(calculate_dry_substance("X", 100, 1, UM, 1, "ml")$warnings, "Extremely small mass")
})

test_that("a purity below 1% is flagged as a likely fraction-for-percent slip", {
  # 0.97 entered instead of 97: the mass is multiplied by 103
  r <- calculate_dry_substance("NaCl", 58.44, 100, "mM", 100, "ml", purity = 0.97)
  expect_equal(r$technical_metadata$mass_g, 0.5844 / 0.0097)
  expect_true(any(grepl("entered in percent", r$warnings)))
  ok <- calculate_dry_substance("NaCl", 58.44, 100, "mM", 100, "ml", purity = 97)
  expect_false(any(grepl("entered in percent", ok$warnings)))
})

test_that("the target concentration of a dry preparation is echoed as entered", {
  r <- calculate_dry_substance("NaCl", 58.44, 154.5, "mM", 1, "L")
  expect_match(r$instruction_text, "^154.5 mM NaCl in Water")
})
