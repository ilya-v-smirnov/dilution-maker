# Input validation and pipetting warnings

test_that("positive-value checks report missing, zero and negative values", {
  expect_true(validate_positive(1, "X")$ok)
  expect_match(validate_positive(NA, "Stock")$msg, "required")
  expect_match(validate_positive(0, "Stock")$msg, "greater than zero")
  expect_match(validate_positive(-1, "Stock")$msg, "greater than zero")
})

test_that("single dilution validation catches impossible and incompatible inputs", {
  ok <- validate_single_dilution(1, "mg/ml", 10, UGML, 1, "ml")
  expect_true(ok$ok)

  up <- validate_single_dilution(1, "mg/ml", 2, "mg/ml", 1, "ml")      # target > stock
  expect_false(up$ok)
  expect_match(up$errors, "dilution is impossible")

  same <- validate_single_dilution(1, "mg/ml", 1000, UGML, 1, "ml")    # equal concentrations
  expect_false(same$ok)

  mixed <- validate_single_dilution(1, "mg/ml", 10, "mM", 1, "ml")
  expect_false(mixed$ok)
  expect_match(mixed$errors, "Incompatible units")

  missing <- validate_single_dilution(NA, "mg/ml", 10, UGML, 0, "ml")
  expect_length(missing$errors, 2)
})

test_that("MW validation requires a positive number", {
  expect_true(validate_mw(58.44)$ok)
  expect_false(validate_mw(NA)$ok)
  expect_false(validate_mw(0)$ok)
})

test_that("pipetting warnings follow the 0.5 / 1 / 2 µl thresholds", {
  expect_match(pipetting_warnings(volume_to_L(0.3, UL)), "Extremely small")
  expect_match(pipetting_warnings(volume_to_L(0.8, UL)), "Very small")
  expect_match(pipetting_warnings(volume_to_L(1.5, UL)), "Small volume")
  expect_length(pipetting_warnings(volume_to_L(5, UL)), 0)
})

test_that("an unrecognised unit is named in the error", {
  v <- validate_single_dilution(1, "g/L", 1, "ng/ml", 1, "ml")
  expect_false(v$ok)
  expect_match(v$errors, "Stock concentration unit 'g/L' is not recognised")
  expect_false(validate_single_dilution(1, NULL, 1, "ng/ml", 1, "ml")$ok)   # no crash
})
