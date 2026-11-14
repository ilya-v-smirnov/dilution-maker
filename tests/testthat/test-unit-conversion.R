# Unit conversion: user units <-> internal g/L, mol/L, L

test_that("mass concentrations convert to g/L", {
  expect_equal(mass_conc_to_gL(1, "mg/ml"), 1)      # 1 mg/ml = 1 g/L
  expect_equal(mass_conc_to_gL(1, UGML), 1e-3)
  expect_equal(mass_conc_to_gL(1, "ng/ml"), 1e-6)
  expect_equal(mass_conc_to_gL(1, "pg/ml"), 1e-9)
  expect_equal(mass_conc_to_gL(250, UGML), 0.25)
})

test_that("molar concentrations convert to mol/L", {
  expect_equal(molar_conc_to_molL(1, "M"), 1)
  expect_equal(molar_conc_to_molL(150, "mM"), 0.15)
  expect_equal(molar_conc_to_molL(1, UM), 1e-6)
  expect_equal(molar_conc_to_molL(10, "nM"), 1e-8)
  expect_equal(molar_conc_to_molL(1, "pM"), 1e-12)
})

test_that("volumes convert to L", {
  expect_equal(volume_to_L(250, UL), 2.5e-4)
  expect_equal(volume_to_L(10, "ml"), 0.01)
  expect_equal(volume_to_L(2, "L"), 2)
})

test_that("every unit round-trips through the internal unit", {
  for (u in MASS_CONC_UNITS) expect_equal(gL_to_mass_conc(mass_conc_to_gL(3.7, u), u), 3.7)
  for (u in MOLAR_CONC_UNITS) expect_equal(molL_to_molar_conc(molar_conc_to_molL(3.7, u), u), 3.7)
  for (u in VOLUME_UNITS) expect_equal(L_to_volume(volume_to_L(3.7, u), u), 3.7)
})

test_that("unknown units are rejected", {
  expect_error(mass_conc_to_gL(1, "mg/L"), "Unknown")
  expect_error(molar_conc_to_molL(1, "mol"), "Unknown")
  expect_error(volume_to_L(1, "dl"), "Unknown")
  expect_error(normalize_concentration(1, "%"), "unknown unit")
})

test_that("unit types and compatibility are detected", {
  expect_equal(get_conc_type("mg/ml"), "mass")
  expect_equal(get_conc_type("nM"), "molar")
  expect_equal(get_conc_type("%"), "unknown")
  expect_true(are_compatible_units("mg/ml", "ng/ml"))
  expect_true(are_compatible_units("M", "pM"))
  expect_false(are_compatible_units("mg/ml", "mM"))
})

test_that("best display units follow the thresholds", {
  expect_equal(best_volume_unit(5e-5), UL)     # 50 µl
  expect_equal(best_volume_unit(1e-4), "ml")   # 100 µl -> 0.1 ml
  expect_equal(best_volume_unit(0.5), "ml")
  expect_equal(best_volume_unit(2), "L")
  expect_equal(best_mass_conc_unit(5), "mg/ml")
  expect_equal(best_mass_conc_unit(0.05), UGML)
  expect_equal(best_mass_conc_unit(5e-4), "ng/ml")
  expect_equal(best_mass_conc_unit(5e-7), "pg/ml")
  expect_equal(best_molar_conc_unit(0.15), "mM")
  expect_equal(best_molar_conc_unit(5e-6), UM)
  expect_equal(best_molar_conc_unit(1e-8), "nM")
  expect_equal(best_molar_conc_unit(5e-10), "pM")
})
