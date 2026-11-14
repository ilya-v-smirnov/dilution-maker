# Display formatting: 3 significant figures, auto-chosen units

test_that("sig3 rounds to 3 significant figures without trailing zeros", {
  expect_equal(sig3(58.44), "58.4")
  expect_equal(sig3(0.0012345), "0.00123")
  expect_equal(sig3(1000), "1000")
  expect_equal(sig3(123456), "123000")
  expect_equal(sig3(2.5), "2.5")
  expect_equal(sig3(0), "0")
  expect_equal(sig3(NA), "NA")
  expect_equal(sig3(Inf), "NA")
})

test_that("sig_n honours the requested significant figures", {
  expect_equal(sig_n(58.8235, 2), "59")
  expect_equal(sig_n(58.8235, 4), "58.82")
  expect_equal(sig_n(58.8235, 0), "58.8")   # invalid -> default 3
})

test_that("volumes are shown in a readable unit", {
  expect_equal(format_volume(5.88235e-5), paste("58.8", UL))
  expect_equal(format_volume(0.0106), "10.6 ml")
  expect_equal(format_volume(1.5), "1.5 L")
  expect_equal(format_volume_sig(9.94118e-3, 4), "9.941 ml")
})

test_that("masses are shown in a readable unit", {
  expect_equal(format_mass(0.05844), "58.4 mg")
  expect_equal(format_mass(3.7224), "3.72 g")
  expect_equal(format_mass(2e-4), paste("200", "µg"))
  expect_equal(format_mass(5e-7), "500 ng")
})

test_that("concentrations and dilution factors are formatted", {
  expect_equal(format_conc_in_unit(0.01, UGML), paste("10", UGML))   # 0.01 g/L
  expect_equal(format_conc_in_unit(0.15, "mM"), "150 mM")
  expect_equal(format_concentration(0.046416, "mass"), paste("46.4", UGML))
  expect_equal(format_concentration(2e-8, "molar"), "20 nM")
  expect_equal(format_df(170), "170x")
  expect_equal(format_df(6.6667), "6.67x")
})
