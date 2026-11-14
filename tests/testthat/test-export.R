# Word and Excel export of saved recipes

saved_memory <- function() {
  m <- init_memory()
  m <- save_to_memory(m, calculate_single_dilution(1.7, "mg/ml", 10, UGML, 10, "ml",
                                                   solution_name = "antibody"))
  m <- save_to_memory(m, calculate_dry_substance("EDTA Na2", 372.24, 0.5, "M", 100, "ml",
                                                 purity = 99, supplier = "Merck", cat = "E5134"))
  m
}

test_that("Word export contains every instruction line and optional warnings", {
  f <- withr::local_tempfile(fileext = ".docx")
  export_word_document(saved_memory(), f, keep_warnings = TRUE)
  text <- officer::docx_summary(officer::read_docx(f))$text
  expect_true("Single Dilution" %in% text)
  expect_true("Dry Substance Preparation" %in% text)
  expect_true(paste0("58.8 ", UL, " stock + 10 ml PBS | DF: 170x") %in% text)
  expect_true("Weigh 18.8 g EDTA Na2 Merck 99% (purity-corrected; 18.6 g at 100%)" %in% text)
  expect_true(any(grepl("^Warnings:", text)))

  f2 <- withr::local_tempfile(fileext = ".docx")
  export_word_document(saved_memory(), f2, keep_warnings = FALSE)
  expect_false(any(grepl("^Warnings:", officer::docx_summary(officer::read_docx(f2))$text)))
})

test_that("Excel export has scenario-specific columns", {
  f <- withr::local_tempfile(fileext = ".xlsx")
  export_scenario_excel(saved_memory(), "dry_substance", f)
  x <- readxl::read_excel(f)
  expect_equal(nrow(x), 1)
  expect_true(all(c("Substance", "Supplier", "Purity_pct", "Cat", "MW",
                    "Mass_g", "Mass_100pct_g") %in% names(x)))
  expect_equal(x$Purity_pct, 99)
  expect_equal(x$Mass_100pct_g, 18.612)

  f2 <- withr::local_tempfile(fileext = ".xlsx")
  export_scenario_excel(saved_memory(), "single_dilution", f2)
  y <- readxl::read_excel(f2)
  expect_equal(y$DF, 170)
  expect_equal(y$Solution, "antibody")

  f3 <- withr::local_tempfile(fileext = ".xlsx")
  export_scenario_excel(init_memory(), "multistep_dilution", f3)   # nothing saved
  expect_equal(names(readxl::read_excel(f3)), "Note")
})
