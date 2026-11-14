# Multi-step dilution planner. Expected values are calculated by hand.

test_that("10 mg/ml -> 1 ng/ml in 10 ml is split into 3 equal steps", {
  # total DF = 1e7; log(1e7)/log(500) = 2.59 -> 3 steps of 1e7^(1/3) = 215.443x
  # intermediates 500 µl: transfers 500/215.443 = 2.32 µl, 2.32 µl; last 10 ml/215.443 = 46.4 µl
  r <- plan_multistep_dilution(10, "mg/ml", 1, "ng/ml", 10, "ml")
  tm <- r$technical_metadata
  expect_equal(tm$n_steps, 3)
  expect_equal(tm$df_per_step, 1e7^(1/3))
  expect_equal(prod(vapply(r$steps, `[[`, numeric(1), "dilution_factor")), 1e7)
  expect_equal(r$steps[[3]]$result_conc_internal, 1e-6)          # 1 ng/ml = 1e-6 g/L

  expect_equal(r$steps[[1]]$stock_volume_fmt, paste("2.32", UL))
  expect_equal(r$steps[[1]]$diluent_volume_fmt, "0.5 ml")        # DF > 100: approximation
  expect_equal(r$steps[[3]]$stock_volume_fmt, paste("46.4", UL))
  expect_equal(r$steps[[3]]$diluent_volume_fmt, "10 ml")
  expect_equal(r$steps[[2]]$source_label, "intermediate 1")

  expect_match(r$instruction_text, "Total DF: 10000000x in 3 step(s)", fixed = TRUE)
  expect_match(r$instruction_text, paste0("2.32 ", UL, " stock + 0.5 ml PBS | DF: 215x"), fixed = TRUE)
  expect_match(r$instruction_text, paste("→ 46.4", UGML), fixed = TRUE)
})

test_that("an exact power of the max DF does not add an extra step", {
  # 1 M -> 1 µM: DF 1e6 = 100^3 -> exactly 3 steps of 100x (exact volumes, DF <= 100)
  # final 1 ml: last step 10 µl + 990 µl; intermediates 500 µl: 5 µl + 495 µl
  # (volumes >= 100 µl are displayed in ml)
  r <- plan_multistep_dilution(1, "M", 1, UM, 1, "ml", max_df_per_step = 100)
  expect_equal(r$technical_metadata$n_steps, 3)
  expect_equal(r$steps[[1]]$stock_volume_fmt, paste("5", UL))
  expect_equal(r$steps[[1]]$diluent_volume_fmt, "0.495 ml")
  expect_equal(r$steps[[3]]$stock_volume_fmt, paste("10", UL))
  expect_equal(r$steps[[3]]$diluent_volume_fmt, "0.99 ml")
})

test_that("a step is added when a transfer would be below the minimum pipetting volume", {
  # 1 mg/ml -> 1 µg/ml in 100 µl with max DF 1000: one step would need 0.1 µl (< 2 µl)
  # two steps of 31.6x: transfers 500/31.6 = 15.8 µl and 100/31.6 = 3.16 µl
  r <- plan_multistep_dilution(1, "mg/ml", 1, UGML, 100, UL,
                               max_df_per_step = 1000, min_pipette_vol_ul = 2)
  expect_equal(r$technical_metadata$n_steps, 2)
  transfers_ul <- vapply(r$steps, function(s) s$stock_volume_L * 1e6, numeric(1))
  expect_true(all(transfers_ul >= 2))
  expect_equal(transfers_ul[2], 100 / sqrt(1000))
})

test_that("an intermediate is enlarged when the next step needs more than the preferred volume", {
  # 10 mg/ml -> 1 µg/ml in 100 ml, max DF 100: 2 steps of 100x
  # step 2 takes 100 ml / 100 = 1 ml > 500 µl -> intermediate 1 = 1 ml * 1.1 = 1.1 ml
  r <- plan_multistep_dilution(10, "mg/ml", 1, UGML, 100, "ml", max_df_per_step = 100)
  expect_equal(r$technical_metadata$n_steps, 2)
  expect_equal(r$steps[[1]]$total_volume_L, 0.0011)
  expect_true(any(grepl("Intermediate 1 volume increased to 1.1 ml", r$warnings)))
})

test_that("ceiling_signif rounds up to the given significant figures", {
  expect_equal(ceiling_signif(1234, 2), 1300)
  expect_equal(ceiling_signif(0.00111, 2), 0.0012)
  expect_equal(ceiling_signif(1.2, 2), 1.2)
})
