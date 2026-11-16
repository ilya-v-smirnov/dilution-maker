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

test_that("an intermediate keeps a reserve over what the next step takes", {
  # 1 mg/ml -> 1 µg/ml in 15.8 ml, max DF 100: 2 steps of 31.62x.
  # Step 2 takes 15.8 ml / 31.62 = 499.6 µl: a 500 µl intermediate would have
  # to be emptied to the last drop, so it is enlarged to 499.6 * 1.1 -> 550 µl
  r <- plan_multistep_dilution(1, "mg/ml", 1, UGML, 15.8, "ml", max_df_per_step = 100)
  expect_equal(r$technical_metadata$n_steps, 2)
  taken_L <- r$steps[[2]]$stock_volume_L
  expect_equal(taken_L, 15.8e-3 / sqrt(1000))
  expect_equal(r$steps[[1]]$total_volume_L, 550e-6)
  expect_gte(r$steps[[1]]$total_volume_L, taken_L * 1.1)
  expect_true(any(grepl("Intermediate 1 volume increased", r$warnings)))
})

test_that("every intermediate covers the next transfer with a 10% reserve", {
  for (vol_ml in c(0.1, 1, 15.8, 50, 400)) {
    r <- plan_multistep_dilution(10, "mg/ml", 1, "ng/ml", vol_ml, "ml", max_df_per_step = 100)
    s <- r$steps
    for (i in seq_len(length(s) - 1)) {
      expect_gte(s[[i]]$total_volume_L, s[[i + 1]]$stock_volume_L * 1.1 * (1 - 1e-9))
    }
  }
})

test_that("the planner rejects inputs it cannot plan", {
  # max DF 1 used to crash ("vector size cannot be infinite"); a stock below
  # the target used to produce a plan with negative diluent volumes
  expect_error(plan_multistep_dilution(10, "mg/ml", 1, "ng/ml", 10, "ml", max_df_per_step = 1),
               "Max DF per step must be greater than 1")
  expect_error(plan_multistep_dilution(1, "ng/ml", 1, "mg/ml", 10, "ml"),
               "greater than target")
  expect_error(plan_multistep_dilution(1, "mg/ml", 1, "nM", 10, "ml"),
               "both be mass or both be molar")
})

test_that("the target concentration of a plan is echoed as entered", {
  r <- plan_multistep_dilution(10, "mg/ml", 1234, "pg/ml", 10, "ml")
  expect_match(r$instruction_text, "^1234 pg/ml solution in PBS")
})
