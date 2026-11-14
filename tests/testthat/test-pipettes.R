# Pipette selection and increment rounding (built-in Gilson set:
# P2 0.2-2/0.002, P10 1-10/0.02, P20 2-20/0.02, P100 10-100/0.2,
# P200 20-200/0.2, P1000 100-1000/2 µl)

test_that("the most precise covering pipette is chosen", {
  p <- get_builtin_pipettes()
  expect_equal(select_pipette(15, p)$name, "P20")    # P20 (0.02) beats P100 (0.2)
  expect_equal(select_pipette(5, p)$name, "P10")     # P10 vs P20 tie on 0.02 -> narrower range
  expect_equal(select_pipette(150, p)$name, "P200")  # P200 (0.2) beats P1000 (2)
  expect_equal(select_pipette(15, p)$status, "in_range")
})

test_that("volumes outside every range are handled", {
  p <- get_builtin_pipettes()
  above <- select_pipette(1500, p)
  expect_equal(above$name, "P1000")
  expect_equal(above$status, "above_max")
  below <- select_pipette(0.1, p)
  expect_equal(below$name, "P2")
  expect_equal(below$status, "below_min")
  expect_null(select_pipette(10, empty_pipettes_df()))
})

test_that("a volume in a gap uses the nearest smaller pipette", {
  p <- get_builtin_pipettes()
  p <- p[p$name %in% c("P10", "P200"), ]            # gap between 10 and 20 µl
  g <- select_pipette(15, p)
  expect_equal(g$name, "P10")
  expect_equal(g$status, "gap")
})

test_that("volumes are rounded to the pipette increment", {
  expect_equal(round_to_increment(96.3, 0.2), 96.4)
  expect_equal(round_to_increment(58.8235, 0.2), 58.8)
  expect_equal(round_to_increment(3.4801, 0.02), 3.48)
  expect_equal(round_to_increment(0.03, 0.02), 0.04)  # exact half rounds up
  expect_equal(format_volume_increment(58.8235e-6, 0.02), paste("58.82", UL))
  expect_equal(format_volume_increment(0.01, 2), "10 ml")
})

test_that("pipette table cleaning fills defaults and drops invalid rows", {
  raw <- data.frame(name = c("P10", "Pbad", "P20"), min_volume = c(1, 1, 2),
                    max_volume = c(10, 10, 20), increment = c(0.02, 0, 0.02),
                    enabled = c("yes", "TRUE", "no"), stringsAsFactors = FALSE)
  df <- clean_pipettes_df(raw)
  expect_equal(df$name, c("P10", "P20"))             # zero increment dropped
  expect_equal(df$group, rep(DEFAULT_PIPETTE_GROUP, 2))
  expect_equal(df$enabled, c(TRUE, FALSE))
  expect_null(clean_pipettes_df(data.frame(x = 1)))
})

test_that("pipette labels are human-readable", {
  expect_equal(pipette_label("P20", 2, 20, 0.02), paste0("P20 (2–20 ", UL, ", step 0.02 ", UL, ")"))
})
