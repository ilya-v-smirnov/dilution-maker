# Saved Recipes memory: save, remove, reorder, sort

make_recipe <- function(name, target, unit, vol_ml) {
  calculate_single_dilution(1000, unit, target, unit, vol_ml, "ml", solution_name = name)
}

test_that("results are saved per scenario with unique ids", {
  m <- init_memory()
  r <- make_recipe("A", 1, "mM", 1)
  m <- save_to_memory(m, r)
  m <- save_to_memory(m, r)
  expect_equal(length(m$single_dilution), 2)
  expect_equal(count_saved(m), 2)
  ids <- vapply(m$single_dilution, `[[`, character(1), "memory_id")
  expect_false(anyDuplicated(ids) > 0)
  expect_s3_class(m$single_dilution[[1]]$saved_at, "POSIXct")
})

test_that("unknown scenarios are refused with a warning", {
  bad <- list(scenario_type = "nonsense")
  expect_warning(m <- save_to_memory(init_memory(), bad), "Unknown scenario")
  expect_equal(count_saved(m), 0)
})

test_that("recipes are removed by id and by index", {
  m <- init_memory()
  for (n in c("A", "B", "C")) m <- save_to_memory(m, make_recipe(n, 1, "mM", 1))
  id_b <- m$single_dilution[[2]]$memory_id
  m <- remove_from_memory_by_id(m, id_b)
  expect_equal(vapply(m$single_dilution, `[[`, character(1), "solution_name"), c("A", "C"))
  m <- remove_from_memory(m, "single_dilution", 1)
  expect_equal(m$single_dilution[[1]]$solution_name, "C")
  expect_equal(count_saved(clear_all_memory()), 0)
})

test_that("Up/Down moves a recipe and ignores out-of-range moves", {
  m <- init_memory()
  for (n in c("A", "B", "C")) m <- save_to_memory(m, make_recipe(n, 1, "mM", 1))
  names_of <- function(m) vapply(m$single_dilution, `[[`, character(1), "solution_name")
  id_c <- m$single_dilution[[3]]$memory_id
  m <- move_recipe(m, id_c, -1)
  expect_equal(names_of(m), c("A", "C", "B"))
  id_a <- m$single_dilution[[1]]$memory_id
  expect_equal(names_of(move_recipe(m, id_a, -1)), c("A", "C", "B"))
})

test_that("sorting by name, target and volume", {
  m <- init_memory()
  m <- save_to_memory(m, make_recipe("beta",  5, "mg/ml", 2))
  m <- save_to_memory(m, make_recipe("",      1, "mM",    1))
  m <- save_to_memory(m, make_recipe("Alpha", 2, "mM",    3))
  names_of <- function(m) vapply(m$single_dilution, `[[`, character(1), "solution_name")

  by_name <- sort_scenario_recipes(m, "single_dilution", list(list(key = "name")))
  expect_equal(names_of(by_name), c("Alpha", "beta", ""))      # empty names last

  # molar block first (1 mM, 2 mM), then mass (5 mg/ml)
  by_target <- sort_scenario_recipes(m, "single_dilution", list(list(key = "target")))
  expect_equal(names_of(by_target), c("", "Alpha", "beta"))

  by_vol_desc <- sort_scenario_recipes(m, "single_dilution",
                                       list(list(key = "volume", desc = TRUE)))
  expect_equal(names_of(by_vol_desc), c("Alpha", "beta", ""))
})
