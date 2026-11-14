# Database files: common files in data/, user-specific files in data/private/;
# missing files are created at start-up and existing ones are never touched.

test_that("files live in the agreed layout", {
  expect_equal(unname(DB_FILES[c("common_substances", "common_proteins")]),
               c("common_substances.xlsx", "common_proteins.xlsx"))
  private <- DB_FILES[c("custom_substances", "custom_proteins", "stocks", "pipettes")]
  expect_true(all(startsWith(private, paste0(PRIVATE_DATA_DIR, "/"))))
})

test_that("init_data_files creates every missing file", {
  d <- local_data_dir()
  created <- suppressMessages(init_data_files(d))
  expect_length(created, 6)
  expect_true(all(file.exists(file.path(d, DB_FILES))))

  # common databases from the built-in sets, private ones empty
  s <- load_substances_db(d)
  expect_equal(nrow(s[["Common substances"]]), nrow(get_builtin_substances()[["Common substances"]]))
  expect_equal(nrow(s[["Custom substances"]]), 0)
  expect_named(readxl::read_excel(db_file_path(d, "custom_substances")),
               c("substance", "MW", "supplier", "purity", "cat"))
  expect_equal(nrow(load_proteins_db(d)[["Custom proteins"]]), 0)
  expect_equal(nrow(load_stocks_db(d)$data), 0)
  expect_named(readxl::read_excel(db_file_path(d, "stocks")),
               c("substance", "date", "conc", "units", "comments"))
  expect_equal(load_pipettes_db(d)$data$name, get_builtin_pipettes()$name)
})

test_that("init_data_files never overwrites existing files", {
  d <- local_data_dir()
  save_stocks_db(list(data = data.frame(substance = "IgG", date = "01.01.2026", conc = 2,
                                        units = "mg/ml", comments = "",
                                        stringsAsFactors = FALSE)), d)
  created <- suppressMessages(init_data_files(d))
  expect_false(db_file_path(d, "stocks") %in% created)
  expect_equal(load_stocks_db(d)$data$substance, "IgG")
  expect_length(suppressMessages(init_data_files(d)), 0)       # second run: nothing to do
})

test_that("saving a custom sheet does not rewrite the common file", {
  d <- local_data_dir()
  suppressMessages(init_data_files(d))
  common <- db_file_path(d, "common_substances")
  before <- tools::md5sum(common)
  db <- load_substances_db(d)
  db[["Custom substances"]] <- data.frame(substance = "X", MW = 1, supplier = "", purity = NA,
                                          cat = "", stringsAsFactors = FALSE)
  save_substances_db(db, d, sheets = "Custom substances")
  expect_equal(tools::md5sum(common), before)
  expect_equal(load_substances_db(d)[["Custom substances"]]$substance, "X")
})

test_that("the app starts without any data files", {
  d <- local_data_dir()
  expect_message(init_data_files(d), "Created database file")
  expect_no_error(load_proteins_db(d))
  expect_no_error(load_pipettes_db(d))
})
