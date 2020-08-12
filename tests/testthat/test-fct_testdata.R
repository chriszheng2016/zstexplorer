# Tests for auxiliary functions about testing data of the project ----

context("Tests for auxiliary functions about testing data")

# Test database is ready ?
dsn <- get_golem_config("database_dsn")
stock_db <- zstmodelr::stock_db(zstmodelr::gta_db, dsn)
suppressMessages(db_ready <- zstmodelr::open_stock_db(stock_db))
suppressMessages(zstmodelr::close_stock_db(stock_db))

test_that("load_factors_info, with various arguments", {

  # load_factors_info on default arguments  ====
  factors_info <- load_factors_info()
  expect_s3_class(factors_info, c("tbl_df", "data.frame"))
  expect_fields <- c("factor_code", "factor_name", "factor_type",
                     "factor_group", "factor_description", "factor_lag_month")
  actual_fields <- names(factors_info)
  expect_equal(expect_fields, actual_fields)


  # load_factors_info on various arguments  ====
  skip_if_not(db_ready, "skip due to database is not ready")
  factors_info <- load_factors_info(use_online_data = TRUE)
  expect_s3_class(factors_info, c("tbl_df", "data.frame"))
  expect_fields <- c("factor_code", "factor_name", "factor_type",
                     "factor_group", "factor_description", "factor_lag_month")
  actual_fields <- names(factors_info)
  expect_equal(expect_fields, actual_fields)

})

test_that("load_tsbl_vars, with various arguments", {

  # load_tsbl_vars on default arguments  ====
  tsbl_vars <- load_tsbl_vars()
  expect_s3_class(tsbl_vars, c("tbl_df", "data.frame"))
  expect_fields <- c("date", "period", "stkcd", "indcd")
  actual_fields <- names(tsbl_vars)
  expect_true(all(expect_fields %in% actual_fields))

  # load_tsbl_vars on various arguments  ====
  skip_if_not(db_ready, "skip due to database is not ready")
  tsbl_vars <- load_tsbl_vars(use_online_data = TRUE)
  expect_s3_class(tsbl_vars, c("tbl_df", "data.frame"))
  expect_fields <- c("date", "period", "stkcd", "indcd")
  actual_fields <- names(tsbl_vars)
  expect_true(all(expect_fields %in% actual_fields))

})
