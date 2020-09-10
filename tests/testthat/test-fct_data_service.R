context("Tests for functions about data service")

# Test database is ready ?
dsn <- get_golem_config("database_dsn")
stock_db <- zstmodelr::stock_db(zstmodelr::gta_db, dsn)
suppressMessages(db_ready <- zstmodelr::open_stock_db(stock_db))
suppressMessages(zstmodelr::close_stock_db(stock_db))
skip_if_not(db_ready,
  message = sprintf("DSN(%s) is not ready, skip all tests for data service", dsn)
)

test_that("stock_db, with various arguments", {

  # stock_db with default arguments ====

  stock_db <- stock_db()
  expect_true(!is.null(stock_db))
  expect_is(stock_db, class = "gta_db")
  expect_gt(length(stock_db$stock_field_list), 0)
  expect_gt(length(stock_db$stock_name_list), 0)
  expect_gt(length(stock_db$industry_name_list), 0)
  expect_gt(length(stock_db$factor_name_list), 0)
  expect_gt(length(stock_db$indicator_name_list), 0)
})

test_that("code2name/name2code, with various arguments", {

  # name2code/code2name with default arguments ====

  # stock code/name
  expect_codes <- c("600031", "600030")
  expect_equal(name2code(code2name(expect_codes)), expect_codes)

  # industry code/name
  expect_codes <- c("C28", "C29")
  expect_equal(name2code(code2name(expect_codes)), expect_codes)

  # factor code/name
  expect_codes <- c("GPM", "OPM")
  expect_equal(name2code(code2name(expect_codes)), expect_codes)


  # indicator code/name
  expect_codes <- c("f050101b", "f050102b")
  expect_equal(name2code(code2name(expect_codes)), expect_codes)

})
