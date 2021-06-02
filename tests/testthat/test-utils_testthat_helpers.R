# Tests for helper functions for testthat ----

# context("Tests for helper functions for testthat")

skip_if_stock_db_not_ready()

test_that("skip_if_stock_db_not_ready, with various arguments", {

  # skip_if_stock_db_not_ready with default arguments ====

  # Test that a skip happens
  mockery::stub(skip_if_stock_db_not_ready,
                what = "get_golem_config",
                how = "GTA_SQLData1")
  expect_condition(skip_if_stock_db_not_ready(), class = "skip")

  # Test that a skip doesn't happen
  mockery::stub(skip_if_stock_db_not_ready,
                what = "get_golem_config",
                how = "GTA_SQLData")
  expect_condition(skip_if_stock_db_not_ready(), NA, class = "skip")

})
