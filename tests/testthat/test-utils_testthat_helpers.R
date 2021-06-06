# Tests for helper functions for testthat ----

# context("Tests for helper functions for testthat")

skip_if_stock_db_not_ready()

test_that("skip_if_stock_db_not_ready, with various arguments", {

  # skip_if_stock_db_not_ready with default arguments ====

  # A skip happens when stock db is able to be connected
  mockery::stub(skip_if_stock_db_not_ready,
                what = "get_golem_config",
                how = "GTA_SQLData1")
  expect_condition(skip_if_stock_db_not_ready(), class = "skip")

  # A skip doesn't happen when stock db is unable to be connected
  mockery::stub(skip_if_stock_db_not_ready,
                what = "get_golem_config",
                how = "GTA_SQLData")
  expect_condition(skip_if_stock_db_not_ready(), NA, class = "skip")

  # Skip happen when "NO_STOCK_DB" is true)
  withr::with_envvar(new = c("NO_STOCK_DB" = "true"),{
    expect_condition(skip_if_stock_db_not_ready(), class = "skip")
  })

  # Skip doesn't happen when "NO_STOCK_DB" is not true)
  withr::with_envvar(new = c("NO_STOCK_DB" = ""),{
    expect_condition(skip_if_stock_db_not_ready(), NA, class = "skip")
  })
})
