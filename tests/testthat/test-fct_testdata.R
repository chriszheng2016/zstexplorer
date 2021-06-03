# Tests for auxiliary functions about testing data of the project ----

# context("Tests for auxiliary functions about testing data")

# Enable parallel process for test
if (is.null(zstmodelr::parallel_status()$cluster)) {
  suppressMessages(zstmodelr::enable_parallel())
  withr::defer({
    suppressMessages({
      zstmodelr::disable_parallel()
    })
  })
}

test_that("load_factors_info, with various arguments", {
  suppressMessages({
    # load_factors_info on default arguments  ====
    factors_info <- load_factors_info()
    expect_s3_class(factors_info, c("tbl_df", "data.frame"))
    expect_fields <- c(
      "factor_code", "factor_name", "factor_type",
      "factor_group", "factor_description", "factor_lag_month"
    )
    actual_fields <- names(factors_info)
    expect_equal(expect_fields, actual_fields)


    # load_factors_info on various arguments  ====
    # Skip tests if stock db is not ready
    skip_if_stock_db_not_ready()
    factors_info <- load_factors_info(use_online_data = TRUE)
    expect_s3_class(factors_info, c("tbl_df", "data.frame"))
    expect_fields <- c(
      "factor_code", "factor_name", "factor_type",
      "factor_group", "factor_description", "factor_lag_month"
    )
    actual_fields <- names(factors_info)
    expect_equal(expect_fields, actual_fields)
  })
})

test_that("load_tsbl_vars, with various arguments", {
  suppressMessages({

    # load_tsbl_vars on default arguments  ====
    tsbl_vars <- load_tsbl_vars()
    expect_s3_class(tsbl_vars, c("tbl_ts", "data.frame"))
    expect_fields <- c("date", "period", "stkcd", "indcd")
    actual_fields <- names(tsbl_vars)
    expect_true(all(expect_fields %in% actual_fields))
    expect_equal(tsibble::index_var(tsbl_vars), "date")
    expect_true(all(tsibble::key_vars(tsbl_vars) %in% c("stkcd", "period")))

    # load_tsbl_vars on various arguments  ====
    # Skip tests if stock db is not ready
    skip_if_stock_db_not_ready()
    tsbl_vars <- load_tsbl_vars(use_online_data = TRUE)
    expect_s3_class(tsbl_vars, c("tbl_ts", "data.frame"))
    expect_fields <- c("date", "period", "stkcd", "indcd")
    actual_fields <- names(tsbl_vars)
    expect_true(all(expect_fields %in% actual_fields))
    expect_equal(tsibble::index_var(tsbl_vars), "date")
    expect_true(all(tsibble::key_vars(tsbl_vars) %in% c("stkcd", "period")))
  })
})

test_that("load_csbl_vars, with various arguments", {
  suppressMessages({
    # load_csbl_vars on default arguments  ====
    csbl_vars <- load_csbl_vars()
    expect_s3_class(csbl_vars, c("tbl_df", "data.frame"))
    expect_fields <- c("id", "indcd", "QR", "CR", "ICR", "CFOR", "TDR", "CFCR")
    actual_fields <- names(csbl_vars)
    expect_true(all(expect_fields %in% actual_fields))

    # load_tsbl_vars on various arguments  ====
    # Skip tests if stock db is not ready
    skip_if_stock_db_not_ready()
    csbl_vars <- load_csbl_vars(use_online_data = TRUE)
    expect_s3_class(csbl_vars, c("tbl_df", "data.frame"))
    expect_fields <- c("id", "indcd", "QR", "CR", "ICR", "CFOR", "TDR", "CFCR")
    actual_fields <- names(csbl_vars)
    expect_true(all(expect_fields %in% actual_fields))
  })
})

test_that("tsbl2csbl, with various arguments", {

  # tsbl2csbl on default arguments  ====
  tsbl_vars <- readRDS("data/tsbl_vars.rds")
  csbl_vars <- tsbl2csbl(tsbl_vars)
  expect_fields <- c("id", setdiff(
    names(tsbl_vars),
    c("date", "stkcd", "period")
  ))

  actual_fields <- names(csbl_vars)
  expect_true(all(expect_fields %in% actual_fields))
})

test_that("load_stock_return, with various arguments", {
  period_type_args <- c("month", "quarter", "year")
  stock_codes <- c(
    "000651", "000333", "600066",
    "000550", "600031", "000157"
  )
  use_online_data_args <- c(TRUE, FALSE)

  # load_stock_return on default arguments  ====
  tsbl_return <- load_stock_return()
  expect_fields <- c("date", "stkcd", "indcd", "period", "return")
  actual_fields <- names(tsbl_return)
  expect_true(all(expect_fields %in% actual_fields))
  expect_true(all(tsbl_return$period %in% c("month")))
  expect_true(
    zstmodelr::is_periodic_dates(tsbl_return$date, freq_rule = "month")
  )
  expect_true(all(tsbl_return$stkcd %in% stock_codes))

  # load_stock_return on various arguments  ====
  for (period_type in period_type_args) {
    for (use_online_data in use_online_data_args) {
      if(use_online_data) skip_if_stock_db_not_ready()
      suppressMessages({
        tsbl_return <- load_stock_return(
          use_online_data = use_online_data,
          period_type = period_type,
          stock_codes = stock_codes
        )
      })

      expect_fields <- c("date", "stkcd", "indcd", "period", "return")
      actual_fields <- names(tsbl_return)
      expect_true(all(expect_fields %in% actual_fields))
      expect_true(all(tsbl_return$period %in% c(period_type)))
      expect_true(
        zstmodelr::is_periodic_dates(tsbl_return$date, freq_rule = period_type)
      )
      expect_true(all(tsbl_return$stkcd %in% stock_codes))
    }
  }
})

test_that("load_market_return, with various arguments", {
  period_type_args <- c("month", "quarter", "year")
  use_online_data_args <- c(TRUE, FALSE)

  # load_market_return on default arguments  ====
  tsbl_return <- load_market_return()
  expect_fields <- c("date", "period", "return")
  actual_fields <- names(tsbl_return)
  expect_true(all(expect_fields %in% actual_fields))
  expect_true(all(tsbl_return$period %in% c("month")))
  expect_true(
    zstmodelr::is_periodic_dates(tsbl_return$date, freq_rule = "month")
  )

  # load_market_return on various arguments  ====
  for (period_type in period_type_args) {
    for (use_online_data in use_online_data_args) {
      if(use_online_data) skip_if_stock_db_not_ready()
      suppressMessages({
        tsbl_return <- load_market_return(
          use_online_data = use_online_data,
          period_type = period_type
        )
      })

      expect_fields <- c("date", "period", "return")
      actual_fields <- names(tsbl_return)
      expect_true(all(expect_fields %in% actual_fields))
      expect_true(all(tsbl_return$period %in% c(period_type)))
      expect_true(
        zstmodelr::is_periodic_dates(tsbl_return$date, freq_rule = period_type)
      )
    }
  }
})

test_that("aggregate_tsbl_vars, with various arguments", {

  # aggregate_tsbl_vars on default arguments  ====

  # Data with period field
  tsbl_vars_with_period <- readRDS("data/tsbl_vars.rds")
  tsbl_vars_aggregate <- aggregate_tsbl_vars(
    tsbl_vars = tsbl_vars_with_period,
    by = "indcd"
  )
  expect_s3_class(tsbl_vars_aggregate, c("tbl_ts", "data.frame"))
  expect_fields <- c(
    c("date", "period", "indcd"),
    setdiff(names(tsbl_vars_with_period), c("date", "period", "stkcd", "indcd"))
  )
  actual_fields <- names(tsbl_vars_aggregate)
  expect_true(all(expect_fields %in% actual_fields))
  expect_equal(tsibble::index_var(tsbl_vars_aggregate), "date")
  expect_true(all(tsibble::key_vars(tsbl_vars_aggregate) %in% c("indcd", "period")))

  # Data without period field
  tsbl_vars_no_period <- tsbl_vars %>%
    dplyr::select(-c("period"))
  tsbl_vars_aggregate <- aggregate_tsbl_vars(
    tsbl_vars = tsbl_vars_no_period,
    by = "indcd"
  )
  expect_s3_class(tsbl_vars_aggregate, c("tbl_ts", "data.frame"))
  expect_fields <- c(
    c("date", "indcd"),
    setdiff(names(tsbl_vars_no_period), c("date", "stkcd", "indcd"))
  )
  actual_fields <- names(tsbl_vars_aggregate)
  expect_true(all(expect_fields %in% actual_fields))
  expect_equal(tsibble::index_var(tsbl_vars_aggregate), "date")
  expect_true(all(tsibble::key_vars(tsbl_vars_aggregate) %in% c("indcd")))
})

test_that("industry_median/industry_mean, with various arguments", {

  # industry_median/industry_mean on default arguments  ====
  tsbl_vars <- readRDS("data/tsbl_vars.rds")

  tsbl_vars_industry <- industry_mean(tsbl_vars)
  expect_s3_class(tsbl_vars_industry, c("tbl_ts", "data.frame"))
  expect_fields <- c(
    c("date", "period", "indcd"),
    setdiff(names(tsbl_vars), c("date", "period", "stkcd", "indcd"))
  )
  actual_fields <- names(tsbl_vars_industry)
  expect_true(all(expect_fields %in% actual_fields))
  expect_equal(tsibble::index_var(tsbl_vars_industry), "date")
  expect_true(all(tsibble::key_vars(tsbl_vars_industry) %in% c("indcd", "period")))

  tsbl_vars_industry <- industry_median(tsbl_vars)
  expect_s3_class(tsbl_vars_industry, c("tbl_ts", "data.frame"))
  expect_fields <- c(
    c("date", "period", "indcd"),
    setdiff(names(tsbl_vars), c("date", "period", "stkcd", "indcd"))
  )
  actual_fields <- names(tsbl_vars_industry)
  expect_true(all(expect_fields %in% actual_fields))
  expect_equal(tsibble::index_var(tsbl_vars_industry), "date")
  expect_true(all(tsibble::key_vars(tsbl_vars_industry) %in% c("indcd", "period")))
})

test_that("periodize_index, with various arguments", {

  # periodize_index on default arguments  ====
  tsbl_vars <- readRDS("data/tsbl_vars.rds")

  tsbl_vars_result <- periodize_index(tsbl_vars)
  period <- unique(tsbl_vars$period)
  switch(period,
    "quarter" = {
      expect_true(tsibble::is_yearquarter(tsbl_vars_result$date))
    },
    "month" = {
      expect_true(tsibble::is_yearmonth(tsbl_vars_result$date))
    },
    "week" = {
      expect_true(tsibble::is_yearweek(tsbl_vars_result$date))
    }
  )

  # periodize_index on various arguments  ====
  for (period in c("quarter", "month", "week")) {
    tsbl_vars_new <- tsbl_vars %>%
      dplyr::mutate(period = {{ period }})

    tsbl_vars_result <- periodize_index(tsbl_vars_new)

    switch(period,
      "quarter" = {
        expect_true(tsibble::is_yearquarter(tsbl_vars_result$date))
      },
      "month" = {
        expect_true(tsibble::is_yearmonth(tsbl_vars_result$date))
      },
      "week" = {
        expect_true(tsibble::is_yearweek(tsbl_vars_result$date))
      }
    )
  }
})
