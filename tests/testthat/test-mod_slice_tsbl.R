# Tests for module of slice_tsbl  ----


# context("Tests for module of slice_tsbl")

# Test database is ready ?
dsn <- get_golem_config("database_dsn")
stock_db <- zstmodelr::stock_db(zstmodelr::gta_db, dsn)
suppressMessages(db_ready <- zstmodelr::open_stock_db(stock_db))
withr::defer({
  suppressMessages(zstmodelr::close_stock_db(stock_db))
})
# Skip tests if test dsn is not ready
skip_if_not(db_ready,
            message = sprintf("DSN(%s) is not ready, skip all tests for load_factors", dsn)
)

# Set up test environment

tsbl_vars <- readRDS("data/tsbl_vars.rds")

test_that("slice_tsbl_server - reactives for output @cross_section", {
  testServer(slice_tsbl_server,
    args = list(
      tsbl_vars = reactive(tsbl_vars),
      slice_type = "cross_section"
    ),
    {
      # Output of slice_tsbl_server with typical user inputs ====

      # >>date_type = "single_period" ----
      # Set input for slice_tsbl
      select_indcd <- c("C38")
      select_stkcd <- c("000651")
      select_vars <- c("CR")
      select_period <- c("quarter")
      select_date_type <- "single_period"
      select_report_date <- "2018-12-31"
      average_by <- "indcd"
      average_method <- "median"

      session$setInputs(
        indcd = select_indcd,
        stkcd = select_stkcd,
        focus_vars = select_vars,
        period = select_period,
        date_type = select_date_type,
        report_date = select_report_date,
        average_by = average_by,
        average_method = average_method,
        apply_filter = 1 # Non-null value indicating use click button
      )

      # Check slice_vars() for slice_result
      slice_tsbl <- slice_vars()
      expect_s3_class(slice_tsbl, "tbl_ts")
      expect_equal(tsibble::index_var(slice_tsbl), "date")
      expect_true(all(tsibble::key_vars(slice_tsbl) %in% c("stkcd", "period")))
      expect_fields <- c(c("date", "period", "stkcd", average_by), select_vars)
      actual_fields <- names(slice_tsbl)
      expect_true(all(actual_fields %in% expect_fields))
      expect_true(all(slice_tsbl$indcd %in% select_indcd))
      expect_true(all(slice_tsbl$stkcd %in% select_stkcd))
      expect_true(all(slice_tsbl$period %in% select_period))
      expect_true(all(slice_tsbl$date == select_report_date))
      expect_true(NROW(slice_tsbl) == 1)

      # Check slice_vars_average() for slice_result
      slice_tsbl <- slice_vars_average()
      expect_s3_class(slice_tsbl, "tbl_ts")
      expect_equal(tsibble::index_var(slice_tsbl), "date")
      expect_true(all(tsibble::key_vars(slice_tsbl) %in% c(average_by, "period")))
      expect_fields <- c(c("date", "period", average_by), select_vars)
      actual_fields <- names(slice_tsbl)
      expect_true(all(actual_fields %in% expect_fields))
      expect_true(all(slice_tsbl$period %in% select_period))
      expect_true(all(slice_tsbl$date == select_report_date))
      expect_true(NROW(slice_tsbl) == 1)
    }
  )
})

test_that("slice_tsbl_server - reactives for output @time_series", {
  testServer(slice_tsbl_server,
    args = list(
      tsbl_vars = reactive(tsbl_vars),
      slice_type = "time_series"
    ),
    {
      # slice_tsbl_server with typical user inputs ====
      # >>date_type = "multi_period" ----
      # Set input for slice_tsbl
      select_indcd <- c("C38")
      select_stkcd <- c("000651")
      select_vars <- c("indcd", "CR")
      select_period <- c("quarter")
      select_date_type <- "multi_period"
      select_start_date <- "2018-12-31"
      select_end_date <- "2019-12-31"
      average_by <- "indcd"
      average_method <- "median"

      session$setInputs(
        indcd = select_indcd,
        stkcd = select_stkcd,
        focus_vars = select_vars,
        period = select_period,
        date_type = select_date_type,
        start_date = select_start_date,
        end_date = select_end_date,
        average_by = average_by,
        average_method = average_method,
        apply_filter = 1 # Non-null value indicating use click button
      )


      # Check slice_vars() for slice_result
      slice_tsbl <- slice_vars()
      expect_s3_class(slice_tsbl, "tbl_ts")
      expect_equal(tsibble::index_var(slice_tsbl), "date")
      expect_true(all(tsibble::key_vars(slice_tsbl) %in% c("stkcd", "period")))
      expect_fields <- c(c("date", "period", "stkcd", average_by), select_vars)
      actual_fields <- names(slice_tsbl)
      expect_true(all(actual_fields %in% expect_fields))
      expect_true(all(slice_tsbl$indcd %in% select_indcd))
      expect_true(all(slice_tsbl$stkcd %in% select_stkcd))
      expect_true(all(slice_tsbl$period %in% select_period))
      expect_true(all((slice_tsbl$date >= select_start_date) &
        (slice_tsbl$date <= select_end_date)))
      expect_true(NROW(slice_tsbl) > 1)

      # Check slice_vars_average() for slice_result
      slice_tsbl <- slice_vars_average()
      expect_s3_class(slice_tsbl, "tbl_ts")
      expect_equal(tsibble::index_var(slice_tsbl), "date")
      expect_true(all(tsibble::key_vars(slice_tsbl) %in% c(average_by, "period")))
      expect_fields <- c(c("date", "period", average_by), select_vars)
      actual_fields <- names(slice_tsbl)
      expect_true(all(actual_fields %in% expect_fields))
      expect_true(all(slice_tsbl$period %in% select_period))
      expect_true(all((slice_tsbl$date >= select_start_date) &
        (slice_tsbl$date <= select_end_date)))
      expect_true(NROW(slice_tsbl) > 1)
    }
  )
})

test_that("slice_tsbl_app - Module App works", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\nslice_tsbl_app(use_online_data = FALSE)",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 300)
    })
    # slice_tsbl_app with typical user inputs ====
    select_indcd <- c("C38")
    select_stkcd <- c("000651")
    select_period <- c("quarter")
    select_vars <- c("CR")
    select_date_type <- "multi_period"
    select_start_date <- "2018-12-31"
    select_end_date <- "2019-12-31"

    app$setInputs(
      `slice_tsbl_module-indcd` = select_indcd,
      `slice_tsbl_module-stkcd` = select_stkcd,
      `slice_tsbl_module-focus_vars` = select_vars,
      `slice_tsbl_module-period` = select_period,
      `slice_tsbl_module-start_date` = select_start_date,
      `slice_tsbl_module-end_date` = select_end_date,
      # `slice_tsbl_module-apply_filter` = "click",
      timeout_ = 1000 * 10
    )
    expect_snapshot_value(app$getAllValues(), style = "serialize")

    app$stop()
  })
})
