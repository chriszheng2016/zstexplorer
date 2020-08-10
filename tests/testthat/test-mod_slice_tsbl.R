# Tests for module of slice_tsbl  ----
options(testthat.edition_ignore = TRUE)

context("Tests for module of slice_tsbl")

dsn <- get_golem_config("database_dsn")
stock_db <- zstmodelr::stock_db(zstmodelr::gta_db, dsn)
suppressMessages(db_ready <- zstmodelr::open_stock_db(stock_db))
# Skip tests if test dsn is not ready
skip_if_not(db_ready,
  message = sprintf("DSN(%s) is not ready, skip all tests for slice_tsbl", dsn)
)


# Set up test environment

# use fixed data(instead of dynamic data from database) for testing, which make
# validate output easier
tsbl_vars <- readRDS("data/tsbl_vars.rds")
ds_factors <- readRDS("data/ds_factors.rds")
mockery::stub(slice_tsbl_app,
  what = "zstmodelr::get_factors",
  how = ds_factors,
  depth = 1
)

test_that("slice_tsbl_server - reactives and output updates", {
  testServer(slice_tsbl_server,
    args = list(tsbl_vars = reactive(tsbl_vars)),
    {
      # slice_tsbl_server with typical user inputs ====
      # >>date_type = "multi_period" ----
      # Set input for slice_tsbl
      select_indcd <- c("C38")
      select_stkcd <- c("000651")
      select_vars <- c("CR")
      select_date_type <- "multi_period"
      select_start_date <- "2018-12-31"
      select_end_date <- "2019-12-31"

      session$setInputs(
        indcd = select_indcd,
        stkcd = select_stkcd,
        focus_vars = select_vars,
        date_type = select_date_type,
        start_date = select_start_date,
        end_date = select_end_date
      )

      # Check slice_dataset()
      slice_tsbl <- invisible(slice_dataset())
      expect_is(slice_tsbl, "tbl_ts")
      expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_vars)
      actual_fields <- names(slice_tsbl)
      expect_true(all(actual_fields %in% expect_fields))
      expect_true(all(slice_tsbl$indcd %in% select_indcd))
      expect_true(all(slice_tsbl$stkcd %in% select_stkcd))
      expect_true(all((slice_tsbl$date >= select_start_date) &
                        (slice_tsbl$date <= select_end_date)))


      # >>date_type = "single_period" ----
      # Set input for slice_tsbl
      select_indcd <- c("C38")
      select_stkcd <- c("000651")
      select_vars <- c("CR")
      select_date_type <- "single_period"
      select_report_date <- "2018-12-31"

      session$setInputs(
        indcd = select_indcd,
        stkcd = select_stkcd,
        focus_vars = select_vars,
        date_type = select_date_type,
        report_date = select_report_date
      )

      # Check slice_dataset()
      slice_tsbl <- slice_dataset()
      expect_is(slice_tsbl, "tbl_ts")
      expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_vars)
      actual_fields <- names(slice_tsbl)
      expect_true(all(actual_fields %in% expect_fields))
      expect_true(all(slice_tsbl$indcd %in% select_indcd))
      expect_true(all(slice_tsbl$stkcd %in% select_stkcd))
      expect_true(all(slice_tsbl$date == select_report_date))
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
    writeLines("pkgload::load_all()\nslice_tsbl_app()",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 300)
    })
    # # slice_tsbl_app with typical user inputs ====
    select_indcd <- c("C38")
    select_stkcd <- c("000651")
    select_vars <- c("CR")
    select_date_type <- "multi_period"
    select_start_date <- "2018-12-31"
    select_end_date <- "2019-12-31"

    app$setInputs(
      `slice_tsbl_module-indcd` = select_indcd,
      `slice_tsbl_module-stkcd` = select_stkcd,
      `slice_tsbl_module-focus_vars` = select_vars,
      `slice_tsbl_module-start_date` = select_start_date,
      `slice_tsbl_module-end_date` = select_end_date
    )
    expect_snapshot_value(app$getAllValues(), style = "json2")

    app$stop()
  })
})
