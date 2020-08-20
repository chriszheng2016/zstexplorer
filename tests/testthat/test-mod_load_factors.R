# Tests for module of load_factors  ----
options(testthat.edition_ignore = TRUE)

context("Tests for module of load_factors")

dsn <- get_golem_config("database_dsn")
stock_db <- zstmodelr::stock_db(zstmodelr::gta_db, dsn)
suppressMessages(db_ready <- zstmodelr::open_stock_db(stock_db))
# Skip tests if test dsn is not ready
skip_if_not(db_ready,
  message = sprintf("DSN(%s) is not ready, skip all tests for load_factors", dsn)
)
suppressMessages(zstmodelr::close_stock_db(stock_db))

# Set up test environment
factors_info <- readRDS("data/factors_info.rds")

test_that("load_factors_server - reactives and output updates", {
  testServer(load_factors_server,
    args = list(factors_info = reactive(factors_info)),
    {
      # load_factors_server with typical user inputs ====
      # Set input for load_factors
      select_factors <- c("CR", "QR")
      session$setInputs(
        factor_groups = "Financial Risk",
        select_factors = select_factors,
        load_factors = 1 # Non-null value indicating use click button
      )

      # Check load_factors()
      load_factors <- load_factors()
      expect_s3_class(load_factors, "tbl_ts")
      expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_factors)
      actual_fields <- names(load_factors)
      expect_true(all(actual_fields %in% expect_fields))
      expect_equal(tsibble::index_var(load_factors), "date")
      expect_true(all(tsibble::key_vars(load_factors) %in% c("stkcd")))
      expect_true(nrow(load_factors) >= 0)

      # Check output
      expect_snapshot_output(output$factors_info_table)
      expect_snapshot_output(output$factors_data_table)
    }
  )
})

test_that("load_factors_app - Module App works", {

  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\nload_factors_app(use_online_data = FALSE)",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 300)
    })


    # load_factors_app with typical user inputs ====
    select_factors <- c("CR", "QR")

    app$setInputs(
      `load_factors_module-factor_groups` = "Financial Risk",
      `load_factors_module-factors_in_group` = select_factors,
      `load_factors_module-load_factors` = "click",
      timeout_ = 1000 * 10
    )

    #app$expectUpdate("factors_data_table", iotype = "output", timeout = 1000*10)

    expect_snapshot_value(app$getAllValues(), style = "serialize")

    app$stop()
  })
})


