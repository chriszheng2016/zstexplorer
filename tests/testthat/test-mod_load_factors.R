# Tests for module of loading factors  ----
options(testthat.edition_ignore = TRUE)

context("Tests for module of loading factors")

dsn <- get_golem_config("database_dsn")
stock_db <- zstmodelr::stock_db(zstmodelr::gta_db, dsn)
suppressMessages(db_ready <- zstmodelr::open_stock_db(stock_db))
# Skip tests if test dsn is not ready
skip_if_not(db_ready,
  message = sprintf("DSN(%s) is not ready, skip all tests for loading factors", dsn)
)


# Set up test environment

# use fixed data(instead of dynamic data from database) for testing, which make
# validate output easier
factors_info <- readRDS("data/factors_info.rds")
mockery::stub(load_factors_app,
  what = "zstmodelr::get_factors_info",
  how = factors_info
)
ds_factors <- readRDS("data/ds_factors.rds")
mockery::stub(load_factors_server,
  what = "zstmodelr::get_factors",
  how = ds_factors,
  depth = 2
)

test_that("load_factors_server - reactives and output updates", {
  testServer(load_factors_server,
    args = list(factors_info = reactive(factors_info)),
    {
      # load_factors_server with typical user inputs ====
      # Set input for testing loading factors
      select_factors <- c("CR", "CFOR")
      session$setInputs(
        factor_groups = "Financial Risk",
        select_factors = select_factors,
        load_factors = 1 # Non-null value indicating use click button
      )

      # Check load_factors()
      expect_is(load_factors(), "data.frame")
      expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_factors)
      actual_fields <- names(load_factors())
      expect_true(all(actual_fields %in% expect_fields))
      expect_true(nrow(load_factors()) >= 0)

      # Check output
      expect_snapshot_output(output$factors_info_table)
      expect_snapshot_output(output$factors_data_table)
    }
  )
})

test_that("load_factors_app - Module App works", {
  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\nload_factors_app()",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 50000 * 2)
    })

    # load_factors_app with typical user inputs ====
    select_factors <- c("CR", "CFOR")
    app$setInputs(
      `load_factors_ui-factor_groups` = "Financial Risk",
      `load_factors_ui-select_factors` = select_factors,
      `load_factors_ui-load_factors` = "click"
    )
    expect_snapshot_value(app$getAllValues(), style = "json2")

    app$stop()
  })
})
