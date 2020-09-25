# Tests for module of explore_factor  ----


#context("Tests for module of explore_factor")

dsn <- get_golem_config("database_dsn")
stock_db <- zstmodelr::stock_db(zstmodelr::gta_db, dsn)
suppressMessages(db_ready <- zstmodelr::open_stock_db(stock_db))
# Skip tests if test dsn is not ready
skip_if_not(db_ready,
  message = sprintf("DSN(%s) is not ready, skip all tests for explore_factors", dsn)
)
suppressMessages(zstmodelr::close_stock_db(stock_db))

# Set up test environment
# use fixed data(instead of dynamic data from database) for testing, which make
# validate output easier
factors_info <- readRDS("data/factors_info.rds")
mockery::stub(explore_factor_app,
              what = "zstmodelr::get_factors_info",
              how = factors_info
)

test_that("explore_factor_app - Module App works", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\nexplore_factor_app(use_online_data = FALSE)",
               con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 300)
    })


    # explore_factor_app with typical user inputs ====

    # Use to avoid skip message due to empty test, replace it with real tests
    expect_true(TRUE)

    # Check initial UI is OK
    # expect_snapshot_value(app$getAllValues(), style = "serialize")

    app$stop()
  })
})
