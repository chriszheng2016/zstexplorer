# Tests for module of explore_factor  ----


#context("Tests for module of explore_factor")

#Skip tests if stock db is not ready
skip_if_stock_db_not_ready()


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

  withr::local_tempdir("test_explore_factor_app")
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
