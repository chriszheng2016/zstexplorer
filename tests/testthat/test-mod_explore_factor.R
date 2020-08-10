# Tests for module of explore_factor  ----
options(testthat.edition_ignore = TRUE)

context("Tests for module of explore_factor")


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
    writeLines("pkgload::load_all()\nexplore_factor_app()",
               con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 300)
    })


    # explore_factor_app with typical user inputs ====

    # Check initial UI is OK
    expect_snapshot_value(app$getAllValues(), style = "json2")

    app$stop()
  })
})
