# Tests for module of cs_analysis  ----


#context("Tests for module of cs_analysis")

#Skip tests if stock db is not ready
skip_if_stock_db_not_ready()

# Set up test environment
tsbl_vars <- readRDS("data/tsbl_vars.rds")

test_that("cs_analysis_server - reactives and output updates", {
  testServer(cs_analysis_server,
    args = list(tsbl_vars = reactive(tsbl_vars)),
    {
      # cs_analysis_server with typical user inputs ====

      # Use to avoid skip message due to empty test, replace it with real tests
      expect_true(TRUE)

      # -- Sample Code for reference --
      # Set input for cs_analysis
      # select_factors <- c("CR", "QR")
      # session$setInputs(
      #   factor_groups = "Financial Risk",
      #   select_factors = select_factors,
      #   cs_analysis = 1 # Non-null value indicating use click button
      # )
      # Check cs_analysis()
      # expect_s3_class(cs_analysis(), "data.frame")
      # expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_factors)
      # actual_fields <- names(cs_analysis())
      # expect_true(all(actual_fields %in% expect_fields))
      # expect_true(nrow(cs_analysis()) >= 0)
      # Check output
      # expect_snapshot_output(output$factors_info_table)
      # expect_snapshot_output(output$factors_data_table)

    }
  )
})

test_that("cs_analysis_app - Module App works", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  withr::local_tempdir("test_cs_analysis_app")
  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\ncs_analysis_app(use_online_data = FALSE)",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 300)
    })


    # cs_analysis_app with typical user inputs ====

    # Use to avoid skip message due to empty test, replace it with real tests
    # expect_true(TRUE)

    # -- Sample Code for reference --
    # select_factors <- c("CR", "QR")
    # app$setInputs(
    #   `cs_analysis_module-factor_groups` = "Financial Risk",
    #   `cs_analysis_module-factors_in_group` = select_factors,
    #   `cs_analysis_module-cs_analysis` = "click",
    #   timeout_ = 1000 * 10
    # )
    expect_snapshot_value(app$getAllValues(), style = "serialize")

    app$stop()
  })
})

