# Tests for module of ts_analysis  ----

#context("Tests for module of ts_analysis")


# Set up test environment

# Prepare test data
tsbl_vars <- load_tsbl_vars(use_online_data = FALSE)

test_that("ts_analysis_server - reactives and output updates", {
  testServer(ts_analysis_server,
    args = list(tsbl_vars = reactive(tsbl_vars)),
    {
      # ts_analysis_server with typical user inputs ====

      # Use to avoid skip message due to empty test, replace it with real tests
      expect_true(TRUE)

      # -- Sample Code for reference --
      # Set input for ts_analysis
      # select_factors <- c("CR", "QR")
      # session$setInputs(
      #   factor_groups = "Financial Risk",
      #   select_factors = select_factors,
      #   ts_analysis = 1 # Non-null value indicating use click button
      # )
      # Check ts_analysis()
      # ts_analysis <- ts_analysis()
      # expect_s3_class(ts_analysis, "data.frame")
      # expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_factors)
      # actual_fields <- names(ts_analysis
      # expect_true(all(actual_fields %in% expect_fields))
      # expect_true(nrow(ts_analysis >= 0)
      # Check output
      # expect_snapshot_output(output$factors_info_table)
      # expect_snapshot_output(output$factors_data_table)

    }
  )
})

test_that("ts_analysis_app - Module App works", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\nts_analysis_app()",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 100)
    })


    # ts_analysis_app with typical user inputs ====

    # Use to avoid skip message due to empty test, replace it with real tests
    # expect_true(TRUE)

    # -- Sample Code for reference --
    # select_factors <- c("CR", "QR")
    # app$setInputs(
    #   `ts_analysis_module-factor_groups` = "Financial Risk",
    #   `ts_analysis_module-factors_in_group` = select_factors,
    #   `ts_analysis_module-ts_analysis` = "click",
    #   timeout_ = 1000 * 10
    # )
    # expect_snapshot_value(app$getAllValues(), style = "json2")
    expect_snapshot_value(app$getAllValues(), style = "serialize")

    app$stop()
  })
})

