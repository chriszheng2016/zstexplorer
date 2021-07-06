# Tests for module of ts_feat_stl_tidyverts  ----

#context("Tests for module of ts_feat_stl_tidyverts")


# Set up test environment

# Prepare test data
tsbl_vars <- load_tsbl_vars(use_online_data = FALSE)
tsbl_vars_average <- industry_mean(tsbl_vars)

focus_stocks <- c(
  "000651", "000333", "600066",
  "000550", "600031", "000157"
)
tsbl_vars <- tsbl_vars %>%
  dplyr::filter(stkcd %in% focus_stocks)

focus_industries <- unique(tsbl_vars$indcd)
tsbl_vars_average <- tsbl_vars_average %>%
  dplyr::filter(.data$indcd %in% focus_industries)

test_that("ts_feat_stl_tidyverts_server - reactives and output updates", {
  testServer(ts_feat_stl_tidyverts_server,
    args = list(
      tsbl_vars = reactive(tsbl_vars),
      tsbl_vars_average = reactive(tsbl_vars_average)
    ),
    {
      # ts_feat_stl_tidyverts_server with typical user inputs ====

      # Use to avoid skip message due to empty test, replace it with real tests
      expect_true(TRUE)

      # -- Sample Code for reference --
      # Set input for ts_feat_stl_tidyverts
      # select_factors <- c("CR", "QR")
      # session$setInputs(
      #   factor_groups = "Financial Risk",
      #   select_factors = select_factors,
      #   ts_feat_stl_tidyverts = 1 # Non-null value indicating use click button
      # )
      # Check ts_feat_stl_tidyverts()
      # ts_feat_stl_tidyverts <- ts_feat_stl_tidyverts()
      # expect_s3_class(ts_feat_stl_tidyverts, "data.frame")
      # expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_factors)
      # actual_fields <- names(ts_feat_stl_tidyverts
      # expect_true(all(actual_fields %in% expect_fields))
      # expect_true(nrow(ts_feat_stl_tidyverts >= 0)
      # Check output
      # expect_snapshot_output(output$factors_info_table)
      # expect_snapshot_output(output$factors_data_table)

    }
  )
})

test_that("ts_feat_stl_tidyverts_app - Module App works", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  withr::local_tempdir("test_ts_feat_stl_tidyverts_app")
  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\nts_feat_stl_tidyverts_app()",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 100)
    })


    # ts_feat_stl_tidyverts_app with typical user inputs ====

    # Use to avoid skip message due to empty test, replace it with real tests
    expect_true(TRUE)

    # -- Sample Code for reference --
    # select_factors <- c("CR", "QR")
    # app$setInputs(
    #   `ts_feat_stl_tidyverts_module-factor_groups` = "Financial Risk",
    #   `ts_feat_stl_tidyverts_module-factors_in_group` = select_factors,
    #   `ts_feat_stl_tidyverts_module-ts_feat_stl_tidyverts` = "click",
    #   timeout_ = 1000 * 10
    # )
    # expect_snapshot_value(app$getAllValues(), style = "json2")
    expect_snapshot_value(app$getAllValues(), style = "serialize")

    app$stop()
  })
})
