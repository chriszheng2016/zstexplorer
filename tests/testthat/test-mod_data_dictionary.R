# Tests for module of data_dictionary  ----


#context("Tests for module of data_dictionary")


# Set up test environment

test_that("data_dictionary_server - reactives and output updates", {
  testServer(data_dictionary_server,
    #args = list(factors_info = reactive(factors_info)),
    {
      # data_dictionary_server with typical user inputs ====

      # Use to avoid skip message due to empty test, replace it with real tests
      expect_true(TRUE)

      # -- Sample Code for reference --
      # Set input for data_dictionary
      # select_factors <- c("CR", "QR")
      # session$setInputs(
      #   factor_groups = "Financial Risk",
      #   select_factors = select_factors,
      #   data_dictionary = 1 # Non-null value indicating use click button
      # )
      # Check data_dictionary()
      # data_dictionary <- data_dictionary()
      # expect_s3_class(data_dictionary, "data.frame")
      # expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_factors)
      # actual_fields <- names(data_dictionary
      # expect_true(all(actual_fields %in% expect_fields))
      # expect_true(nrow(data_dictionary >= 0)
      # Check output
      # expect_snapshot_output(output$factors_info_table)
      # expect_snapshot_output(output$factors_data_table)

    }
  )
})

test_that("data_dictionary_app - Module App works", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\ndata_dictionary_app()",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 100)
    })


    # data_dictionary_app with typical user inputs ====

    # Use to avoid skip message due to empty test, replace it with real tests
    expect_true(TRUE)

    # -- Sample Code for reference --
    # select_factors <- c("CR", "QR")
    # app$setInputs(
    #   `data_dictionary_module-factor_groups` = "Financial Risk",
    #   `data_dictionary_module-factors_in_group` = select_factors,
    #   `data_dictionary_module-data_dictionary` = "click",
    #   timeout_ = 1000 * 10
    # )
    # expect_snapshot_value(app$getAllValues(), style = "json2")
    # expect_snapshot_value(app$getAllValues(), style = "serialize")

    app$stop()
  })
})

