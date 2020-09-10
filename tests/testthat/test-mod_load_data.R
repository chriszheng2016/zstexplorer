# Tests for module of load_data  ----
options(testthat.edition_ignore = TRUE)

context("Tests for module of load_data")


# Set up test environment

test_that("load_data_server - reactives and output updates", {
  testServer(load_data_server,
    #args = list(factors_info = reactive(factors_info)),
    {
      # load_data_server with typical user inputs ====

      # >> load factors ----
      # Set input for load_data
      select_vars <- c("CR", "QR")
      session$setInputs(
        data_type = "factor",
        data_groups = "Financial Risk",
        select_vars = select_vars,
        load_data = 1 # Non-null value indicating use click button
      )
      # Check load_data()
      load_data <- load_data()
      expect_s3_class(load_data, "data.frame")
      expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_vars)
      actual_fields <- names(load_data)
      expect_true(all(actual_fields %in% expect_fields))
      expect_true(nrow(load_data) >= 0)

      # >> load indicators ----
      # Set input for load_data
      select_vars <- c("f030101a", "f030201a")
      session$setInputs(
        data_type = "indicator",
        data_groups = "±ÈÀý½á¹¹",
        select_vars = select_vars,
        load_data = 1 # Non-null value indicating use click button
      )
      # Check load_data()
      load_data <- load_data()
      expect_s3_class(load_data, "data.frame")
      expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_vars)
      actual_fields <- names(load_data)
      expect_true(all(actual_fields %in% expect_fields))
      expect_true(nrow(load_data) >= 0)

    }
  )
})

test_that("load_data_app - Module App works", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\nload_data_app()",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 100)
    })


    # load_data_app with typical user inputs ====

    # -- Sample Code for reference --
    # select_factors <- c("CR", "QR")
    # app$setInputs(
    #   `load_data_module-factor_groups` = "Financial Risk",
    #   `load_data_module-factors_in_group` = select_factors,
    #   `load_data_module-load_data` = "click",
    #   timeout_ = 1000 * 10
    # )
    # expect_snapshot_value(app$getAllValues(), style = "json2")
    # expect_snapshot_value(app$getAllValues(), style = "serialize")

    app$stop()
  })
})

