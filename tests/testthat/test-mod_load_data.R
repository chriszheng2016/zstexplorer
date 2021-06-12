# Tests for module of load_data  ----


# context("Tests for module of load_data")

#Skip tests if stock db is not ready
skip_if_stock_db_not_ready()


# Set up test environment

test_that("load_data_server - reactives and output updates", {
  testServer(load_data_server, {
    suppressMessages({

      # reactive with typical user inputs ====

      # >> load_vars() ----

      # Load factors
      # Set input for load_var()
      select_factors <- c("CR", "QR")
      session$setInputs(
        vars_type = "factor",
        vars_groups = "Financial Risk",
        select_vars = paste0(select_factors, collapse = ","),
        load_vars = 1 # Non-null value indicating use click button
      )
      # Check load_vars()
      load_vars <- load_vars()
      expect_s3_class(load_vars, "tbl_ts")
      expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_factors)
      actual_fields <- names(load_vars)
      expect_true(all(actual_fields %in% expect_fields))
      expect_true(nrow(load_vars) >= 0)
      expect_true(tsibble::index_var(load_vars) == "date")
      expect_true(all(tsibble::key_vars(load_vars) %in% c("stkcd", "period")))

      # load indicators
      # Set input for load_vars()
      select_indicators <- c("f030101a", "f030201a")
      session$setInputs(
        vars_type = "indicator",
        vars_groups = "比率结构",
        select_vars = paste0(select_indicators, collapse = ","),
        load_vars = 1 # Non-null value indicating use click button
      )
      # Check load_vars()
      load_vars <- load_vars()
      expect_s3_class(load_vars, "tbl_ts")
      expect_fields <- c(
        c("date", "period", "stkcd", "indcd"),
        select_indicators
      )
      actual_fields <- names(load_vars)
      expect_true(all(actual_fields %in% expect_fields))
      expect_true(nrow(load_vars) >= 0)
      expect_true(tsibble::index_var(load_vars) == "date")
      expect_true(all(tsibble::key_vars(load_vars) %in% c("stkcd", "period")))

      # >> all_dataset_output()----
      # Set input for all_dataset_output()
      unified_period <- c("quarter")
      start_date <- as.Date("2019-01-01")
      end_date <- as.Date("2019-12-31")
      session$setInputs(
        output_all_dates = FALSE,
        output_date_range = c(start_date, end_date),
        output_unified_period = unified_period,
        output_data = 1 # Non-null value indicating use click button
      )
      # Check all_dataset_output()
      output_data <- all_dataset_output()
      expect_s3_class(output_data, "tbl_ts")
      expect_fields <- c(
        c("date", "period", "stkcd", "indcd"),
        select_factors, select_indicators
      )
      actual_fields <- names(output_data)
      expect_true(all(actual_fields %in% expect_fields))
      expect_true(nrow(output_data) >= 0)
      expect_true(all(output_data$period %in% unified_period))
      expect_true(tsibble::index_var(output_data) == "date")
      expect_true(all(tsibble::key_vars(output_data) %in% c("stkcd", "period")))
      expect_true(all(
        (output_data$date >= start_date)&&(output_data$date <= end_date)
      ))
    })
  })
})

test_that("load_data_app - Module App works", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  withr::local_tempdir("test_load_data_app")
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

    # Use to avoid skip message due to empty test, replace it with real tests
    expect_true(TRUE)

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
