# Tests for module of ts_missing_tidyverts  ----

# context("Tests for module of ts_missing_tidyverts")

#Skip tests if stock db is not ready
skip_if_stock_db_not_ready()

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

test_that("ts_missing_tidyverts_server - reactives for output", {
  testServer(ts_missing_tidyverts_server,
    args = list(
      tsbl_vars = reactive(tsbl_vars),
      tsbl_vars_average = reactive(tsbl_vars_average)
    ),
    {
      # ts_missing_tidyverts_server with typical user inputs ====

      # >> Check output data by output_type ----
      output_type_choices <- c("Origin", "Fill gaps", "Fill gaps + impute nas")

      for (output_type in output_type_choices) {

        # Set input for ts_missing_tidyverts
        session$setInputs(
          ts_type = "stock",
          full_gap = "TRUE",
          output_type = output_type
        )

        # browser()
        # Check tidy_tsbl_vars() for output
        tidy_tsbl <- tidy_tsbl_vars()
        expect_s3_class(tidy_tsbl, "tbl_ts")
        expect_equal(tsibble::index_var(tidy_tsbl), "date")
        expect_true(all(tsibble::key_vars(tidy_tsbl) %in% c("stkcd")))
        expect_fields <- names(tsbl_vars_stock_raw())
        actual_fields <- names(tidy_tsbl)
        expect_true(all(actual_fields %in% expect_fields))

        # Make sure critical columns don't contain any Nas
        if (output_type == "Fill gaps + impute nas") {
          critical_columns <- c("date", "stkcd", "indcd")
        } else {
          critical_columns <- c("date", "stkcd")
        }

        nas_in_critical_columns <- tidy_tsbl %>%
          tibble::as_tibble() %>%
          dplyr::summarise(dplyr::across(dplyr::all_of(critical_columns),
            .f = ~ sum(is.na(.x))
          ))
        expect_true(sum(nas_in_critical_columns) == 0)

        # browser()
        # Check tidy_tsbl_vars_average() for output
        tidy_tsbl <- tidy_tsbl_vars_average()
        expect_s3_class(tidy_tsbl, "tbl_ts")
        expect_equal(tsibble::index_var(tidy_tsbl), "date")
        expect_true(all(tsibble::key_vars(tidy_tsbl) %in% c("indcd")))
        expect_fields <- names(tsbl_vars_stock_raw())
        actual_fields <- names(tidy_tsbl)
        expect_true(all(actual_fields %in% expect_fields))


        # Make sure critical columns don't contain any Nas
        critical_columns <- c("date", "indcd")
        nas_in_critical_columns <- tidy_tsbl %>%
          tibble::as_tibble() %>%
          dplyr::summarise(dplyr::across(dplyr::all_of(critical_columns),
            .f = ~ sum(is.na(.x))
          ))
        expect_true(sum(nas_in_critical_columns) == 0)
      }
    }
  )
})

test_that("ts_missing_tidyverts_app - Module App works", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  withr::local_tempdir("ts_missing_tidyverts_app")
  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\nts_missing_tidyverts_app()",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 100)
    })


    # ts_missing_tidyverts_app with typical user inputs ====

    # Use to avoid skip message due to empty test, replace it with real tests
    # expect_true(TRUE)

    # -- Sample Code for reference --
    # select_factors <- c("CR", "QR")
    # app$setInputs(
    #   `ts_missing_tidyverts_module-factor_groups` = "Financial Risk",
    #   `ts_missing_tidyverts_module-factors_in_group` = select_factors,
    #   `ts_missing_tidyverts_module-ts_missing_tidyverts` = "click",
    #   timeout_ = 1000 * 10
    # )
    # expect_snapshot_value(app$getAllValues(), style = "json2")
    expect_snapshot_value(app$getAllValues(), style = "serialize")

    app$stop()
  })
})
