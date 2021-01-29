# Tests for module of {{module_name}}  ----

#context("Tests for module of {{module_name}}")


# Set up test environment

# Prepare test data
tsbl_vars <- load_tsbl_vars(use_online_data = FALSE)

test_that("{{module_name}}_server - reactives and output updates", {
  testServer({{module_name}}_server,
    args = list(tsbl_vars = reactive(tsbl_vars)),
    {
      # {{module_name}}_server with typical user inputs ====

      # Use to avoid skip message due to empty test, replace it with real tests
      expect_true(TRUE)

      # -- Sample Code for reference --
      # Set input for {{module_name}}
      # select_factors <- c("CR", "QR")
      # session$setInputs(
      #   factor_groups = "Financial Risk",
      #   select_factors = select_factors,
      #   {{module_name}} = 1 # Non-null value indicating use click button
      # )
      # Check {{module_name}}()
      # {{module_name}} <- {{module_name}}()
      # expect_s3_class({{module_name}}, "data.frame")
      # expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_factors)
      # actual_fields <- names({{module_name}}
      # expect_true(all(actual_fields %in% expect_fields))
      # expect_true(nrow({{module_name}} >= 0)
      # Check output
      # expect_snapshot_output(output$factors_info_table)
      # expect_snapshot_output(output$factors_data_table)

    }
  )
})

test_that("{{module_name}}_app - Module App works", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\n{{module_name}}_app()",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 100)
    })


    # {{module_name}}_app with typical user inputs ====

    # Use to avoid skip message due to empty test, replace it with real tests
    expect_true(TRUE)

    # -- Sample Code for reference --
    # select_factors <- c("CR", "QR")
    # app$setInputs(
    #   `{{module_name}}_module-factor_groups` = "Financial Risk",
    #   `{{module_name}}_module-factors_in_group` = select_factors,
    #   `{{module_name}}_module-{{module_name}}` = "click",
    #   timeout_ = 1000 * 10
    # )
    # expect_snapshot_value(app$getAllValues(), style = "json2")
    # expect_snapshot_value(app$getAllValues(), style = "serialize")

    app$stop()
  })
})

