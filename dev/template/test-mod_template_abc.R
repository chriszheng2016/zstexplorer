# Tests for module of template_abc  ----
options(testthat.edition_ignore = TRUE)

context("Tests for module of template_abc")


# Set up test environment

test_that("template_abc_server - reactives and output updates", {
  testServer(template_abc_server,
    args = list(factors_info = reactive(factors_info)),
    {
      # template_abc_server with typical user inputs ====

      # -- Sample Code for reference --
      # Set input for template_abc
      # select_factors <- c("CR", "QR")
      # session$setInputs(
      #   factor_groups = "Financial Risk",
      #   select_factors = select_factors,
      #   template_abc = 1 # Non-null value indicating use click button
      # )
      # Check template_abc()
      # expect_is(template_abc(), "data.frame")
      # expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_factors)
      # actual_fields <- names(template_abc())
      # expect_true(all(actual_fields %in% expect_fields))
      # expect_true(nrow(template_abc()) >= 0)
      # Check output
      # expect_snapshot_output(output$factors_info_table)
      # expect_snapshot_output(output$factors_data_table)

    }
  )
})

test_that("template_abc_app - Module App works", {
  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\ntemplate_abc_app()",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 100)
    })


    # template_abc_app with typical user inputs ====

    # -- Sample Code for reference --
    # select_factors <- c("CR", "QR")
    # app$setInputs(
    #   `template_abc_module-factor_groups` = "Financial Risk",
    #   `template_abc_module-factors_in_group` = select_factors,
    #   `template_abc_module-template_abc` = "click",
    #   timeout_ = 1000 * 10
    # )
    # expect_snapshot_value(app$getAllValues(), style = "json2")

    app$stop()
  })
})


