# Tests for module of cs_missing_vim  ----

#context("Tests for module of cs_missing_vim")

#Skip tests if stock db is not ready
skip_if_stock_db_not_ready()

# Set up test environment

# Prepare test data
csbl_vars <- load_csbl_vars(use_online_data = FALSE)

test_that("cs_missing_vim_server - reactives and output updates", {
  testServer(cs_missing_vim_server,
    args = list(csbl_vars = reactive(csbl_vars)),
    {
      # cs_missing_vim_server with typical user inputs ====

      # Use to avoid skip message due to empty test, replace it with real tests
      expect_true(TRUE)

      # -- Sample Code for reference --
      # Set input for cs_missing_vim
      # select_factors <- c("CR", "QR")
      # session$setInputs(
      #   factor_groups = "Financial Risk",
      #   select_factors = select_factors,
      #   cs_missing_vim = 1 # Non-null value indicating use click button
      # )
      # Check cs_missing_vim()
      # cs_missing_vim <- cs_missing_vim()
      # expect_s3_class(cs_missing_vim, "data.frame")
      # expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_factors)
      # actual_fields <- names(cs_missing_vim
      # expect_true(all(actual_fields %in% expect_fields))
      # expect_true(nrow(cs_missing_vim >= 0)
      # Check output
      # expect_snapshot_output(output$factors_info_table)
      # expect_snapshot_output(output$factors_data_table)

    }
  )
})

test_that("cs_missing_vim_app - Module App works", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  withr::local_tempdir("test_cs_missing_vim_app")
  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\ncs_missing_vim_app()",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 100)
    })


    # cs_missing_vim_app with typical user inputs ====

    # Use to avoid skip message due to empty test, replace it with real tests
    expect_true(TRUE)

    # -- Sample Code for reference --
    # select_factors <- c("CR", "QR")
    # app$setInputs(
    #   `cs_missing_vim_module-factor_groups` = "Financial Risk",
    #   `cs_missing_vim_module-factors_in_group` = select_factors,
    #   `cs_missing_vim_module-cs_missing_vim` = "click",
    #   timeout_ = 1000 * 10
    # )
    # expect_snapshot_value(app$getAllValues(), style = "json2")
    expect_snapshot_value(app$getAllValues(), style = "serialize")

    app$stop()
  })
})
