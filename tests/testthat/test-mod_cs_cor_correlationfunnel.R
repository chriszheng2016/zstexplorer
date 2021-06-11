# Tests for module of cs_cor_correlationfunnel  ----


#context("Tests for module of cs_cor_correlationfunnel")

#Skip tests if stock db is not ready
skip_if_stock_db_not_ready()

# Set up test environment

# Prepare test data
csbl_vars <- load_csbl_vars(use_online_data = FALSE)


test_that("cs_cor_correlationfunnel_server - reactives and output updates", {
  testServer(cs_cor_correlationfunnel_server,
    args = list(csbl_vars = reactive(csbl_vars)),
    {
      # cs_cor_correlationfunnel_server with typical user inputs ====

      # Use to avoid skip message due to empty test, replace it with real tests
      expect_true(TRUE)

      # -- Sample Code for reference --
      # Set input for cs_cor_correlationfunnel
      # select_factors <- c("CR", "QR")
      # session$setInputs(
      #   factor_groups = "Financial Risk",
      #   select_factors = select_factors,
      #   cs_cor_correlationfunnel = 1 # Non-null value indicating use click button
      # )
      # Check cs_cor_correlationfunnel()
      # cs_cor_correlationfunnel <- cs_cor_correlationfunnel()
      # expect_s3_class(cs_cor_correlationfunnel, "data.frame")
      # expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_factors)
      # actual_fields <- names(cs_cor_correlationfunnel
      # expect_true(all(actual_fields %in% expect_fields))
      # expect_true(nrow(cs_cor_correlationfunnel >= 0)
      # Check output
      # expect_snapshot_output(output$factors_info_table)
      # expect_snapshot_output(output$factors_data_table)

    }
  )
})

test_that("cs_cor_correlationfunnel_app - Module App works", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  withr::local_tempdir("test_cs_cor_correlationfunnel_app")
  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\ncs_cor_correlationfunnel_app()",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 100)
    })


    # cs_cor_correlationfunnel_app with typical user inputs ====

    # -- Sample Code for reference --
    # select_factors <- c("CR", "QR")
    # app$setInputs(
    #   `cs_cor_correlationfunnel_module-factor_groups` = "Financial Risk",
    #   `cs_cor_correlationfunnel_module-factors_in_group` = select_factors,
    #   `cs_cor_correlationfunnel_module-cs_cor_correlationfunnel` = "click",
    #   timeout_ = 1000 * 10
    # )
    # expect_snapshot_value(app$getAllValues(), style = "json2")
    expect_snapshot_value(app$getAllValues(), style = "serialize")

    app$stop()
  })
})

