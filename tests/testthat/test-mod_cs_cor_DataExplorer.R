# Tests for module of cs_cor_DataExplorer  ----
options(testthat.edition_ignore = TRUE)

context("Tests for module of cs_cor_DataExplorer")


# Set up test environment

# Prepare test data
csbl_vars <- load_csbl_vars(use_online_data = FALSE)

test_that("cs_cor_DataExplorer_server - reactives and output updates", {
  testServer(cs_cor_DataExplorer_server,
    args = list(csbl_vars = reactive(csbl_vars)),
    {
      # cs_cor_DataExplorer_server with typical user inputs ====

      # -- Sample Code for reference --
      # Set input for cs_cor_DataExplorer
      # select_factors <- c("CR", "QR")
      # session$setInputs(
      #   factor_groups = "Financial Risk",
      #   select_factors = select_factors,
      #   cs_cor_DataExplorer = 1 # Non-null value indicating use click button
      # )
      # Check cs_cor_DataExplorer()
      # cs_cor_DataExplorer <- cs_cor_DataExplorer()
      # expect_s3_class(cs_cor_DataExplorer, "data.frame")
      # expect_fields <- c(c("date", "period", "stkcd", "indcd"), select_factors)
      # actual_fields <- names(cs_cor_DataExplorer
      # expect_true(all(actual_fields %in% expect_fields))
      # expect_true(nrow(cs_cor_DataExplorer >= 0)
      # Check output
      # expect_snapshot_output(output$factors_info_table)
      # expect_snapshot_output(output$factors_data_table)

    }
  )
})

test_that("cs_cor_DataExplorer_app - Module App works", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  test_app_file <- "app.R"
  withr::with_file(test_app_file, {

    # Set up temp app.R for loading App
    writeLines("pkgload::load_all()\ncs_cor_DataExplorer_app()",
      con = test_app_file
    )

    # Load test App
    suppressWarnings({
      app <- shinytest::ShinyDriver$new(".", loadTimeout = 1000 * 100)
    })


    # cs_cor_DataExplorer_app with typical user inputs ====

    # -- Sample Code for reference --
    # select_factors <- c("CR", "QR")
    # app$setInputs(
    #   `cs_cor_DataExplorer_module-factor_groups` = "Financial Risk",
    #   `cs_cor_DataExplorer_module-factors_in_group` = select_factors,
    #   `cs_cor_DataExplorer_module-cs_cor_DataExplorer` = "click",
    #   timeout_ = 1000 * 10
    # )
    # expect_snapshot_value(app$getAllValues(), style = "json2")
    expect_snapshot_value(app$getAllValues(), style = "serialize")

    app$stop()
  })
})

