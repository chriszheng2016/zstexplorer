# Tests for utility functions of development ----

context("Tests for utility functions of development")

test_that("add_shiny_module, with various arguments", {

  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  # add_shiny_module on default arguments  ====
  expect_true(create_success <- add_shiny_module("xyz"))
  if(create_success) {
    expect_file <- fs::path(find.package("zstexplorer"),
                            "R/mod_xyz.R")
    expect_true(fs::file_exists(expect_file))
    fs::file_delete(expect_file)
  }

  # add_shiny_module on various arguments  ====
  expect_true(create_success <- add_shiny_module("xyz", type = "test"))
  if(create_success) {
    expect_file <- fs::path(find.package("zstexplorer"),
                            "tests/testthat/test-mod_xyz.R")
    expect_true(fs::file_exists(expect_file))
    fs::file_delete(expect_file)
  }

})
