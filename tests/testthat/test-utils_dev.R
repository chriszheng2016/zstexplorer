# Tests for utility functions of development ----

# context("Tests for utility functions of development")

test_that("add_shiny_module, with various arguments", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  # add_shiny_module on default arguments  ====
  suppressMessages({
    expect_true(create_success <- add_shiny_module("xyz"))
  })

  if (create_success) {
    expect_file <- fs::path(
      find.package("zstexplorer"),
      "R/mod_xyz.R"
    )
    expect_true(fs::file_exists(expect_file))
    fs::file_delete(expect_file)
  }

  # add_shiny_module on various arguments  ====
  suppressMessages({
    expect_true(create_success <- add_shiny_module("xyz", type = "test"))
  })

  if (create_success) {
    expect_file <- fs::path(
      find.package("zstexplorer"),
      "tests/testthat/test-mod_xyz.R"
    )
    expect_true(fs::file_exists(expect_file))
    fs::file_delete(expect_file)
  }
})

test_that("add_analysis_module, with various arguments", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  # add_analysis_module on default arguments  ====
  suppressMessages({
    expect_true(create_success <- add_analysis_module("abc"))
  })

  if (create_success) {
    expect_source_file <- fs::path(
      find.package("zstexplorer"),
      "R/mod_abc.R"
    )
    expect_true(fs::file_exists(expect_source_file))
    fs::file_delete(expect_source_file)

    expect_test_file <- fs::path(
      find.package("zstexplorer"),
      "tests/testthat/test-mod_abc.R"
    )
    expect_true(fs::file_exists(expect_test_file))
    fs::file_delete(expect_test_file)
  }
})

test_that("add_cs_module, with various arguments", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  # add_cs_module on default arguments  ====
  suppressMessages({
    expect_true(create_success <- add_cs_module("abc"))
  })

  if (create_success) {
    expect_source_file <- fs::path(
      find.package("zstexplorer"),
      "R/mod_cs_abc.R"
    )
    expect_true(fs::file_exists(expect_source_file))
    fs::file_delete(expect_source_file)

    expect_test_file <- fs::path(
      find.package("zstexplorer"),
      "tests/testthat/test-mod_cs_abc.R"
    )
    expect_true(fs::file_exists(expect_test_file))
    fs::file_delete(expect_test_file)
  }
})

test_that("add_ts_module, with various arguments", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  # add_ts_module on default arguments  ====
  suppressMessages({
    expect_true(create_success <- add_ts_module("abc"))
  })

  if (create_success) {
    expect_source_file <- fs::path(
      find.package("zstexplorer"),
      "R/mod_ts_abc.R"
    )
    expect_true(fs::file_exists(expect_source_file))
    fs::file_delete(expect_source_file)

    expect_test_file <- fs::path(
      find.package("zstexplorer"),
      "tests/testthat/test-mod_ts_abc.R"
    )
    expect_true(fs::file_exists(expect_test_file))
    fs::file_delete(expect_test_file)
  }
})

test_that("enable_debug/disable_debug/on_debug, with various arguments", {

  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  withr::with_envvar(new = c("APP_DEBUG" = FALSE), {
    # enable_debug/disable_debug/on_debug on default arguments  ====
    expect_message(enable_debug(), regexp = "APP_DEBUG = TRUE")
    expect_true(on_debug() == TRUE)

    expect_message(disable_debug(), regexp = "APP_DEBUG = FALSE")
    expect_true(on_debug() == FALSE)

    # enable_debug/disable_debug/on_debug on various arguments  ====
    expect_silent(enable_debug(quiet = TRUE))
    expect_true(on_debug() == TRUE)

    expect_silent(disable_debug(quiet = TRUE))
    expect_true(on_debug() == FALSE)
  })
})

test_that("save_debug_data, with various arguments", {

  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  # save_debug_data on default arguments  ====
  withr::with_envvar(new = c("APP_DEBUG" = FALSE), {
    output_file <- "output_file"
    output_data <- tibble::tibble(A = 1:5, B = 6:10)

    # Clear existed files
    existed_files <- fs::dir_ls(
      path = fs::path(system.file(package = "zstexplorer"), "app/temp"),
      glob = "*output_file*.RDS"
    )
    fs::file_delete(existed_files)

    # Work in debug_mode
    enable_debug(quiet = TRUE)
    expect_message(
      {
        saved_file <- save_debug_data(output_data, output_file = output_file)
      },
      regexp = "Save output data in|successfully"
    )
    expect_true(fs::file_exists(saved_file))
    fs::file_delete(saved_file)

    # Work in non-debug mode
    disable_debug(quiet = TRUE)
    expect_silent({
      saved_file <- save_debug_data(output_data, output_file = output_file)
    })
    expect_true(is.null(saved_file))
  })

  # save_debug_data on various arguments  ====
  withr::with_envvar(new = c("APP_DEBUG" = FALSE), {
    output_file <- "output_file"
    output_data <- tibble::tibble(A = 1:5, B = 6:10)

    # Clear existed files
    existed_files <- fs::dir_ls(
      path = fs::path(system.file(package = "zstexplorer"), "app/temp"),
      glob = "*output_file*.RDS"
    )
    fs::file_delete(existed_files)

    # Work in debug_mode
    enable_debug(quiet = TRUE)
    expect_message(
      {
        saved_file <- save_debug_data(output_data,
          output_file = output_file,
          overwrite = FALSE
        )
      },
      regexp = "Save output data in|successfully"
    )
    expect_true(fs::file_exists(saved_file))

    expect_message(
      {
        saved_file1 <- save_debug_data(output_data,
          output_file = output_file,
          overwrite = FALSE
        )
      },
      regexp = "Save output data in|successfully"
    )
    expect_true(fs::file_exists(saved_file1))

    existed_files <- fs::dir_ls(
      path = fs::path_dir(saved_file),
      glob = "*output_file*.RDS"
    )
    expect_true(NROW(existed_files) == 2)
    fs::file_delete(existed_files)

    # Work in non-debug mode
    disable_debug(quiet = TRUE)
    expect_silent({
      saved_file <- save_debug_data(output_data,
        output_file = output_file,
        overwrite = FALSE
      )
    })
    expect_true(is.null(saved_file))
  })
})
