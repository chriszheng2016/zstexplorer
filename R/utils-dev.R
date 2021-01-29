# Utility functions of development ----

#' Add shiny module file
#'
#' Create source/test file for a shiny module by template file in
#' templates directory of the packages.
#'
#' This function is a wrapper of [usethis::use_template()] to use shiny
#' module template.
#'
#' It produces source file in "R" directory and test file in "tests/testthat"
#' directory. If target file existed, it would not overwrite it and
#' return FALSE.
#'
#' @param name A character of module name, i.g. "abc".
#' @param type A character of type of target file to produce:
#'   * source: produce module source file as "mod_abc.R".
#'   * test: produce module test file as "test-mod_abc.R".
#' @param source_template A character of R source template, default is
#'  "mod_template.R".
#' @param test_template A character of R source template, default is
#'  "test-mod_template.R".
#' @param ignore A logical to indicate whether the produced file to be ignored
#'   by git.
#' @param open A logical to indicate whether open the produced file.
#' @param package A character of package which contains templates files.
#' Default is current package returned by [pkgload::pkg_name()].
#'
#' @return A logical vector indicating if file was modified.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Create source file for module of "xyz"
#' add_shiny_module("xyz")
#'
#' # Try integration test of module
#' xyz_app()
#'
#' # Create test file for module of "xyz"
#' add_shiny_module("xyz", type = "test")
#'
#' # Make sure tests of module is OK
#' testthat::test_file("tests/testthat/test-mod_xyz.R")
#' }
add_shiny_module <- function(name,
                             type = c("source", "test"),
                             source_template = "mod_template.R",
                             test_template = "test-mod_template.R",
                             ignore = FALSE,
                             open = TRUE,
                             package = pkgload::pkg_name()) {

  # Validate parameters
  assertive::assert_is_character(source_template)
  assertive::assert_is_character(test_template)

  pkg_path <- find.package(package)

  # Setting for output file
  type <- match.arg(type)
  switch(type,
    "source" = {
      template_file <- source_template
      target_file <- glue::glue("R/mod_{name}.R")
    },
    "test" = {
      template_file <- test_template
      target_file <- glue::glue("tests/testthat/test-mod_{name}.R")
    }
  )
  # Build template to target file
  usethis::use_template(
    template = template_file,
    save_as = target_file,
    data = list(module_name = name),
    open = open,
    package = package
  )
}

#' Add skeleton of analysis module
#'
#' This function is a wrapper of [add_shiny_module()] to add skeleton for a
#' analysis module.
#'
#' * `add_cs_module()` is a wrapper of this function for cross-sectional
#' analysis modules.
#'
#' * `add_ts_module()` is a wrapper of this function for time series
#' analysis modules.
#'
#' @param name A character of module name, i.g. "abc".
#' @param ignore A logical to indicate whether the produced file to be ignored
#'   by git.
#' @param open A logical to indicate whether open the produced file.
#' @param source_template A character of R source template, default is
#'  "mod_template.R".
#' @param test_template A character of R source template, default is
#'  "test-mod_template.R".
#' @return A logical vector indicating if target files created successfully.
#' @examples
#' \dontrun{
#'
#' ## Create skeleton of general analysis module
#'
#' # Create source/test file for module of "abc"
#' # ("mod_abc.R" and "test-mod_abc.R")
#' add_analysis_module("abc")
#'
#' # Try integration test of module
#' abc_app()
#'
#' # Make sure tests of module is OK
#' testthat::test_file("tests/testthat/test-mod_abc.R")
#'
#' ## Create skeleton of cross-section analysis module
#'
#' # Create source/test file for module of "abc"
#' # ("mod_cs_abc.R" and "test-mod_cs_abc.R")
#' add_analysis_module("abc")
#'
#' # Try integration test of module
#' cs_abc_app()
#'
#' # Make sure tests of module is OK
#' testthat::test_file("tests/testthat/test-mod_cs_abc.R")
#'
#' ## Create skeleton of time series analysis module
#'
#' # Create source/test file for module of "abc"
#' # ("mod_ts_abc.R" and "test-mod_ts_abc.R")
#' add_analysis_module("abc")
#'
#' # Try integration test of module
#' ts_abc_app()
#'
#' # Make sure tests of module is OK
#' testthat::test_file("tests/testthat/test-mod_ts_abc.R")
#' }
add_analysis_module <- function(name,
                                ignore = FALSE,
                                open = TRUE,
                                source_template = "mod_template.R",
                                test_template = "test-mod_template.R") {
  # Add source file
  success <- add_shiny_module(name,
    source_template = source_template,
    test_template = test_template,
    type = "source",
    ignore = ignore,
    open = open
  )

  # Add test file
  if (success) {
    success <- add_shiny_module(name,
      source_template = source_template,
      test_template = test_template,
      type = "test",
      ignore = ignore,
      open = open
    )
    if (!success) {
      rlang::warn(glue::glue("Fail to create test file by {test_template}"))
    }
  } else {
    rlang::warn(glue::glue("Fail to create source file by {source_template}"))
  }

  return(success)
}


# Add skeleton of module for cross-sectional analysis
#' @describeIn  add_analysis_module Add skeleton of module for
#' cross-sectional analysis.
add_cs_module <- function(name,
                          ignore = FALSE,
                          open = TRUE) {
  if (!stringr::str_detect(name, pattern = "^cs_")) {
    name <- glue::glue("cs_{name}")
  }

  add_analysis_module(name,
    ignore = FALSE,
    open = TRUE,
    source_template = "mod_cs_template.R",
    test_template = "test-mod_cs_template.R"
  )
}
# Add skeleton of module for time series analysis
#' @describeIn  add_analysis_module Add skeleton of module for
#' time series analysis.
add_ts_module <- function(name,
                          ignore = FALSE,
                          open = TRUE) {
  if (!stringr::str_detect(name, pattern = "^ts_")) {
    name <- glue::glue("ts_{name}")
  }

  add_analysis_module(name,
    ignore = FALSE,
    open = TRUE,
    source_template = "mod_ts_template.R",
    test_template = "test-mod_ts_template.R"
  )
}

# Enable environment variable for debug
enable_debug <- function(quiet = FALSE) {
  Sys.setenv(APP_DEBUG = "TRUE")
  if (!quiet) {
    rlang::inform(glue::glue(
      "Environment Variable: APP_DEBUG = ",
      Sys.getenv("APP_DEBUG")
    ))
  }
}

# Enable environment variable for debug
disable_debug <- function(quiet = FALSE) {
  Sys.setenv(APP_DEBUG = "FALSE")
  if (!quiet) {
    rlang::inform(glue::glue(
      "Environment Variable: APP_DEBUG = ",
      Sys.getenv("APP_DEBUG")
    ))
  }
}

# Judge whether debug is enable or not
on_debug <- function() {
  isTRUE(as.logical(Sys.getenv("APP_DEBUG")))
}
