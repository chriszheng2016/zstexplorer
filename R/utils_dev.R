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
#'
#' @export
#'
#' @family utils_dev
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
#'
#' @family utils_dev
#'
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

#' Turn on/off debug mode
#'
#' You can put some debug code in your app for debugging. When app is running in
#' debug mode, all debug code will be activated to show debug messages or
#' store data for debug, etc. When app is running in non-debug mode, all
#' debug code will do nothing.
#'
#' @param quiet A logic to show message or not.
#'
#' @family utils_dev
#'
#' @name debug_mode
NULL

# Enable debug mode
#' @describeIn debug_mode Enable debug mode.
#' @export
enable_debug <- function(quiet = FALSE) {
  Sys.setenv(APP_DEBUG = "TRUE")
  if (!quiet) {
    rlang::inform(glue::glue(
      "Environment Variable: APP_DEBUG = ",
      Sys.getenv("APP_DEBUG")
    ))
  }
}

# Disable debug mode
#' @describeIn debug_mode Disable debug mode.
#' @export
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
#' @describeIn debug_mode Judge whether debug is enable or not.
#' @export
on_debug <- function() {
  isTRUE(as.logical(Sys.getenv("APP_DEBUG")))
}


#' Save data for debug
#'
#' Save data in RDS file for debug if [on_debug()] is TRUE.
#'
#' If on_debug() is TRUE, save_debug_data will save data  and return the path
#' of saved file, otherwise it will save nothing and return NULL.
#'
#' The saved data is located in app/temp directory with name like
#' \emph{"outputfile_(YYYY-MM-DD).RDS"}. It will overwrite existed file if
#' overwrite is TRUE, otherwise it will create a new file with a name with serial
#' numbers, like \emph{"outputfile_(YYYY-MM-DD)_1.RDS"}.
#'
#'
#' @param data A R obj to save.
#' @param output_file A character of name of output file
#' @param overwrite A logical to overwrite existed output_file
#' or not. Default FALSE means not to overwrite existed output file.
#'
#' @return A character of path of output file saved or NULL.
#'
#' @family utils_dev
#'
#' @examples
#' \dontrun{
#'
#' # Step1, put save_debug_data() at lines where to save data for debug
#' result <- foo()
#' save_debug_data(result, output_file = "result_data")
#'
#' # Step2, activate debug mode if you want to store data for debug
#' enable_debug()
#'
#' # Step3, deactivate debug mode if you want run app normally
#' disable_debug()
#'
#' }
#' @export
save_debug_data <- function(data, output_file, overwrite = FALSE) {

  #Validate

  if(on_debug()) {
    temp_dir <- app_sys("app/temp")
    output_file_name <- glue::glue({"{output_file}({Sys.Date()}).RDS"})

    # Compose file name for output file
    existed_out_files_count <- length(
      fs::dir_ls(temp_dir, glob = glue::glue("*{output_file}*.RDS"))
    )

    if (!overwrite) {
      if (existed_out_files_count > 0) {
        output_file_name <- glue::glue(
          "{fs::path_ext_remove(output_file_name)}\\
          _{existed_out_files_count}\\
          .{fs::path_ext(output_file_name)}"
        )
      }
    }

    output_file <- fs::path(temp_dir, output_file_name)

    # Save output data in output file
    if (NROW(data) > 0) {
      saveRDS(data, file = output_file)
      msg <- glue::glue("Save output data in {output_file} successfully.")
    } else {
      msg <- glue::glue("No output data to save.")
    }

    if(shiny::isRunning()){
      shiny::showNotification(msg)
    }else{
      rlang::inform(msg)
    }
  } else {
    output_file <- NULL
  }

  output_file
}
