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
                             ignore = FALSE,
                             open = TRUE,
                             package = pkgload::pkg_name()) {
  pkg_path <- find.package(package)

  # Setting for output file
  type <- match.arg(type)
  switch(type,
    "source" = {
      template_file <- "mod_template.R"
      target_file <- glue::glue("R/mod_{name}.R")
    },
    "test" = {
      template_file <- "test-mod_template.R"
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
