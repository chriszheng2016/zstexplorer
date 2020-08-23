#' {{module_name}}
#'
#' @description A shiny module for {{module_name}}.
#'
#' @details
#'  The module is an UI for user to display plots of ...
#'  by [`package_abc`][package_abc::package_abc] package.
#'
#' @name {{module_name}}
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   {{module_name}}_ui("{{module_name}}_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   {{module_name}} <- {{module_name}}_server(
#'   "{{module_name}}_module",
#'    csbl_vars = reactive(csbl_vars))
#' }
#'
#' # Run testing App for integration testing
#' {{module_name}}_app()
#' }
#'
NULL

#' UI function of {{module_name}}
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn {{module_name}}  UI function of {{module_name}}.
#' @importFrom shiny NS tagList
{{module_name}}_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("{{module_name}}")
  )
}

#' Server function of {{module_name}}
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn {{module_name}}  Server function of {{module_name}}.
#' @return * Server function return a data frame of ...
{{module_name}}_server <- function(id, csbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(csbl_vars))
  })
}

#' Testing module app of {{module_name}}
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn {{module_name}}  Testing App of {{module_name}}.
{{module_name}}_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data = FALSE)

  ui <- fluidPage(
    {{module_name}}_ui("{{module_name}}_module")
  )
  server <- function(input, output, session) {
    {{module_name}}_server(
      "{{module_name}}_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}
