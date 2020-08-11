#' {{module_name}}
#'
#' @description A shiny module for {{module_name}}.
#'
#' @details
#'  The module is an UI for user to ...
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
#'   {{module_name}} <- {{module_name}}_server("{{module_name}}_module")
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
  tagList()
}

#' Server function of {{module_name}}
#'
#' @describeIn {{module_name}}  Server function of {{module_name}}.
#' @return * Server function return a data frame of ...
{{module_name}}_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

#' Testing module app of {{module_name}}
#'
#' @describeIn {{module_name}}  Testing App of {{module_name}}.
{{module_name}}_app <- function() {

  ui <- fluidPage(
    {{module_name}}_ui("{{module_name}}_module")
  )
  server <- function(input, output, session) {
    {{module_name}} <- {{module_name}}_server("{{module_name}}_module")
  }
  shinyApp(ui, server)
}
