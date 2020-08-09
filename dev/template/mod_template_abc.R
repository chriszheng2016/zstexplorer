#' template_abc
#'
#' @description A shiny module for template_abc.
#'
#' @details
#'  The module is an UI for user to ...
#'
#' @name template_abc
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   template_abc_ui("template_abc_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   template_abc <- template_abc_server("template_abc_module")
#' }
#'
#' # Run testing App for integration testing
#' template_abc_app()
#' }
#'
NULL

#' UI function of template_abc
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn template_abc  UI function of template_abc.
#' @importFrom shiny NS tagList
template_abc_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' Server function of template_abc
#'
#' @describeIn template_abc  Server function of template_abc.
#' @return * Server function return a data frame of ...
template_abc_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

#' Testing module app of template_abc
#'
#' @describeIn template_abc  Testing App of template_abc.
template_abc_app <- function() {

  ui <- fluidPage(
    template_abc_ui("template_abc_module")
  )
  server <- function(input, output, session) {
    template_abc <- template_abc_server("template_abc_module")
  }
  shinyApp(ui, server)
}
