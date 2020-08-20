#' cs_missing_mice
#'
#' @description A shiny module for cs_missing_mice.
#'
#' @details
#'  The module is an UI for user to display missing data by [`mice`][mice::mice] package.
#'
#' @name cs_missing_mice
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_missing_mice_ui("cs_missing_mice_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_missing_mice <- cs_missing_mice_server(
#'     "cs_missing_mice_module",
#'     csbl_vars = reactive(csbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_missing_mice_app()
#' }
#'
NULL

#' UI function of cs_missing_mice
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_missing_mice  UI function of cs_missing_mice.
#' @importFrom shiny NS tagList
cs_missing_mice_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Pattern plot",
        plotOutput(ns("missing_pattern_plot"))
      ),
      tabPanel(
        "Pattern info",
        verbatimTextOutput(ns("missing_pattern_info"))
      )
    )
  )
}

#' Server function of cs_missing_mice
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn cs_missing_mice  Server function of cs_missing_mice.
#' @return * Server function doesn't return value.
cs_missing_mice_server <- function(id, csbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(csbl_vars))

    output$missing_pattern_plot <- renderPlot(
      width = 800,
      height = 800,
      {
        csbl_vars() %>%
          mice::md.pattern(rotate.names = TRUE)
      }
    )

    output$missing_pattern_info <- renderPrint({
      csbl_vars() %>%
        mice::md.pattern(plot = FALSE)
    })
  })
}

#' Testing module app of cs_missing_mice
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_missing_mice  Testing App of cs_missing_mice.
cs_missing_mice_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data)

  ui <- fluidPage(
    cs_missing_mice_ui("cs_missing_mice_module")
  )
  server <- function(input, output, session) {
    cs_missing_mice_server(
      "cs_missing_mice_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}
