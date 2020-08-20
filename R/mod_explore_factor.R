#' Explore factors in tsibble
#'
#' @description A shiny module to explore factors in tibble of time series(tsibble).
#'
#' @details
#'  The module is an UI for user to explore factors.
#'
#' @name explore_factor
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#' @param factors_info  A data frame of factor information for user to choose.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   explore_factor_ui("explore_factor_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   explore_factor_server("explore_factor_module",
#'     factors_info = reactive(factors_info)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' explore_factor__app()
#' }
#'
NULL

#' UI Function of explore_factor
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn explore_factor  UI function of exploring factors.
#'
#' @importFrom shiny NS tagList
explore_factor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Prepare", tabName = "prepare_data", icon = icon("dashboard")),
          menuItem("Analyze", tabName = "analyze_data", icon = icon("dashboard"))
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "prepare_data",
            tabsetPanel(
              tabPanel("Load Factors", load_factors_ui(ns("load_factors")))
            )
          ),
          tabItem(
            tabName = "analyze_data",
            tabsetPanel(
              tabPanel("Cross-section Analysis",
                       cs_analysis_ui(ns("cs_analysis_module"))),
              tabPanel("Time-series Analysis",
                       ts_analysis_ui(ns("ts_analysis_module")))
            )
          )
        )
      )
    )
  )
}

#' Server Function of explore_factor
#'
#' @return * Server function doesn't return value.
#'
#' @describeIn explore_factor  Server function of exploring factors.

explore_factor_server <- function(id, factors_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(factors_info))

    # Load factors
    load_factors <- load_factors_server("load_factors",
      factors_info = factors_info
    )

    # Cross-section analysis
    cs_analysis_server("cs_analysis_module",
                      tsbl_vars = load_factors
    )

    # Time-series analysis
    ts_analysis_server("ts_analysis_module",
                       tsbl_vars = load_factors
    )
  })
}


#' Testing module app of explore_factor
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn explore_factor  Testing App of exploring factors.
explore_factor_app <- function(use_online_data = FALSE) {

  # Prepare data
  factors_info <- load_factors_info(use_online_data)

  ui <- fluidPage(
    explore_factor_ui("explore_factor_module")
  )
  server <- function(input, output, session) {
    explore_factor_server("explore_factor_module",
      factors_info = reactive(factors_info)
    )
  }
  shinyApp(ui, server)
}
