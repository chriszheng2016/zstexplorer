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
              tabPanel("Distribution", univar_dis_ui(ns("univar_dist")))
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

    # Analyze univariate distribution
    univar_dis_server("univar_dist",
      tsbl_vars = load_factors
    )
  })
}


#' Testing module app of explore_factor
#'
#' @describeIn explore_factor  Testing App of exploring factors.
explore_factor_app <- function() {

  # Get factors info
  stock_db <- zstmodelr::stock_db(
    zstmodelr::gta_db,
    get_golem_config("database_dsn")
  )
  zstmodelr::open_stock_db(stock_db)
  factors_info <- zstmodelr::get_factors_info(stock_db, factor_groups = NULL)
  zstmodelr::close_stock_db(stock_db)

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
