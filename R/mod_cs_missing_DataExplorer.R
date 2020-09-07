#' cs_missing_DataExplorer
#'
#' @description A shiny module for cs_missing_DataExplorer.
#'
#' @details
#'  The module is an UI for user to display plots of missing pattern
#'  by [`DataExplorer`][DataExplorer::DataExplorer] package.
#'
#' @name cs_missing_DataExplorer
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_missing_DataExplorer_ui("cs_missing_DataExplorer_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_missing_DataExplorer <- cs_missing_DataExplorer_server(
#'     "cs_missing_DataExplorer_module",
#'     csbl_vars = reactive(csbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_missing_DataExplorer_app()
#' }
#'
NULL

#' UI function of cs_missing_DataExplorer
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_missing_DataExplorer  UI function of cs_missing_DataExplorer.
#' @importFrom shiny NS tagList
cs_missing_DataExplorer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Missing Summary Plot", status = "primary",
        solidHeader = TRUE, collapsible = TRUE, width = 9,

        plotOutput(ns("missing_plot"))
      ),
      box(
        title = "Missing Summary Table", status = "primary",
        solidHeader = TRUE, collapsible = TRUE, width = 3,

        tableOutput(ns("missing_table"))
      )
    )
  )
}

#' Server function of cs_missing_DataExplorer
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn cs_missing_DataExplorer  Server function of cs_missing_DataExplorer.
#' @return * Server function return a data frame of ...
cs_missing_DataExplorer_server <- function(id, csbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(csbl_vars))

    # Focus csbl_vars for analyzing
    csbl_vars_focus <- reactive({
      csbl_vars() %>%
        dplyr::select(-c("id"))
    })


    output$missing_plot <- renderPlot({
      csbl_vars_focus() %>%
        DataExplorer::plot_missing()
    })

    output$missing_table <- renderTable({
      csbl_vars_focus() %>%
        DataExplorer::profile_missing()
    })
  })
}

#' Testing module app of cs_missing_DataExplorer
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_missing_DataExplorer  Testing App of cs_missing_DataExplorer.
cs_missing_DataExplorer_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data = FALSE)

  ui <- fluidPage(
    cs_missing_DataExplorer_ui("cs_missing_DataExplorer_module")
  )
  server <- function(input, output, session) {
    cs_missing_DataExplorer_server(
      "cs_missing_DataExplorer_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}
