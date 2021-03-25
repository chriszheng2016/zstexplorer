#' ts_analysis
#'
#' @description A shiny module for ts_analysis.
#'
#' @details
#'  The module is an UI for user to analyze variables in time series.
#'
#' @name ts_analysis
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   ts_analysis_ui("ts_analysis_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   ts_analysis <- ts_analysis_server("ts_analysis_module",
#'     tsbl_vars = reactive(tsbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' ts_analysis_app()
#' }
#'
NULL

#' UI function of ts_analysis
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn ts_analysis  UI function of ts_analysis.
#' @importFrom shiny NS tagList
#'
#' @import tsibble
ts_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Side panel for input
    sidebarPanel(
      width = 2,

      # Ui for slicing tsbl_vars
      slice_tsbl_ui(ns("slice_tsbl_module"))
    ),
    # MainPanel for Output
    mainPanel(
      width = 10,
      titlePanel(textOutput(outputId = ns("caption"))),
      navbarPage("Navigate:",
        id = ns("tabs"),
        navbarMenu(
          "Summary",
          tabPanel(
            "Gaps in timeseries",
            ts_gap_tidyverts_ui(ns("ts_gap_tidyverts_module"))
          ),
          tabPanel(
            "Misings values",
            ts_missing_tidyverts_ui(ns("ts_missing_tidyverts_module"))
          )
        ),
        navbarMenu(
          "Features",
          tabPanel(
            "Basic features",
            ts_feat_basic_tidyverts_ui(ns("ts_feat_basic_tidyverts_module"))
          ),
          tabPanel(
            "Correlation features",
            ts_feat_cor_tidyverts_ui(ns("ts_feat_cor_tidyverts_module"))
          )
        )
      )
    )
  )
}

#' Server function of ts_analysis
#'
#' @param tsbl_vars A tsibble of vars for time-series analysis.
#'
#' @param debug A logic to enable/disable output for debug. Default FALSE
#'  means to disable output of debug.
#'
#' @describeIn ts_analysis  Server function of ts_analysis.
#' @return * Server function dosen't return value.
ts_analysis_server <- function(id, tsbl_vars, debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(tsbl_vars))

    slice_tsbl_vars <- slice_tsbl_server("slice_tsbl_module",
      tsbl_vars = tsbl_vars,
      slice_type = "time_series",
      debug = debug
    )

    # Summary output ----

    # Gaps in time series
    ts_gap_tidyverts_server("ts_gap_tidyverts_module",
      tsbl_vars = slice_tsbl_vars
    )

    # Missing values in time series
    tidy_tsbl_vars <- ts_missing_tidyverts_server(
      "ts_missing_tidyverts_module",
      tsbl_vars = slice_tsbl_vars
    )

    # Analyze basic features of time series
    ts_feat_basic_tidyverts_server(
      "ts_feat_basic_tidyverts_module",
      tsbl_vars = tidy_tsbl_vars
    )

    # Analyze correlation features of time series
    ts_feat_cor_tidyverts_server(
      "ts_feat_cor_tidyverts_module",
      tsbl_vars = tidy_tsbl_vars
    )
  })
}

#' Testing module app of ts_analysis
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @param debug A logic to enable debug or not, default is on_debug() which
#'  returns DEBUG environment variable.
#'
#' @describeIn ts_analysis  Testing App of ts_analysis.
ts_analysis_app <- function(use_online_data = FALSE, debug = on_debug()) {

  # Prepare data
  tsbl_vars <- load_tsbl_vars(use_online_data)

  ui <- fluidPage(
    ts_analysis_ui("ts_analysis_module")
  )
  server <- function(input, output, session) {
    ts_analysis <- ts_analysis_server("ts_analysis_module",
      tsbl_vars = reactive(tsbl_vars)
    )
  }
  shinyApp(ui, server)
}
