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
#'   explore_factor_server("explore_factor_module")
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
      dashboardHeader(title = "Explore factors"),
      dashboardSidebar(
        sidebarSearchForm(
          textId = ns("search_text"),
          buttonId = ns("search_button"),
          label = "Enter a code:",
        ),

        sidebarMenu(
          id = ns("side_menu_tabs"),
          menuItem("Prepare", tabName = "prepare_data", icon = icon("dashboard")),
          menuItem("Analyze",
            tabName = "analyze_data", icon = icon("bar-chart-o"),
            startExpanded = TRUE,
            menuSubItem("Cross-section Analysis",
              tabName = "cross_section_analysis"
            ),
            menuSubItem("Time-series Analysis",
              tabName = "time_series_analysis"
            )
          ),
          menuItem("Dictionary",
            tabName = "data_dictionary",
            icon = icon("fas fa-book-open")
          )
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "prepare_data",
            tabsetPanel(
              tabPanel("Load data", load_data_ui(ns("load_data_module")))
            )
          ),
          tabItem(
            tabName = "cross_section_analysis",
            cs_analysis_ui(ns("cs_analysis_module"))
          ),
          tabItem(
            tabName = "time_series_analysis",
            ts_analysis_ui(ns("ts_analysis_module"))
          ),
          tabItem(
            tabName = "data_dictionary",
            data_dictionary_ui(ns("data_dictionary_module"))
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

explore_factor_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters

    # UI interactive events ----

    observeEvent(input$search_button, {

      # Match codes to names
      codes <- stringr::str_split(
        stringr::str_trim(input$search_text),
        pattern = "\\s*,\\s*|\\s+",
        simplify = FALSE
      )[[1]]

      names <- code2name(codes)

      showModal(modalDialog(
        title = glue::glue("Matched name for code: {input$search_text}"),
        glue::glue_collapse(names, sep = ","),
        easyClose = TRUE
      ))
    })

    # UI logic server  ----

    # Load data for analyzing
    load_vars <- load_data_server("load_data_module")

    # Cross-section analysis
    cs_analysis_server("cs_analysis_module",
      tsbl_vars = load_vars
    )

    # Time-series analysis
    ts_analysis_server("ts_analysis_module",
      tsbl_vars = load_vars
    )

    # Data dictionary
    data_dictionary_server("data_dictionary_module")
  })
}


#' Testing module app of explore_factor
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn explore_factor  Testing App of exploring factors.
explore_factor_app <- function(use_online_data = FALSE) {
  ui <- fluidPage(
    explore_factor_ui("explore_factor_module")
  )
  server <- function(input, output, session) {
    explore_factor_server("explore_factor_module")
  }
  shinyApp(ui, server)
}
