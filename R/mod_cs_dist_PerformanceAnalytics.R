#' cs_dist_PerformanceAnalytics
#'
#' @description A shiny module for cs_dist_PerformanceAnalytics.
#'
#' @details The module is an UI for user to display plots of distribution by
#'   PerformanceAnalytics package.
#'
#' @name cs_dist_PerformanceAnalytics
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_dist_PerformanceAnalytics_ui("cs_dist_PerformanceAnalytics_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_dist_PerformanceAnalytics <- cs_dist_PerformanceAnalytics_server(
#'       "cs_dist_PerformanceAnalytics_module"
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_dist_PerformanceAnalytics_app()
#' }
#'
NULL

#' UI function of cs_dist_PerformanceAnalytics
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_dist_PerformanceAnalytics  UI function of
#'    cs_dist_PerformanceAnalytics.
#' @importFrom shiny NS tagList
cs_dist_PerformanceAnalytics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      position = "right",
      sidebarPanel(
        width = 3,
        selectInput(
          inputId = ns("value_scale"),
          label = strong("Value Scale:"),
          choices = c(
            "continuous", "log10", "reverse", "sqrt"
          ),
          selected = "continuous"
        ),

        sliderInput(
          inputId = ns("hist_bins"),
          label = strong("Histogram bins:"),
          min = 1,
          max = 100,
          value = 30,
          step = 1
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Boxplot",
            plotOutput(ns("boxplot_plot"))
          ),
          tabPanel(
            "Histogram",
            plotOutput(ns("histogram_plot"))
          ),
          tabPanel(
            "Density",
            plotOutput(ns("density_plot"))
          ),
          tabPanel(
            "qq-plot",
            plotOutput(ns("qq_plot"))
          )
        )
      )
    )
  )
}

#' Server function of cs_dist_PerformanceAnalytics
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn cs_dist_PerformanceAnalytics  Server function of
#' cs_dist_PerformanceAnalytics.
#' @return * Server function doesn't return value.
cs_dist_PerformanceAnalytics_server <- function(id, csbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(csbl_vars))

    csbl_continuous_vars <- reactive({
      csbl_vars() %>%
        dplyr::select(where(is.numeric))
    })

    csbl_discrete_vars <- reactive({
      csbl_vars() %>%
        dplyr::select(where(~ !is.numeric(.x)))
    })

    output$boxplot_plot <- renderPlot({
      csbl_continuous_vars() %>%
        PerformanceAnalytics::chart.Boxplot(
          plot.engine = "default",
          xlab = "Value",
          main = "Variable Distribution Comparison",
        )
    })
  })
}

#' Testing module app of cs_dist_PerformanceAnalytics
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_dist_PerformanceAnalytics  Testing App of cs_dist_PerformanceAnalytics.
cs_dist_PerformanceAnalytics_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data)

  ui <- fluidPage(
    cs_dist_PerformanceAnalytics_ui("cs_dist_PerformanceAnalytics_module")
  )
  server <- function(input, output, session) {
    cs_dist_PerformanceAnalytics <- cs_dist_PerformanceAnalytics_server(
      "cs_dist_PerformanceAnalytics_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}
