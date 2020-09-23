#' cs_dist_DataExplorer
#'
#' @description A shiny module for cs_dist_DataExplorer.
#'
#' @details
#'  The module is an UI for user to display distribution by
#'  [`DataExplorer`][DataExplorer::DataExplorer] package.
#'
#' @name cs_dist_DataExplorer
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_dist_DataExplorer_ui("cs_dist_DataExplorer_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_dist_DataExplorer <- cs_dist_DataExplorer_server(
#'     "cs_dist_DataExplorer_module",
#'     csbl_vars = reactive(csbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_dist_DataExplorer_app()
#' }
#'
NULL

#' UI function of cs_dist_DataExplorer
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_dist_DataExplorer  UI function of cs_dist_DataExplorer.
#' @importFrom shiny NS tagList
cs_dist_DataExplorer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      position = "right",
      sidebarPanel(
        width = 3,
        tabsetPanel(
          id = ns("setting_tabs"),
          type = "hidden",
          tabPanelBody(
            value = "discrete_vars",
            h3("Discrete variables"),
            sliderInput(
              inputId = ns("max_cats"),
              label = strong("Max categories allowed for each feature:"),
              min = 1,
              max = 200,
              value = 100,
              step = 1
            )
          ),
          tabPanelBody(
            value = "continous_vars",
            h3("Continuous variables"),
            selectInput(
              inputId = ns("value_scale"),
              label = strong("Value scale:"),
              choices = c("continuous", "log10", "reverse", "sqrt")
            ),
            sliderInput(
              inputId = ns("hist_bins"),
              label = strong("Hisogram bins:"),
              min = 1,
              max = 100,
              value = 30,
              step = 1
            )
          )
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("plot_tabs"),
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
          ),
          tabPanel(
            "Bar Chart(freq)",
            plotOutput(ns("bar_char"))
          )
        )
      )
    )
  )
}

#' Server function of cs_dist_DataExplorer
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn cs_dist_DataExplorer  Server function of cs_dist_DataExplorer.
#' @return * Server function doesn't return value.
cs_dist_DataExplorer_server <- function(id, csbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(csbl_vars))

    # Focus csbl_vars for analyzing
    csbl_vars_focus <- reactive({
      csbl_vars() %>%
        dplyr::select(-c("id"))
    })

    # Update UI when user choose plot tabs
    observeEvent(input$plot_tabs, ignoreInit = TRUE, {

      # Update setting_tabs according to plot type
      if (input$plot_tabs %in% c("Bar Chart(freq)")) {
        updateTabsetPanel(session,
          inputId = "setting_tabs", # notice: don't use ns("setting_tabs")
          selected = "discrete_vars"
        )
      } else {
        updateTabsetPanel(session,
          inputId = "setting_tabs",
          selected = "continous_vars"
        )
      }
    })

    output$bar_char <- renderPlot(
      # width = 800,
      height = 600,
      {
        csbl_vars_focus() %>%
          DataExplorer::plot_bar(maxcat = input$max_cats)
      }
    )

    output$boxplot_plot <- renderPlot({
      csbl_vars_focus() %>%
        dplyr::mutate(id = "value") %>%
        DataExplorer::plot_boxplot(
          by = "id",
          geom_boxplot_args = list(
            outlier.color = "red",
            outlier.alpha = 0.10
          ),
          scale_y = input$value_scale
        )
    })

    output$histogram_plot <- renderPlot({
      csbl_vars_focus() %>%
        DataExplorer::plot_histogram(
          geom_histogram_args = list(
            bins = input$hist_bins
          ),
          scale_x = input$value_scale
        )

      updateTabsetPanel(session,
        inputId = "setting_tabs",
        selected = "continous_vars"
      )
    })

    output$density_plot <- renderPlot({
      csbl_vars_focus() %>%
        DataExplorer::plot_density(
          scale_x = input$value_scale
        )
    })

    output$qq_plot <- renderPlot({
      csbl_vars_focus() %>%
        DataExplorer::plot_qq()
    })
  })
}

#' Testing module app of cs_dist_DataExplorer
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_dist_DataExplorer  Testing App of cs_dist_DataExplorer.
cs_dist_DataExplorer_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data)

  ui <- fluidPage(
    cs_dist_DataExplorer_ui("cs_dist_DataExplorer_module")
  )
  server <- function(input, output, session) {
    cs_dist_DataExplorer_server(
      "cs_dist_DataExplorer_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}
