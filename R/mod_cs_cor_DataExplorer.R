#' cs_cor_DataExplorer
#'
#' @description A shiny module for cs_cor_DataExplorer.
#'
#' @details
#'  The module is an UI for user to display plots of correlation
#'  by [`DataExplorer`][DataExplorer::DataExplorer] package.
#'
#' @name cs_cor_DataExplorer
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#'
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_cor_DataExplorer_ui("cs_cor_DataExplorer_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_cor_DataExplorer <- cs_cor_DataExplorer_server(
#'     "cs_cor_DataExplorer_module",
#'     csbl_vars = reactive(csbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_cor_DataExplorer_app()
#' }
#'
NULL

#' UI function of cs_cor_DataExplorer
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_cor_DataExplorer  UI function of cs_cor_DataExplorer.
#' @importFrom shiny NS tagList
cs_cor_DataExplorer_ui <- function(id) {
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
            value = "continous_vars",
            h3("Continuous variables"),
            selectInput(
              inputId = ns("cor_method"),
              label = strong("cor method:"),
              choices = c("pearson", "spearman")
            ),
            selectInput(
              inputId = ns("cor_use"),
              label = strong("cor use:"),
              choices = c(
                "everything",
                "all.obs",
                "complete.obs",
                "na.or.complete",
                "pairwise.complete.obs"
              ),
              selected = "pairwise.complete.obs"
            )
          ),
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
          )
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("plot_tabs"),
          type = "tabs",
          tabPanel(
            "Continuous vars",
            plotOutput(ns("continuous_corrlation"))
          ),
          tabPanel(
            "Discrete vars",
            plotOutput(ns("discrete_corrlation"))
          )
        )
      )
    )
  )
}

#' Server function of cs_cor_DataExplorer
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn cs_cor_DataExplorer  Server function of cs_cor_DataExplorer.
#' @return * Server function doesn't return value.
cs_cor_DataExplorer_server <- function(id, csbl_vars) {
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
    observeEvent(input$plot_tabs, {

      # Update setting_tabs according to plot type
      if (input$plot_tabs %in% c("Discrete vars")) {
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

    output$continuous_corrlation <- renderPlot({

      cor_method <- input$cor_method
      cor_use <- input$cor_use

      csbl_vars_focus() %>%
        DataExplorer::plot_correlation(
          type = "continuous",
          cor_args = list(
            method = cor_method,
            use = cor_use
          )
        )
    })

    output$discrete_corrlation <- renderPlot({

      cor_method <- input$cor_method
      cor_use <- input$cor_use

      csbl_vars_focus() %>%
        DataExplorer::plot_correlation(
          type = "discrete",
          maxcat = input$max_cats
        )
    })
  })
}

#' Testing module app of cs_cor_DataExplorer
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_cor_DataExplorer  Testing App of cs_cor_DataExplorer.
cs_cor_DataExplorer_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data)

  ui <- fluidPage(
    cs_cor_DataExplorer_ui("cs_cor_DataExplorer_module")
  )
  server <- function(input, output, session) {
    cs_cor_DataExplorer_server(
      "cs_cor_DataExplorer_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}
