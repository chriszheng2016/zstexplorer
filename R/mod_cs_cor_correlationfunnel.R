#' cs_cor_correlationfunnel
#'
#' @description A shiny module for cs_cor_correlationfunnel.
#'
#' @details
#'  The module is an UI for user to to display plots of correlation
#'  by [`correlationfunnel`][correlationfunnel::correlationfunnel] package.
#'
#' @name cs_cor_correlationfunnel
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_cor_correlationfunnel_ui("cs_cor_correlationfunnel_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_cor_correlationfunnel <- cs_cor_correlationfunnel_server(
#'     "cs_cor_correlationfunnel_module",
#'     csbl_vars = reactive(csbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_cor_correlationfunnel_app()
#' }
#'
NULL

#' UI function of cs_cor_correlationfunnel
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_cor_correlationfunnel  UI function of cs_cor_correlationfunnel.
#' @importFrom shiny NS tagList
cs_cor_correlationfunnel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      position = "right",
      sidebarPanel(
        width = 3,

        selectInput(
          inputId = ns("target_var"),
          label = strong("Target Varable:"),
          choices = NULL
        ),

        selectInput(
          inputId = ns("target_var_level"),
          label = strong("Target Level:"),
          choices = NULL
        ),

        sliderInput(
          inputId = ns("continuous_bins"),
          label = strong("Bins for continuous var:"),
          min = 0,
          max = 10,
          value = 2,
          step = 1
        ),

        sliderInput(
          inputId = ns("discrete_thresh_infreq"),
          label = strong("Infrequent threshold for discrete var:"),
          min = 0.0,
          max = 0.5,
          value = 0.01,
          step = 0.01
        ),

        sliderInput(
          inputId = ns("reference_level"),
          label = strong("reference level:"),
          min = 0.1,
          max = 1,
          value = 0.3,
          step = 0.05
        ),
      ),
      mainPanel(
        width = 9,
        plotOutput(ns("correlation_funnel_plot"))
      )
    )
  )
}

#' Server function of cs_cor_correlationfunnel
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn cs_cor_correlationfunnel  Server function of cs_cor_correlationfunnel.
#' @return * Server function doesn't return value.
cs_cor_correlationfunnel_server <- function(id, csbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(csbl_vars))

    # Binarize csbl_vars
    vars_binarized <- reactive({
      csbl_vars() %>%
        na.omit() %>%
        correlationfunnel::binarize(
          n_bins = req(input$continuous_bins),
          thresh_infreq = req(input$discrete_thresh_infreq)
        )
    })

    # Compute correlations
    vars_correlation <- reactive({
      vars_binarized() %>%
        correlationfunnel::correlate(target = req(input$target_var_level))
    })

    # Update UI with dataset and user inputs
    observe({

      # Target variable input
      target_vars <- names(csbl_vars())
      updateSelectInput(
        session = session, inputId = "target_var",
        choices = target_vars,
        # Set selected with current value to ensure not clear current input
        selected = input$target_var
      )

      # Target variable level input
      target_var_levels <- grep(
        paste0("^", req(input$target_var)),
        x = names(vars_binarized()),
        value = TRUE
      )
      updateSelectInput(
        session = session, inputId = "target_var_level",
        choices = target_var_levels,
        selected = ""
      )
    })


    output$correlation_funnel_plot <- renderPlot({

      # Compute limits of vars_correlation
      vars_correlation_limit <-
        vars_correlation() %>%
        dplyr::group_by(.data$feature) %>%
        dplyr::mutate(
          max = max(.data$correlation),
          min = min(.data$correlation)
        )

      # Draw funnel plot
      correlation_plot <- vars_correlation() %>%
        correlationfunnel::plot_correlation_funnel() +
        # add reference line
        geom_vline(
          xintercept = c(-input$reference_level, input$reference_level),
          color = "blue",
          linetype = "dotted"
        ) +
        geom_text(
          aes(
            x = -input$reference_level, y = 0,
            label = input$reference_level
          ),
          color = "blue", size = 3, vjust = -0.5
        ) +
        geom_text(
          aes(
            x = input$reference_level, y = 0,
            label = input$reference_level
          ),
          color = "blue", size = 3, vjust = -0.5
        ) +
        # mark target variable and level
        geom_hline(
          yintercept = req(input$target_var),
          color = "red", linetype = "longdash",
          size = 1, alpha = 0.3
        ) +
        geom_point(aes(x = 1, y = req(input$target_var)),
          color = "red", shape = 18, size = 3
        ) +
        # mark limit point of each feature
        geom_point(aes(x = max, y = feature),
          data = vars_correlation_limit,
          color = "orange", shape = 3, size = 3
        ) +
        geom_point(aes(x = min, y = feature),
          data = vars_correlation_limit,
          color = "lightblue", shape = 3, size = 3
        )

      return(correlation_plot)
    })
  })
}

#' Testing module app of cs_cor_correlationfunnel
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_cor_correlationfunnel  Testing App of cs_cor_correlationfunnel.
cs_cor_correlationfunnel_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data)

  ui <- fluidPage(
    cs_cor_correlationfunnel_ui("cs_cor_correlationfunnel_module")
  )
  server <- function(input, output, session) {
    cs_cor_correlationfunnel_server(
      "cs_cor_correlationfunnel_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}
