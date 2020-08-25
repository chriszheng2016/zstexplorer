#' cs_missing_visdat
#'
#' @description A shiny module for cs_missing_visdat.
#'
#' @details
#'  The module is an UI for user to display missing pattern by
#'  [`visdat`][visdat::visdat] package.
#'
#' @name cs_missing_visdat
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_missing_visdat_ui("cs_missing_visdat_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_missing_visdat <- cs_missing_visdat_server(
#'      "cs_missing_visdat_module"
#'      csbl_vars = reactive(csbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_missing_visdat_app()
#' }
#'
NULL

#' UI function of cs_missing_visdat
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_missing_visdat  UI function of cs_missing_visdat.
#' @importFrom shiny NS tagList
cs_missing_visdat_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      position = "right",
      sidebarPanel(
        width = 3,
        sliderInput(
          inputId = ns("show_from"),
          label = strong("Show from:"),
          min = 1,
          max = 100,
          value = 1,
          step = 1
        ),
        selectInput(
          inputId = ns("show_window_size"),
          label = strong("Window Size:"),
          choices = c(50, 100, 500, 1000, 10000, 20000, 30000, 40000, 50000),
          selected = 500
        )
      ),
      mainPanel(
        width = 9,
        box(
         title = "Missing Pattern", status = "primary",
         solidHeader = TRUE, collapsible = TRUE,
         width = 12,

         plotOutput(ns("missing_plot"), height = 600)
        )
      )
    )
  )
}

#' Server function of cs_missing_visdat
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn cs_missing_visdat  Server function of cs_missing_visdat.
#' @return * Server function doesn't return value.
cs_missing_visdat_server <- function(id, csbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(csbl_vars))

    # Update UI with dataset and user inputs
    observe({

      updateSliderInput(
        session = session, inputId = "show_from",
        min = 1,
        max = nrow(csbl_vars()),
        value = 1
      )

    })

    output$missing_plot <- renderPlot(
      {
        show_from <- input$show_from
        show_to <- show_from + as.numeric(input$show_window_size) - 1

        csbl_vars()%>%
          dplyr::slice({{show_from}}:{{show_to}}) %>%
          visdat::vis_miss()
      }
    )
  })
}

#' Testing module app of cs_missing_visdat
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_missing_visdat  Testing App of cs_missing_visdat.
cs_missing_visdat_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data)

  ui <- fluidPage(
    cs_missing_visdat_ui("cs_missing_visdat_module")
  )
  server <- function(input, output, session) {
    cs_missing_visdat_server(
      "cs_missing_visdat_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}
