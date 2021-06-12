#' cs_dist_plotly
#'
#' @description A shiny module for cs_dist_plotly.
#'
#' @details
#'  The module is an UI for user to display plots of distribution
#'  by [`plotly`][plotly::plotly] package.
#'
#' @name cs_dist_plotly
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_dist_plotly_ui("cs_dist_plotly_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_dist_plotly <- cs_dist_plotly_server(
#'     "cs_dist_plotly_module",
#'     csbl_vars = reactive(csbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_dist_plotly_app()
#' }
#'
NULL

#' UI function of cs_dist_plotly
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_dist_plotly  UI function of cs_dist_plotly.
#' @importFrom shiny NS tagList
cs_dist_plotly_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      position = "right",
      sidebarPanel(
        width = 2,
        selectInput(
          inputId = ns("discrete_var"),
          label = strong("Discrete var:"),
          choices = ""
        ),
        selectInput(
          inputId = ns("continuous_var"),
          label = strong("Continuous var:"),
          choices = ""
        ),
        selectInput(
          inputId = ns("plot_method"),
          label = strong("Plot method:"),
          choices = c("plot_ly", "ggplot")
        ),
        actionButton(
          inputId = ns("clear_selection"),
          label = strong("Clear selection")
        )
      ),
      mainPanel(
        width = 10,
        fluidRow(
          box(
            title = "Discrete Varable", status = "primary",
            solidHeader = TRUE, collapsible = TRUE, width = 6,
            plotly::plotlyOutput(ns("discrete_freqbar"), height = 260)
          ),
          box(
            title = "Continuous Varable", status = "primary",
            solidHeader = TRUE, collapsible = TRUE, width = 6,
            tabsetPanel(
              id = ns("continuous_plot_tabs"),
              type = "tabs",
              tabPanel(
                "Boxplot",
                plotly::plotlyOutput(ns("continuous_boxplot"), height = 220)
              ),
              tabPanel(
                "Histogram",
                plotly::plotlyOutput(ns("continuous_hist"), height = 220)
              ),
              tabPanel(
                "Density",
                plotly::plotlyOutput(ns("continuous_density"), height = 220)
              ),
              tabPanel(
                "qq-plot",
                plotly::plotlyOutput(ns("continuous_qq"), height = 220)
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Selected Data ", status = "primary",
            solidHeader = TRUE, collapsible = TRUE, width = 12,
            DT::dataTableOutput(ns("selected_table"))
          )
        )
      )
    )
  )
}

#' Server function of cs_dist_plotly
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn cs_dist_plotly  Server function of cs_dist_plotly.
#' @return * Server function return a data frame of ...
cs_dist_plotly_server <- function(id, csbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(csbl_vars))

    # Logic reactive ----

    # Record selection of plotly controls
    select_indicator <- reactiveValues()

    # Return selected data by plotly controls
    select_data <- reactive({

      # Build up final selection from plotly controls' selection
      final_selection <- rep(TRUE, NROW(csbl_vars()))
      for (control_selection in reactiveValuesToList(select_indicator)) {
        # Apply selection from a control only if it had some selection
        if (any(control_selection)) {
          final_selection <- final_selection & control_selection
        }
      }

      # Get selected data
      csbl_vars() %>%
        dplyr::filter(final_selection)
    })


    # Map action of plotly controls to selection ----

    # Map selection from discrete_freqbar
    observeEvent(plotly::event_data("plotly_selected",
      source = "discrete_freqbar"
    ), ignoreInit = TRUE, {
      origin_data <- csbl_vars()
      evt <- plotly::event_data("plotly_selected",
        source = "discrete_freqbar"
      )
      if (!is.null(evt)) {
        var_column <- origin_data[[isolate(input$discrete_var)]]
        if (is.character(evt$x)) {
          selection <- var_column %in% evt$x
        } else {
          selection <- var_column %in% sort(unique(var_column))[evt$x]
        }
        select_indicator$discrete_var <- selection
      }
    })


    # Map selection from continuous_boxplot
    observeEvent(plotly::event_data("plotly_brushed",
      source = "continuous_boxplot"
    ), ignoreInit = TRUE, {
      origin_data <- csbl_vars()
      evt <- plotly::event_data("plotly_brushed",
        source = "continuous_boxplot"
      )
      if (!is.null(evt)) {
        selection <- dplyr::between(
          origin_data[[isolate(input$continuous_var)]],
          evt$x[[1]], evt$x[[2]]
        )
        select_indicator$continuous_var <- selection
      }
    })

    # Map selection from continuous_hist
    observeEvent(plotly::event_data("plotly_brushed",
      source = "continuous_hist"
    ), ignoreInit = TRUE, {
      origin_data <- csbl_vars()
      evt <- plotly::event_data("plotly_brushed",
        source = "continuous_hist"
      )
      if (!is.null(evt)) {
        selection <- dplyr::between(
          origin_data[[isolate(input$continuous_var)]],
          evt$x[[1]], evt$x[[2]]
        )
        select_indicator$continuous_var <- selection
      }
    })

    # Map selection from continuous_density
    observeEvent(plotly::event_data("plotly_brushed",
      source = "continuous_density"
    ), ignoreInit = TRUE, {
      origin_data <- csbl_vars()
      evt <- plotly::event_data("plotly_brushed",
        source = "continuous_density"
      )
      if (!is.null(evt)) {
        selection <- dplyr::between(
          origin_data[[isolate(input$continuous_var)]],
          evt$x[[1]], evt$x[[2]]
        )
        select_indicator$continuous_var <- selection
      }
    })

    # Map selection from continuous_qq
    observeEvent(plotly::event_data("plotly_brushed",
                                    source = "continuous_qq"
    ), ignoreInit = TRUE, {
      origin_data <- csbl_vars()
      evt <- plotly::event_data("plotly_brushed",
                                source = "continuous_qq"
      )
      if (!is.null(evt)) {

        selection <- dplyr::between(
          origin_data[[isolate(input$continuous_var)]],
          evt$y[[1]], evt$y[[2]]
        )
        select_indicator$continuous_var <- selection
      }
    })


    # Controls Interaction ----

    # Update UI with dataset and user inputs
    observe({
      discrete_vars <- csbl_vars() %>%
        dplyr::select(where(~ !is.numeric(.x))) %>%
        names()
      discrete_vars <- setdiff(discrete_vars, "id")

      continuous_vars <- csbl_vars() %>%
        dplyr::select(where(~ is.numeric(.x))) %>%
        names()

      # Set choices for select inputs
      updateSelectInput(
        session = session, inputId = "discrete_var",
        choices = discrete_vars
      )

      updateSelectInput(
        session = session, inputId = "continuous_var",
        choices = continuous_vars
      )
    })

    # Clear maping selection of ploty controls
    observeEvent(input$clear_selection, ignoreInit = TRUE, {
      select_indicator$discrete_var <- rep(
        FALSE,
        length(select_indicator$discrete_var)
      )
      select_indicator$continuous_var <- rep(
        FALSE,
        length(select_indicator$continuous_var)
      )
    })

    # Plot vars distribution ----

    output$discrete_freqbar <- plotly::renderPlotly({

      csbl_vars() %>%
        freqbar_plotly(
          var_name = req(input$discrete_var),
          ds_vars_compare = select_data(),
          plot_method = input$plot_method,
          source_id = "discrete_freqbar"
        ) %>%
        plotly::layout(
          dragmode = "select",
          selectdirection = "h"
        ) %>%
        plotly::event_register(event = "plotly_brushed")
    })

    output$continuous_boxplot <- plotly::renderPlotly({
      csbl_vars() %>%
        boxplot_plotly(
          var_name = req(input$continuous_var),
          ds_vars_compare = select_data(),
          plot_method = input$plot_method,
          source_id = "continuous_boxplot"
        ) %>%
        plotly::layout(
          dragmode = "select",
          selectdirection = "h"
        ) %>%
        plotly::event_register(event = "plotly_brushed") %>%
        plotly::event_register(event = "plotly_doubleclick")
    })

    output$continuous_hist <- plotly::renderPlotly({
      csbl_vars() %>%
        hist_plotly(
          var_name = req(input$continuous_var),
          ds_vars_compare = select_data(),
          plot_method = input$plot_method,
          source_id = "continuous_hist"
        ) %>%
        plotly::layout(
          dragmode = "select",
          selectdirection = "h"
        ) %>%
        plotly::event_register(event = "plotly_brushed")
    })

    output$continuous_density <- plotly::renderPlotly({
      csbl_vars() %>%
        density_plotly(
          var_name = req(input$continuous_var),
          ds_vars_compare = select_data(),
          plot_method = input$plot_method,
          source_id = "continuous_density"
        ) %>%
        plotly::layout(
          dragmode = "select",
          selectdirection = "h"
        ) %>%
        plotly::event_register(event = "plotly_brushed")
    })


    output$continuous_qq <- plotly::renderPlotly({

      csbl_vars() %>%
        qqplot_plotly(
          var_name = req(input$continuous_var),
          ds_vars_compare = select_data(),
          plot_method = input$plot_method,
          source_id = "continuous_qq"
        ) %>%
        plotly::layout(
          dragmode = "select",
          selectdirection = "d"
        ) %>%
        plotly::event_register(event = "plotly_brushed")

    })

    output$selected_table <- DT::renderDataTable({
      continuous_vars <- csbl_vars() %>%
        dplyr::select(where(~ is.numeric(.x))) %>%
        names()

      DT::datatable(
        select_data(),
        filter = "top",
        extensions = "Scroller",
        options = list(
          columnDefs = list(list(className = "dt-center")),
          pageLength = 5,
          dom = 'ltir',
          deferRender = TRUE,
          scrollY = 180,
          scrollX = TRUE,
          scroller = TRUE
        )
      ) %>%
        DT::formatRound(columns = continuous_vars, digits = 2)
    })
  })
}

#' Testing module app of cs_dist_plotly
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_dist_plotly  Testing App of cs_dist_plotly.
cs_dist_plotly_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data = FALSE)

  ui <- fluidPage(
    cs_dist_plotly_ui("cs_dist_plotly_module")
  )
  server <- function(input, output, session) {
    cs_dist_plotly_server(
      "cs_dist_plotly_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}

