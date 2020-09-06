#' cs_cor_plotly
#'
#' @description A shiny module for cs_cor_plotly.
#'
#' @details
#'  The module is an UI for user to display plots of ...
#'  by [`package_abc`][package_abc::package_abc] package.
#'
#' @name cs_cor_plotly
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_cor_plotly_ui("cs_cor_plotly_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_cor_plotly <- cs_cor_plotly_server(
#'     "cs_cor_plotly_module",
#'     csbl_vars = reactive(csbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_cor_plotly_app()
#' }
#'
NULL

#' UI function of cs_cor_plotly
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_cor_plotly  UI function of cs_cor_plotly.
#' @importFrom shiny NS tagList
cs_cor_plotly_ui <- function(id) {
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
          inputId = ns("discrete_var_levels"),
          label = strong("Top discrete levels to display:"),
          choices = as.character(1:10),
          selected = "5"
        ),
        selectInput(
          inputId = ns("continuous_var_x"),
          label = strong("Continuous var_x:"),
          choices = ""
        ),

        selectInput(
          inputId = ns("continuous_var_y"),
          label = strong("Continuous var_y:"),
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
                "Scatter",
                plotly::plotlyOutput(ns("continuous_scatter"), height = 220)
              ),
              tabPanel(
                "Combo chart",
                plotly::plotlyOutput(ns("continuous_combochart"), height = 220)
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

#' Server function of cs_cor_plotly
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn cs_cor_plotly  Server function of cs_cor_plotly.
#' @return * Server function return a data frame of ...
cs_cor_plotly_server <- function(id, csbl_vars) {
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
    ), {
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

    # Map selection from continuous_scatter
    observeEvent(plotly::event_data("plotly_brushed",
      source = "continuous_scatter"
    ), {
      origin_data <- csbl_vars()
      evt <- plotly::event_data("plotly_brushed",
        source = "continuous_scatter"
      )

      if (!is.null(evt)) {
        selection_x <- between(
          origin_data[[isolate(input$continuous_var_x)]],
          evt$x[[1]], evt$x[[2]]
        )
        selection_y <- between(
          origin_data[[isolate(input$continuous_var_y)]],
          evt$y[[1]], evt$y[[2]]
        )

        new_selection <- (selection_x & selection_y)
        select_indicator$continuous_var <- new_selection
      }

    })

    # Map selection from continuous_combochart
    observeEvent(plotly::event_data("plotly_brushed",
                                    source = "continuous_combochart"
    ), {
      origin_data <- csbl_vars()
      evt <- plotly::event_data("plotly_brushed",
                                source = "continuous_combochart"
      )
      if (!is.null(evt)) {
        selection <- between(
          origin_data[[isolate(input$continuous_var_x)]],
          evt$x[[1]], evt$x[[2]]
        )
        select_indicator$continuous_var <- selection
      }

    })


    # Controls interaction ----

    # Update UI with dataset and user inputs
    observe({
      discrete_vars <- csbl_vars() %>%
        dplyr::select(where(~ !is.numeric(.x))) %>%
        names()

      continuous_vars <- csbl_vars() %>%
        dplyr::select(where(~ is.numeric(.x))) %>%
        names()

      # Set choices for select inputs
      updateSelectInput(
        session = session, inputId = "discrete_var",
        choices = discrete_vars
      )

      continuous_vars_x <- continuous_vars
      updateSelectInput(
        session = session, inputId = "continuous_var_x",
        choices = continuous_vars_x,
        # Set selected with current value to ensure not clear current input
        selected = input$continuous_var_x
      )

      continuous_vars_y <- setdiff(continuous_vars, input$continuous_var_x)
      updateSelectInput(
        session = session, inputId = "continuous_var_y",
        choices = continuous_vars_y,
        # Set selected with current value to ensure not clear current input
        selected = input$continuous_var_y
      )
    })

    # Clear maping selection of ploty controls
    observeEvent(input$clear_selection, {
      select_indicator$discrete_var <- rep(
        FALSE,
        length(select_indicator$discrete_var)
      )
      select_indicator$continuous_var <- rep(
        FALSE,
        length(select_indicator$continuous_var)
      )
    })



    # Plot vars correlation ----
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


    output$continuous_scatter <- plotly::renderPlotly({
      csbl_vars() %>%
        scatter_plotly(
          x_var_name = req(input$continuous_var_x),
          y_var_name = req(input$continuous_var_y),
          ds_vars_compare = select_data(),
          plot_method = input$plot_method,
          source_id = "continuous_scatter"
        ) %>%
        plotly::layout(
          dragmode = "select",
          selectdirection = "d"
        ) %>%
        plotly::event_register(event = "plotly_brushed")
    })

    output$continuous_combochart <- plotly::renderPlotly({

      csbl_vars() %>%
        combochat_plotly(
          continuous_var_name = req(input$continuous_var_x),
          discrete_var_name = req(input$discrete_var),
          ds_vars_compare = select_data(),
          top_levels = as.numeric(input$discrete_var_levels),
          plot_method = input$plot_method,
          source_id = "continuous_combochart"
        ) %>%
        plotly::layout(
          dragmode = "select",
          selectdirection = "h"
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

#' Testing module app of cs_cor_plotly
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_cor_plotly  Testing App of cs_cor_plotly.
cs_cor_plotly_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data = FALSE)

  ui <- fluidPage(
    cs_cor_plotly_ui("cs_cor_plotly_module")
  )
  server <- function(input, output, session) {
    cs_cor_plotly_server(
      "cs_cor_plotly_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}
