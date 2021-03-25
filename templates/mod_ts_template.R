#' {{module_name}}
#'
#' @description A shiny module for {{module_name}}.
#'
#' @details
#'  The module is an UI for user to display plots of ...
#'  by [`package_abc`][package_abc::package_abc] package.
#'
#' @name {{module_name}}
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   {{module_name}}_ui("{{module_name}}_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   {{module_name}} <- {{module_name}}_server(
#'   "{{module_name}}_module",
#'    tsbl_vars = reactive(tsbl_vars))
#' }
#'
#' # Run testing App for integration testing
#' {{module_name}}_app()
#' }
#'
NULL

#' UI function of {{module_name}}
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn {{module_name}}  UI function of {{module_name}}.
#' @importFrom shiny NS tagList
{{module_name}}_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # h1("{{module_name}}"),
    sidebarLayout(
      position = "right",
      sidebarPanel(
        width = 3,
        fluidRow(
          column(
            width = 6,
            actionButton(
              inputId = ns("update_output"),
              label = strong("Refresh output"),
              width = "100%"
            )
          ),
          column(
            width = 6,
            actionButton(
              inputId = ns("clear_focus"),
              label = strong("Clear selection"),
              width = "100%"
            ),
          )
        ),
        br(),
        wellPanel(
          selectInput(
            inputId = ns("ts_type"),
            label = strong("Time-series type:"),
            choices = c("stock", "industry")
          ),
          selectInput(
            inputId = ns("focus_var"),
            label = strong("Focus variables:"),
            multiple = TRUE,
            choices = ""
          ),
          selectInput(
            inputId = ns("focus_indcd"),
            label = strong("Focus industries:"),
            multiple = TRUE,
            choices = ""
          ),
          selectInput(
            inputId = ns("focus_stkcd"),
            label = strong("Focus stocks:"),
            multiple = TRUE,
            choices = ""
          ),
        ),

      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("ts_feature"),
          type = "tabs",
          tabPanel(
            "Feature stats",
            # DT::dataTableOutput(ns("stats_table"))
            # dataTableOutput(ns("stats_table"))
            ),
          tabPanel(
            "Feature plot",
            # plotly::plotlyOutput(ns("feature_plot"), height = "600px")
            # plotOutput(ns("feature_plot"), height = "600px")
          )
        )
      )
    )
  )
}

#' Server function of {{module_name}}
#'
#' @param tsbl_vars A tibble of vars of time series.
#'
#' @describeIn {{module_name}}  Server function of {{module_name}}.
#' @return * Server function return a data frame of ...
{{module_name}}_server <- function(id, tsbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(tsbl_vars))

    ## Logical reactive ----

    # Raw time series
    tsbl_vars_stock_raw <- reactive({
      tsbl_vars_stock_raw <- tsbl_vars()
      if ("period" %in% names(tsbl_vars_stock_raw)) {
        tsbl_vars_stock_raw <- tsbl_vars_stock_raw %>%
          periodize_index(period_field = "period")
      }
      tsbl_vars_stock_raw
    })

    tsbl_vars_industry_raw <- reactive({
      tsbl_vars_stock_raw() %>%
        industry_median()
    })

    # Raw time series
    tsbl_vars_stock_raw <- reactive({
      tsbl_vars_stock_raw <- tsbl_vars()
      if ("period" %in% names(tsbl_vars_stock_raw)) {
        tsbl_vars_stock_raw <- tsbl_vars_stock_raw %>%
          periodize_index(period_field = "period")
      }
      tsbl_vars_stock_raw
    })

    tsbl_vars_industry_raw <- reactive({
      tsbl_vars_stock_raw() %>%
        industry_median()
    })

    # Focused time series

    focus_var <- reactive({
      if (is.null(input$focus_var)) {
        zstmodelr::expect_type_fields(
          tsbl_vars_stock_raw(),
          expect_type = "numeric"
        )
      } else {
        req(input$focus_var)
      }
    })

    focus_stock <- reactive({
      if (is.null(input$focus_stkcd)) {
        unique(tsbl_vars_stock_raw()[["stkcd"]])
      } else {
        req(input$focus_stkcd)
      }
    })

    focus_industry <- reactive({
      if (is.null(input$focus_indcd)) {
        unique(tsbl_vars_stock_raw()[["indcd"]])
      } else {
        req(input$focus_indcd)
      }
    })

    tsbl_focus_stock <- eventReactive(input$update_output,
      # Build focus data when creating button and clicking button
      ignoreInit = FALSE,
      ignoreNULL = FALSE,
    {
      tsbl_vars_stock_raw() %>%
        dplyr::filter(
          .data$stkcd %in% focus_stock(),
          .data$indcd %in% focus_industry()
        ) %>%
        dplyr::select(
          c("date", "stkcd", "indcd"),
          tidyselect::all_of(focus_var())
        )
    })

    long_tsbl_focus_stock <- reactive({
      tsbl_focus_stock() %>%
        tidyr::pivot_longer(
          cols = -c("date", "stkcd", "indcd"),
          names_to = "variable", values_to = "value"
        )
    })

    tsbl_focus_industry <- eventReactive(input$update_output,
      # Build focus data when creating button and clicking button
      ignoreInit = FALSE,
      ignoreNULL = FALSE,
    {
      tsbl_vars_industry_raw() %>%
        dplyr::filter(.data$indcd %in% focus_industry()) %>%
        dplyr::select(
          c("date", "indcd"),
          tidyselect::all_of(focus_var())
        )
    })

    long_tsbl_focus_industry <- reactive({
      tsbl_focus_industry() %>%
        tidyr::pivot_longer(
          cols = -c("date", "indcd"),
          names_to = "variable", values_to = "value"
        )
    })

    long_tsbl_focus <- reactive({
      long_tsbl_focus_stock() %>%
        dplyr::left_join(
          long_tsbl_focus_industry(),
          by = c("date", "indcd", "variable"),
          suffix = c("_stock", "_industry")
        ) %>%
        dplyr::select(c(
          date, stkcd, indcd,
          variable, value_stock, value_industry
        )) %>%
        tsibble::as_tsibble(key = c(stkcd, indcd, variable))
    })

    # Available variables for choices
    available_variable_codes <- reactive({

      variable_codes <- zstmodelr::expect_type_fields(
        tsbl_vars_stock_raw(),
        expect_type = "numeric"
      )
      variable_codes <- sort(variable_codes)
      variable_names <- paste0(
        variable_codes,
        "(", code2name(variable_codes, exact_match = TRUE), ")"
      )
      variable_codes <- setNames(variable_codes, variable_names)

      variable_codes

    })

    # Available industries for choices
    available_industry_codes <- reactive({
      tsbl_vars <- tsbl_vars_stock_raw()
      industry_codes <- sort(unique(tsbl_vars$indcd))
      industry_names <- paste0(
        code2name(industry_codes, exact_match = TRUE),
        "(", industry_codes, ")"
      )
      industry_codes <- setNames(industry_codes, industry_names)

      industry_codes
    })

    # Available stocks for choices
    available_stock_codes <- reactive({
      if (is.null(input$focus_indcd)) {
        tsbl_vars <- tsbl_vars_stock_raw()
      } else {
        tsbl_vars <- tsbl_vars_stock_raw() %>%
          dplyr::filter(.data$indcd %in% input$focus_indcd)
      }

      stock_codes <- sort(unique(tsbl_vars$stkcd))
      stock_names <- paste0(
        code2name(stock_codes, exact_match = TRUE),
        "(", stock_codes, ")"
      )
      stock_codes <- setNames(stock_codes, stock_names)

      stock_codes
    })

    # Controls interaction ----

    # Update UI with dataset and user inputs
    observe({

      # Set choices for select inputs
      updateSelectInput(
        session = session, inputId = "focus_var",
        choices = available_variable_codes(),
        # Set selected with current value to ensure not clear current input
        selected = input$focus_var
      )

      updateSelectInput(
        session = session, inputId = "focus_indcd",
        choices = available_industry_codes(),
        # Set selected with current value to ensure not clear current input
        selected = input$focus_indcd
      )

      updateSelectInput(
        session = session, inputId = "focus_stkcd",
        choices = available_stock_codes(),
        # Set selected with current value to ensure not clear current input
        selected = input$focus_stkcd
      )
    })

    ## Output of features ----

    # output$stats_table <- DT::renderDataTable({
    output$stats_table <- renderDataTable({


    })

    #output$feature_plot <- plotly::renderPlotly({
    output$feature_plot <- renderPlot({

    })

  })
}

#' Testing module app of {{module_name}}
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn {{module_name}}  Testing App of {{module_name}}.
{{module_name}}_app <- function(use_online_data = FALSE) {

  # Prepare data
  tsbl_vars <- load_tsbl_vars(use_online_data = use_online_data)

  # Only show some stocks for demonstration
  focus_stocks <- c(
    "000651", "000333", "600066",
    "000550", "600031", "000157"
  )

  tsbl_vars <- tsbl_vars %>%
    dplyr::filter(stkcd %in% focus_stocks)

  ui <- fluidPage(
    {{module_name}}_ui("{{module_name}}_module")
  )
  server <- function(input, output, session) {
    {{module_name}}_server(
      "{{module_name}}_module",
      tsbl_vars = reactive(tsbl_vars)
    )
  }
  shinyApp(ui, server)
}
