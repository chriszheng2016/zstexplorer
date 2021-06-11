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
#'    tsbl_vars = reactive(tsbl_vars)),
#'    tsbl_vars_average = reactive(tsbl_vars_average)
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
            offset = 1,
            width = 10,
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
            label = strong("Focus variable:"),
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
            "Feature stats1",

            # DT::dataTableOutput(ns("feat_stats_table")),
            dataTableOutput(ns("feat_stats_table")),

            # plotly::plotlyOutput(ns("feat_stats_plot"), height = "600px"),
            plotOutput(ns("feat_stats_plot"), height = "600px"),
            #
            ),
          tabPanel(
            "Feature stats2",
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
#' @param tsbl_vars_average A tsibble of average of vars of time series.
#'
#' @describeIn {{module_name}}  Server function of {{module_name}}.
#' @return * Server function return a data frame of ...
{{module_name}}_server <- function(id, tsbl_vars, tsbl_vars_average) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(tsbl_vars))
    assertive::assert_all_are_true(is.reactive(tsbl_vars_average))

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
      tsbl_vars_industry_raw <- tsbl_vars_average()
      if ("period" %in% names(tsbl_vars_industry_raw)) {
        tsbl_vars_industry_raw <- tsbl_vars_industry_raw %>%
          periodize_index(period_field = "period")
      }
      tsbl_vars_industry_raw
    })

    # ID var of stock time series
    stock_id_var <- reactive({
      key_vars <- tsibble::key_vars(tsbl_vars_stock_raw())

      # Use first var as id var
      id_var <- key_vars[1]

      id_var
    })

    # ID var of industry time series
    industry_id_var <- reactive({
      key_vars <- tsibble::key_vars(tsbl_vars_industry_raw())

      # Use first var as id var
      id_var <- key_vars[1]

      id_var
    })

    # Focused time series
    focus_var <- reactive({
      if (!isTruthy(input$focus_var)) {
        zstmodelr::expect_type_fields(
          tsbl_vars_stock_raw(),
          expect_type = "numeric"
        )
      } else {
        input$focus_var
      }
    })

    tsbl_focus_stock <- reactive({
      tsbl_vars_stock_raw() %>%
        dplyr::select(
          c(
            tsibble::index_var(.), tsibble::key_vars(.),
            industry_id_var()
          ),
          tidyselect::all_of(focus_var())
        )
    })

    long_tsbl_focus_stock <- reactive({
      tsbl_focus_stock() %>%
        tidyr::pivot_longer(
          cols = -c(
            tsibble::index_var(.), tsibble::key_vars(.),
            industry_id_var()
          ),
          names_to = "variable", values_to = "value"
        )
    })

    tsbl_focus_industry <- reactive({
      tsbl_vars_industry_raw() %>%
        dplyr::select(
          c(tsibble::index_var(.), tsibble::key_vars(.)),
          tidyselect::all_of(focus_var())
        )
    })

    long_tsbl_focus_industry <- reactive({
      tsbl_focus_industry() %>%
        tidyr::pivot_longer(
          cols = -c(tsibble::index_var(.), tsibble::key_vars(.)),
          names_to = "variable", values_to = "value"
        )
    })

    long_tsbl_focus <- reactive({
      long_tsbl_focus_stock() %>%
        dplyr::left_join(
          long_tsbl_focus_industry(),
          by = c(tsibble::index_var(.), industry_id_var(), "variable"),
          suffix = c("_stock", "_industry")
        ) %>%
        dplyr::select(c(
          tsibble::index_var(.), stock_id_var(), industry_id_var(),
          "variable", "value_stock", "value_industry"
        ))
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

    # Controls interaction ----

    # Update UI with dataset and user inputs
    observe({

      # Set choices for select inputs
      updateSelectInput(
        session = session, inputId = "focus_var",
        choices = available_variable_codes(),
        # Set selected with current value to ensure not clear current input
        # selected = input$focus_var
      )

    })

    # Handler for user to clear focus input for stock and industry
    clear_focus <- function() {

      # Clear selections in table
      # dt_table <- "feat_stats_table"
      # proxy <- DT::dataTableProxy(dt_table)
      # DT::selectRows(proxy, selected = NULL)

      # Refresh output
      # shinyjs::click(id = "update_output")
    }

    # Click to clear focus inputs for stock and industry
    observeEvent(input$clear_focus, ignoreInit = TRUE, {
      clear_focus()
    })




    ## Output of features ----

    # output$feat_stats_table <- DT::renderDataTable({
    output$feat_stats_table <- renderDataTable({


    })

    #output$feat_stats_plot <- plotly::renderPlotly({
    output$feat_stats_plot <- renderPlot({


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
  tsbl_vars_average <- industry_mean(tsbl_vars)

  # Only show some stocks for demonstration
  focus_stocks <- c(
    "000651", "000333", "600066",
    "000550", "600031", "000157"
  )

  tsbl_vars <- tsbl_vars %>%
    dplyr::filter(.data$stkcd %in% focus_stocks)

  focus_industries <- unique(tsbl_vars$indcd)

  tsbl_vars_average <- tsbl_vars_average %>%
    dplyr::filter(.data$indcd %in% focus_industries)

  ui <- fluidPage(
    {{module_name}}_ui("{{module_name}}_module")
  )
  server <- function(input, output, session) {
    {{module_name}}_server(
      "{{module_name}}_module",
      tsbl_vars = reactive(tsbl_vars),
      tsbl_vars_average = reactive(tsbl_vars_average)
    )
  }
  shinyApp(ui, server)
}
