#' ts_missing_tidyverts
#'
#' @description A shiny module for ts_missing_tidyverts.
#'
#' @details
#'  The module is an UI for user to explore missing values in time series
#'  by tidyverts family packages.
#'
#' @name ts_missing_tidyverts
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   ts_missing_tidyverts_ui("ts_missing_tidyverts_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   ts_missing_tidyverts <- ts_missing_tidyverts_server(
#'     "ts_missing_tidyverts_module",
#'     tsbl_vars = reactive(tsbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' ts_missing_tidyverts_app()
#' }
#'
NULL

#' UI function of ts_missing_tidyverts
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn ts_missing_tidyverts  UI function of ts_missing_tidyverts.
#' @importFrom shiny NS tagList
ts_missing_tidyverts_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      position = "right",
      sidebarPanel(
        width = 3,
        selectInput(
          inputId = ns("ts_type"),
          label = strong("Time-series type:"),
          choices = c("stock", "industry")
        ),
        selectInput(
          inputId = ns("full_gap"),
          label = strong("Full gap:"),
          choices = c("TRUE", "FALSE", "start()", "end()")
        ),
        selectInput(
          inputId = ns("output_type"),
          label = strong("Output Type:"),
          choices = c("Fill gaps + impute nas", "Only fill gaps", "Origin")
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("missing_value"),
          type = "tabs",
          tabPanel(
            "Original NAs in ts",
            plotOutput(ns("original_nas_plot"), height = "600px")
          ),
          tabPanel(
            "Fill gaps with NAs",
            plotOutput(ns("fill_gaps_plot"), height = "600px")
          ),
          tabPanel(
            "Imputing NAs",
            plotOutput(ns("impute_nas_plot"), height = "600px")
          )
        )
      )
    )
  )
}

#' Server function of ts_missing_tidyverts
#'
#' @param tsbl_vars A tsibble of vars of time series.
#'
#' @describeIn ts_missing_tidyverts  Server function of ts_missing_tidyverts.
#' @return * Server function return a tsibble of time series after processing
#'  values.
ts_missing_tidyverts_server <- function(id, tsbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(tsbl_vars))

    ## Logical reactive ----

    # Stock time series
    tsbl_vars_stock_raw <- reactive({
      tsbl_vars_stock_raw <- tsbl_vars()
      if ("period" %in% names(tsbl_vars_stock_raw)) {
        tsbl_vars_stock_raw <- tsbl_vars_stock_raw%>%
          periodize_index(period_field = "period")
      }
      tsbl_vars_stock_raw
    })

    # Industry time series
    tsbl_vars_industry_raw <- reactive({
      tsbl_vars_stock_raw() %>%
        industry_median()
    })

    # Stock time series after filling gaps
    tsbl_vars_stock_fill_gaps <- reactive({
      tsbl_vars_stock_raw() %>%
        tsibble::group_by_key() %>%
        tsibble::fill_gaps(
          .full = !!rlang::parse_expr(req(input$full_gap))
        )
    })

    # Industry Time series after filling gaps
    tsbl_vars_industry_fill_gaps <- reactive({
      tsbl_vars_industry_raw() %>%
        tsibble::group_by_key()
      tsibble::fill_gaps(
        .full = !!rlang::parse_expr(req(input$full_gap))
      )
    })

    # Stock time series after imputing NAs
    tsbl_vars_stock_impute_nas <- reactive({
      key_vars <- tsibble::key_vars(tsbl_vars_stock_raw())
      tsbl_vars_stock_fill_gaps() %>%
        tidyr::fill(tidyselect::everything(), .direction = "down") %>%
        dplyr::ungroup() %>%
        tsibble::as_tsibble(key = {{ key_vars }})
    })

    # Industry time series of after imputing NAs
    tsbl_vars_industry_impute_nas <- reactive({
      key_vars <- tsibble::key_vars(tsbl_vars_industry_raw())

      tsbl_vars_industry_fill_gaps() %>%
        tidyr::fill(tidyselect::everything(), .direction = "down") %>%
        dplyr::ungroup() %>%
        tsibble::as_tsibble(key = {{ key_vars }})
    })


    # Stock time series after processing
    tidy_tsbl_vars <- reactive({
      switch(input$output_type,
        "Fill gaps + impute nas" = {
          tsbl_vars_stock_impute_nas()
        },
        "Only fill gaps" = {
          tsbl_vars_stock_fill_gaps()
        },
        "Origin" = {
          tsbl_vars_stock_raw
        }
      )
    })

    ## Output of missing information ----
    output$original_nas_plot <- renderPlot({
      switch(input$ts_type,
        "stock" = {
          tsbl_vars_stock_raw() %>%
            tibble::as_tibble() %>%
            visdat::vis_dat() +
            ggplot2::labs(
              title = "Original missing values",
              subtitle = "Focused stocks"
            )
        },
        "industry" = {
          tsbl_vars_industry_raw() %>%
            tibble::as_tibble() %>%
            visdat::vis_dat() +
            ggplot2::labs(
              title = "Original missing values",
              subtitle = "Focused industries"
            )
        }
      )
    })

    output$fill_gaps_plot <- renderPlot({
      switch(input$ts_type,
        "stock" = {
          tsbl_vars_stock_fill_gaps() %>%
            tibble::as_tibble() %>%
            visdat::vis_dat() +
            ggplot2::labs(
              title = "Missing values after filling gaps with NAs",
              subtitle = "Focused stocks"
            )
        },
        "industry" = {
          tsbl_vars_industry_fill_gaps() %>%
            tibble::as_tibble() %>%
            visdat::vis_dat() +
            ggplot2::labs(
              title = "Missing values after filling gaps with NAs",
              subtitle = "Focused industries"
            )
        }
      )
    })

    output$impute_nas_plot <- renderPlot({
      switch(input$ts_type,
        "stock" = {
          tsbl_vars_stock_impute_nas() %>%
            tibble::as_tibble() %>%
            visdat::vis_dat() +
            ggplot2::labs(
              title = "Missing values after imputing NAs",
              subtitle = "Focused stocks"
            )
        },
        "industry" = {
          tsbl_vars_industry_impute_nas() %>%
            tibble::as_tibble() %>%
            visdat::vis_dat() +
            ggplot2::labs(
              title = "Missing values after imputing NAs",
              subtitle = "Focused industries"
            )
        }
      )
    })

    return(tidy_tsbl_vars)
  })
}

#' Testing module app of ts_missing_tidyverts
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn ts_missing_tidyverts  Testing App of ts_missing_tidyverts.
ts_missing_tidyverts_app <- function(use_online_data = FALSE) {

  # Prepare data
  tsbl_vars <- load_tsbl_vars(use_online_data)

  # Only show some stocks for demonstration
  focus_stocks <- c(
    "000651", "000333", "600066",
    "000550", "600031", "000157"
  )
  tsbl_vars <- tsbl_vars %>%
    dplyr::filter(stkcd %in% focus_stocks)

  ui <- fluidPage(
    ts_missing_tidyverts_ui("ts_missing_tidyverts_module")
  )
  server <- function(input, output, session) {
    ts_missing_tidyverts <- ts_missing_tidyverts_server(
      "ts_missing_tidyverts_module",
      tsbl_vars = reactive(tsbl_vars)
    )
  }
  shinyApp(ui, server)
}
