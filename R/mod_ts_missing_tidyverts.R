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
#'     tsbl_vars = reactive(tsbl_vars),
#'     tsbl_vars_average = reactive(tsbl_vars_average)
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
          choices = c("Origin", "Fill gaps", "Fill gaps + impute nas"),
          selected = "Origin"
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("missing_value"),
          type = "tabs",
          tabPanel(
            "Original NAs in ts",
            value = "missing_value_in_origin_data",
            plotOutput(ns("original_nas_plot"), height = "600px")
          ),
          tabPanel(
            "Fill gaps with NAs",
            value = "missing_value_after_filling_gaps",
            plotOutput(ns("fill_gaps_plot"), height = "600px")
          ),
          tabPanel(
            "Imputing NAs",
            value = "missing_value_after_imputing_nas",
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
#' @param tsbl_vars_average A tsibble of average of vars of time series.
#'
#' @describeIn ts_missing_tidyverts  Server function of ts_missing_tidyverts.
#' @return * Server function return return a list of tsibbles of time series
#'     after processing missing values, which contains following fields:
#'
#'    + tidy_tsbl_vars: a tsibble of variables after processing missing value.
#'
#'    + tidy_tsbl_vars_average: a tsibble of average of variables after
#'      processing missing value.
#'
ts_missing_tidyverts_server <- function(id, tsbl_vars, tsbl_vars_average) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(tsbl_vars))
    assertive::assert_all_are_true(is.reactive(tsbl_vars_average))

    # Logical reactive ----

    # Stock time series
    tsbl_vars_stock_raw <- reactive({
      tsbl_vars_stock_raw <- tsbl_vars()
      if ("period" %in% names(tsbl_vars_stock_raw)) {
        tsbl_vars_stock_raw <- tsbl_vars_stock_raw %>%
          periodize_index(period_field = "period")
      }
      tsbl_vars_stock_raw
    })

    # Industry time series
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


    # Stock time series after filling gaps
    tsbl_vars_stock_fill_gaps <- reactive({
      tsbl_vars_stock_raw() %>%
        tsibble::group_by_key() %>%
        tsibble::fill_gaps(
          .full = !!rlang::parse_expr(req(input$full_gap))
        ) %>%
        dplyr::ungroup()
    })

    # Industry Time series after filling gaps
    tsbl_vars_industry_fill_gaps <- reactive({
      tsbl_vars_industry_raw() %>%
        tsibble::group_by_key() %>%
        tsibble::fill_gaps(
          .full = !!rlang::parse_expr(req(input$full_gap))
        )%>%
        dplyr::ungroup()
    })

    # Stock time series after imputing NAs
    tsbl_vars_stock_impute_nas <- reactive({
      tsbl_vars_stock_fill_gaps() %>%
        tsibble::group_by_key() %>%
        #tidyr::fill(tidyselect::everything(), .direction = "down") %>%
        tidyr::fill(where(~is.numeric(.)), .direction = "down") %>%
        tidyr::fill(where(~!is.numeric(.)), .direction = "updown") %>%
        dplyr::ungroup()
    })

    # Industry time series of after imputing NAs
    tsbl_vars_industry_impute_nas <- reactive({
      tsbl_vars_industry_fill_gaps() %>%
        tsibble::group_by_key() %>%
        #tidyr::fill(tidyselect::everything(), .direction = "down") %>%
        tidyr::fill(where(~is.numeric(.)), .direction = "down") %>%
        tidyr::fill(where(~!is.numeric(.)), .direction = "updown") %>%
        dplyr::ungroup()
    })


    # Stock time series after processing
    tidy_tsbl_vars <- reactive({
      switch(input$output_type,
        "Fill gaps + impute nas" = {
          tsbl_vars_stock_impute_nas()
        },
        "Fill gaps" = {
          tsbl_vars_stock_fill_gaps()
        },
        "Origin" = {
          tsbl_vars_stock_raw()
        }
      )
    })

    # Industry time series after processing
    tidy_tsbl_vars_average <- reactive({
      switch(input$output_type,
        "Fill gaps + impute nas" = {
          tsbl_vars_industry_impute_nas()
        },
        "Fill gaps" = {
          tsbl_vars_industry_fill_gaps()
        },
        "Origin" = {
          tsbl_vars_industry_raw()
        }
      )
    })

    # Controls interaction ----
    observeEvent(input$output_type, ignoreInit = FALSE, {

      switch(input$output_type,
        "Fill gaps + impute nas" = {
          updateTabsetPanel(session,
            inputId = "missing_value",
            selected = "missing_value_after_imputing_nas"
          )
        },
        "Fill gaps" = {
          updateTabsetPanel(session,
            inputId = "missing_value",
            selected = "missing_value_after_filling_gaps"
          )
        },
        "Origin" = {
          updateTabsetPanel(session,
            inputId = "missing_value",
            selected = "missing_value_in_origin_data"
          )
        }
      )
    })


    # Output of missing information ----
    output$original_nas_plot <- renderPlot({

      # Data setting for plot by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- tsbl_vars_stock_raw()
          sub_tile <- "Stocks series"
        },
        "industry" = {
          tsbl_focus <- tsbl_vars_industry_raw()
          sub_tile <- "Industris series"
        }
      )

      # Plot result
      tsbl_focus %>%
        tibble::as_tibble() %>%
        visdat::vis_dat() +
        ggplot2::labs(
          title = "Missing values in data",
          subtitle = sub_tile
        )
    })

    output$fill_gaps_plot <- renderPlot({

      # Data setting for plot by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- tsbl_vars_stock_fill_gaps()
          sub_tile <- "Stocks series"
        },
        "industry" = {
          tsbl_focus <- tsbl_vars_industry_fill_gaps()
          sub_tile <- "Industris series"
        }
      )

      # Plot result
      tsbl_focus %>%
        tibble::as_tibble() %>%
        visdat::vis_dat() +
        ggplot2::labs(
          title = "Missing values in data",
          subtitle = sub_tile
        )
    })

    output$impute_nas_plot <- renderPlot({

      # Data setting for plot by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- tsbl_vars_stock_impute_nas()
          sub_tile <- "Stocks series"
        },
        "industry" = {
          tsbl_focus <- tsbl_vars_industry_impute_nas()
          sub_tile <- "Industris series"
        }
      )

      # Plot result
      tsbl_focus %>%
        tibble::as_tibble() %>%
        visdat::vis_dat() +
        ggplot2::labs(
          title = "Missing values in data",
          subtitle = sub_tile
        )
    })



    # Result returned by Server function
    tidy_result <- list(
      tidy_tsbl_vars = tidy_tsbl_vars,
      tidy_tsbl_vars_average = tidy_tsbl_vars_average
    )
    return(tidy_result)
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
  tsbl_vars_average <- industry_mean(tsbl_vars)

  # Only show some stocks for demonstration
  focus_stocks <- c(
    "000651", "000333", "600066",
    "000550", "600031", "000157"
  )
  tsbl_vars <- tsbl_vars %>%
    dplyr::filter(stkcd %in% focus_stocks)

  focus_industries <- unique(tsbl_vars$indcd)

  tsbl_vars_average <- tsbl_vars_average %>%
    dplyr::filter(.data$indcd %in% focus_industries)

  ui <- fluidPage(
    ts_missing_tidyverts_ui("ts_missing_tidyverts_module")
  )
  server <- function(input, output, session) {
    ts_missing_tidyverts <- ts_missing_tidyverts_server(
      "ts_missing_tidyverts_module",
      tsbl_vars = reactive(tsbl_vars),
      tsbl_vars_average = reactive(tsbl_vars_average)
    )
  }
  shinyApp(ui, server)
}
