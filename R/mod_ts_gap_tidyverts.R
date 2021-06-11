#' ts_gap_tidyverts
#'
#' @description A shiny module for ts_gap_tidyverts.
#'
#' @details
#'  The module is an UI for user to explore gaps in time series by
#'  tidyverts family packages.
#'
#' @name ts_gap_tidyverts
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   ts_gap_tidyverts_ui("ts_gap_tidyverts_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   ts_gap_tidyverts <- ts_gap_tidyverts_server(
#'     "ts_gap_tidyverts_module",
#'     tsbl_vars = reactive(tsbl_vars),
#'     tsbl_vars_average = reactive(tsbl_vars_average)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' ts_gap_tidyverts_app()
#' }
#'
NULL

#' UI function of ts_gap_tidyverts
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn ts_gap_tidyverts  UI function of ts_gap_tidyverts.
#' @importFrom shiny NS tagList
ts_gap_tidyverts_ui <- function(id) {
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
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("gaps_info"),
          type = "tabs",
          tabPanel(
            "Having gaps",
            DT::dataTableOutput(ns("has_gaps_table"))
          ),
          tabPanel(
            "Scanning gaps",
            DT::dataTableOutput(ns("scan_gaps_table"))
          ),
          tabPanel(
            "Counting gaps",
            DT::dataTableOutput(ns("count_gaps_table"))
          ),
          tabPanel(
            "Ploting gaps",
            plotOutput(ns("gaps_plot"), height = "600px")
          )
        )
      )
    )
  )
}

#' Server function of ts_gap_tidyverts
#'
#' @param tsbl_vars A tsibble of vars of time series.
#'
#' @param tsbl_vars_average A tsibble of average of vars of time series.
#'
#' @describeIn ts_gap_tidyverts  Server function of ts_gap_tidyverts.
#' @return * Server function doesn't return value.
ts_gap_tidyverts_server <- function(id, tsbl_vars, tsbl_vars_average) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(tsbl_vars))
    assertive::assert_all_are_true(is.reactive(tsbl_vars_average))

    ## Logical reactive ----


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

    # Info about gap existence in ts
    tsbl_vars_focus_has_gaps <- reactive({

      # Data setting for computing by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- tsbl_vars_stock_raw()
          id_var <- stock_id_var()
          id_name <- "stkname"
        },
        "industry" = {
          tsbl_focus <- tsbl_vars_industry_raw()
          id_var <- industry_id_var()
          id_name <- "indname"
        }
      )

      # Compute gap info
      tsbl_focus %>%
        tsibble::has_gaps(
          .full = !!rlang::parse_expr(req(input$full_gap))
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(!!id_name := ifelse(!purrr::is_empty(.data[[id_var]]),
          code2name(.data[[id_var]]), character(0)
        )) %>%
        dplyr::select(c(id_var, id_name), tidyselect::everything())
    })

    # Info about gap positions in ts
    tsbl_vars_focus_scan_gaps <- reactive({

      # Data setting for computing by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- tsbl_vars_stock_raw()
          id_var <- stock_id_var()
          id_name <- "stkname"
        },
        "industry" = {
          tsbl_focus <- tsbl_vars_industry_raw()
          id_var <- industry_id_var()
          id_name <- "indname"
        }
      )

      # Compute gap info
      tsbl_focus %>%
        tsibble::scan_gaps(
          .full = !!rlang::parse_expr(req(input$full_gap))
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(!!id_name := ifelse(!purrr::is_empty(.data[[id_var]]),
          code2name(.data[[id_var]]), character(0)
        )) %>%
        dplyr::select(c(id_var, id_name), tidyselect::everything())
    })

    # Info about gap counts in ts
    tsbl_vars_focus_count_gaps <- reactive({

      # Data setting for computing by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- tsbl_vars_stock_raw()
          id_var <- stock_id_var()
          id_name <- "stkname"
        },
        "industry" = {
          tsbl_focus <- tsbl_vars_industry_raw()
          id_var <- industry_id_var()
          id_name <- "indname"
        }
      )

      # Compute gap info
      tsbl_focus %>%
        tsibble::count_gaps(
          .full = !!rlang::parse_expr(req(input$full_gap))
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(!!id_name := ifelse(!purrr::is_empty(.data[[id_var]]),
          code2name(.data[[id_var]]), character(0)
        )) %>%
        dplyr::select(c(id_var, id_name), tidyselect::everything())
    })


    ## Output of gaps information ----
    output$has_gaps_table <- DT::renderDataTable({
      tsbl_vars_focus_has_gaps() %>%
        DT::datatable(
          filter = "top",
          extensions = "Scroller",
          options = list(
            columnDefs = list(list(className = "dt-center")),
            pageLength = 10,
            dom = "ltir",
            deferRender = TRUE,
            scrollY = 370,
            scrollX = TRUE,
            scroller = TRUE
          )
        )
    })

    output$scan_gaps_table <- DT::renderDataTable({
      tsbl_vars_focus_scan_gaps() %>%
        dplyr::mutate(
          date = format(date)
        ) %>%
        DT::datatable(
          filter = "top",
          extensions = "Scroller",
          options = list(
            columnDefs = list(list(className = "dt-center")),
            pageLength = 10,
            dom = "ltir",
            deferRender = TRUE,
            scrollY = 370,
            scrollX = TRUE,
            scroller = TRUE
          )
        )
    })

    output$count_gaps_table <- DT::renderDataTable({
      tsbl_vars_focus_count_gaps() %>%
        dplyr::mutate(
          .from = format(.data$.from),
          .to = format(.data$.to)
        ) %>%
        DT::datatable(
          filter = "top",
          extensions = "Scroller",
          options = list(
            columnDefs = list(list(className = "dt-center")),
            pageLength = 10,
            dom = "ltir",
            deferRender = TRUE,
            scrollY = 370,
            scrollX = TRUE,
            scroller = TRUE
          )
        )
    })

    output$gaps_plot <- renderPlot({
      p <- switch(input$ts_type,
        "stock" = {
          tsbl_vars_focus_count_gaps() %>%
            ggplot2::ggplot(ggplot2::aes(x = .data$stkcd, colour = .data$stkcd))
        },
        "industry" = {
          tsbl_vars_focus_count_gaps() %>%
            ggplot2::ggplot(ggplot2::aes(x = .data$indcd, colour = .data$indcd))
        }
      )

      p + ggplot2::geom_linerange(ggplot2::aes(ymin = .data$.from, ymax = .data$.to)) +
        ggplot2::geom_point(ggplot2::aes(y = .data$.from)) +
        ggplot2::geom_point(ggplot2::aes(y = .data$.to)) +
        ggplot2::coord_flip() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(title = "Gaps(Implict Missingnes) in time series")
    })
  })
}

#' Testing module app of ts_gap_tidyverts
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn ts_gap_tidyverts  Testing App of ts_gap_tidyverts.
ts_gap_tidyverts_app <- function(use_online_data = FALSE) {

  # Prepare data
  tsbl_vars <- load_tsbl_vars(use_online_data)
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
    ts_gap_tidyverts_ui("ts_gap_tidyverts_module")
  )
  server <- function(input, output, session) {
    ts_gap_tidyverts_server(
      "ts_gap_tidyverts_module",
      tsbl_vars = reactive(tsbl_vars),
      tsbl_vars_average = reactive(tsbl_vars_average)
    )
  }
  shinyApp(ui, server)
}
