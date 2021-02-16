#' ts_feat_basic_tidyverts
#'
#' @description A shiny module for ts_feat_basic_tidyverts.
#'
#' @details
#'  The module is an UI for user to display simple features of time series
#'  tidyverts family packages.
#'
#' @name ts_feat_basic_tidyverts
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   ts_feat_basic_tidyverts_ui("ts_feat_basic_tidyverts_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   ts_feat_basic_tidyverts <- ts_feat_basic_tidyverts_server(
#'     "ts_feat_basic_tidyverts_module",
#'     tsbl_vars = reactive(tsbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' ts_feat_basic_tidyverts_app()
#' }
#'
NULL

#' UI function of ts_feat_basic_tidyverts
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn ts_feat_basic_tidyverts  UI function of ts_feat_basic_tidyverts.
#' @importFrom shiny NS tagList
ts_feat_basic_tidyverts_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
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
            label = strong("Time-series Type:"),
            choices = c("stock", "industry")
          ),
          selectInput(
            inputId = ns("focus_var"),
            label = strong("Focus variable:"),
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
            "Simple stats",
            DT::dataTableOutput(ns("simple_stats_table")),
            plotOutput(ns("simple_stats_plot"), height = "300px")
          ),
          tabPanel(
            "basic plot",
            # plotOutput(ns("timeseries_plot"), height = "600px")
            plotly::plotlyOutput(ns("timeseries_plot"), height = "600px")
          )
        )
      )
    )
  )
}

#' Server function of ts_feat_basic_tidyverts
#'
#' @param tsbl_vars A tibble of vars of time series.
#'
#' @describeIn ts_feat_basic_tidyverts  Server function of ts_feat_basic_tidyverts.
#' @return * Server function return a data frame of ...
ts_feat_basic_tidyverts_server <- function(id, tsbl_vars) {
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

    tsbl_focus_stock <- eventReactive(input$update_output, {
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

    tsbl_focus_industry <- eventReactive(input$update_output, {
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

    # Feature of time series

    feats_simple_stats <- reactive({
      switch(input$ts_type,
        "stock" = {
          long_tsbl_focus_stock() %>%
            dplyr::group_by(.data$stkcd, .data$variable) %>%
            fabletools::features(
              .var = .data$value,
              features = list(
                mean = ~ mean(., na.rm = TRUE),
                median = ~ median(., na.rm = TRUE),
                sd = ~ sd(., na.rm = TRUE),
                Q = ~ quantile(., na.rm = TRUE)
              )
            ) %>%
            dplyr::arrange(.data$variable, dplyr::desc(.data$median)) %>%
            dplyr::rowwise() %>%
            dplyr::mutate(stkname = ifelse(!purrr::is_empty(stkcd),
              zstexplorer::code2name(stkcd), character(0)
            )) %>%
            dplyr::select(c("variable", "stkcd", "stkname"), tidyselect::everything())
        },
        "industry" = {
          long_tsbl_focus_industry() %>%
            dplyr::group_by(.data$indcd, .data$variable) %>%
            fabletools::features(
              .var = .data$value,
              features = list(
                mean = ~ mean(., na.rm = TRUE),
                median = ~ median(., na.rm = TRUE),
                sd = ~ sd(., na.rm = TRUE),
                Q = ~ quantile(., na.rm = TRUE)
              )
            ) %>%
            dplyr::arrange(.data$variable, .data$indcd) %>%
            dplyr::rowwise() %>%
            dplyr::mutate(indname = ifelse(!purrr::is_empty(indcd),
              zstexplorer::code2name(indcd), character(0)
            )) %>%
            dplyr::select(c("variable", "indcd", "indname"), tidyselect::everything())
        }
      )
    })



    # Controls interaction ----

    # Update UI with dataset and user inputs
    observe({
      focus_var_value <- zstmodelr::expect_type_fields(
        tsbl_vars_stock_raw(),
        expect_type = "numeric"
      )

      # Set choices for select inputs
      updateSelectInput(
        session = session, inputId = "focus_var",
        choices = focus_var_value,
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


    # Handler for user to clear focus input for stock and industry
    clear_focus <- function() {

      # Clear focus input
      updateTextInput(
        session = session,
        inputId = "focus_stkcd",
        value = ""
      )

      updateTextInput(
        session = session,
        inputId = "focus_indcd",
        value = ""
      )

      # Clear selections in tab;e
      dt_table <- "simple_stats_table"
      proxy <- DT::dataTableProxy(dt_table)
      DT::selectRows(proxy, selected = NULL)

      # Refresh output
      shinyjs::click(id = "update_output")
    }

    # Click to clear focus inputs for stock and industry
    observeEvent(input$clear_focus, ignoreInit = TRUE, {
      clear_focus()
    })

    # Handler for user to select stock code from table
    user_select_codes <- function(DT_tableId, ds_info, id_var) {
      select_index <- input[[glue::glue("{DT_tableId}_rows_selected")]]
      select_stock <- dplyr::pull(ds_info[select_index, ], {{ id_var }})
      if (length(select_stock) > 0) {
        updateTextInput(
          session = session,
          inputId = "focus_stkcd",
          value = select_stock
        )
      }
    }

    # Click to select focus stock from table
    observeEvent(input$simple_stats_table_cell_clicked, ignoreInit = TRUE, {
      if (length(input$simple_stats_table_cell_clicked) > 0) {
        user_select_codes(
          DT_tableId = "simple_stats_table",
          ds_info = feats_simple_stats(),
          id_var = "stkcd"
        )
      }
    })

    ## Output of features ----

    output$simple_stats_table <- DT::renderDataTable({
      numeric_vars <- zstmodelr::expect_type_fields(
        feats_simple_stats(),
        expect_type = "numeric"
      )

      feats_simple_stats() %>%
        DT::datatable(
          filter = "top",
          extensions = "Scroller",
          options = list(
            columnDefs = list(list(className = "dt-center")),
            pageLength = 10,
            dom = "ltir",
            deferRender = TRUE,
            scrollY = 270,
            scrollX = TRUE,
            scroller = TRUE
          )
        ) %>%
        DT::formatRound(columns = numeric_vars, digits = 3)
    })

    output$simple_stats_plot <- renderPlot({
      p <- switch(input$ts_type,
        "stock" = {
          long_tsbl_focus() %>%
            ggplot2::ggplot() +
            ggplot2::geom_boxplot(ggplot2::aes(
              x = .data$stkcd,
              y = .data$value_stock,
              color = .data$indcd,
            )) +
            ggplot2::geom_boxplot(ggplot2::aes(
              x = .data$indcd,
              y = .data$value_industry,
              color = .data$indcd,
              fill = .data$indcd
            ),
            alpha = 0.5
            )
        },
        "industry" = {
          long_tsbl_focus_industry() %>%
            ggplot2::ggplot() +
            ggplot2::geom_boxplot(ggplot2::aes(
              x = .data$indcd,
              y = .data$value,
              color = .data$indcd,
              fill = .data$indcd
            ),
            alpha = 0.5
            )
        }
      )

      p + ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_light()
    })

    # output$timeseries_plot <- renderPlot({
    output$timeseries_plot <- plotly::renderPlotly({
      p <- switch(input$ts_type,
        "stock" = {
          long_tsbl_focus() %>%
            fabletools::autoplot(.data$value_stock) +
            fabletools::autolayer(long_tsbl_focus(),
              .data$value_industry,
              # color = "blue",
              alpha = 0.5,
              linetype = "dotted"
            )
        },
        "industry" = {
          long_tsbl_focus_industry() %>%
            fabletools::autoplot(.data$value)
        }
      )

      p <- p + ggplot2::labs(y = NULL) +
        ggplot2::theme_light() +
        ggplot2::theme(legend.position = "bottom")

      p <- plotly::ggplotly(p, tooltip = c("x", "y", "colour"))

      p
    })
  })
}

#' Testing module app of ts_feat_basic_tidyverts
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn ts_feat_basic_tidyverts  Testing App of ts_feat_basic_tidyverts.
ts_feat_basic_tidyverts_app <- function(use_online_data = FALSE) {

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
    ts_feat_basic_tidyverts_ui("ts_feat_basic_tidyverts_module")
  )
  server <- function(input, output, session) {
    ts_feat_basic_tidyverts_server(
      "ts_feat_basic_tidyverts_module",
      tsbl_vars = reactive(tsbl_vars)
    )
  }

  shinyApp(ui, server)
}
