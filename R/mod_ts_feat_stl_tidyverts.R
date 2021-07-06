#' ts_feat_stl_tidyverts
#'
#' @description A shiny module for ts_feat_stl_tidyverts.
#'
#' @details
#'  The module is an UI for user to display STL features of time series
#'  tidyverts family packages.
#'
#' @name ts_feat_stl_tidyverts
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   ts_feat_stl_tidyverts_ui("ts_feat_stl_tidyverts_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   ts_feat_stl_tidyverts <- ts_feat_stl_tidyverts_server(
#'     "ts_feat_stl_tidyverts_module",
#'     tsbl_vars = reactive(tsbl_vars),
#'     tsbl_vars_average = reactive(tsbl_vars_average)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' ts_feat_stl_tidyverts_app()
#' }
#'
NULL

#' UI function of ts_feat_stl_tidyverts
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn ts_feat_stl_tidyverts  UI function of ts_feat_stl_tidyverts.
#' @importFrom shiny NS tagList
ts_feat_stl_tidyverts_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # h1("ts_feat_stl_tidyverts"),
    sidebarLayout(
      position = "right",
      sidebarPanel(
        width = 3,
        fluidRow(
          column(
            width = 6,
            actionButton(
              inputId = ns("clear_focus"),
              label = strong("Clear selection"),
              width = "100%"
            ),
          ),
          column(
            width = 6,
            actionButton(
              inputId = ns("show_focus"),
              label = strong("Show selection"),
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
            "STL Decomposition",

            DT::dataTableOutput(ns("stl_stats_table")),

            fluidRow(
              box(
                title = "Components Plot", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE, width = 6,
                plotOutput(ns("stl_component_plot"), height = "300px")
              ),
              box(
                title = "Season adjust Plot", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE, width = 6,
                plotOutput(ns("stl_season_adjust_plot"), height = "300px")
              )
            )
          )
        )
      )
    )
  )
}

#' Server function of ts_feat_stl_tidyverts
#'
#' @param tsbl_vars A tibble of vars of time series.
#'
#' @param tsbl_vars_average A tsibble of average of vars of time series.
#'
#' @describeIn ts_feat_stl_tidyverts  Server function of ts_feat_stl_tidyverts.
#' @return * Server function return a data frame of ...
ts_feat_stl_tidyverts_server <- function(id, tsbl_vars, tsbl_vars_average) {
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

      tsbl_vars_stock_raw <- tsbl_vars_stock_raw %>%
        tidy_tsbl(fill_gaps = "individual", fill_nas = "down")

      tsbl_vars_stock_raw
    })

    tsbl_vars_industry_raw <- reactive({
      tsbl_vars_industry_raw <- tsbl_vars_average()
      if ("period" %in% names(tsbl_vars_industry_raw)) {
        tsbl_vars_industry_raw <- tsbl_vars_industry_raw %>%
          periodize_index(period_field = "period")
      }

      tsbl_vars_industry_raw <- tsbl_vars_industry_raw %>%
        tidy_tsbl(fill_gaps = "individual", fill_nas = "down")

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

    tsbl_stock <- reactive({
      tsbl_vars_stock_raw() %>%
        dplyr::select(
          c(
            tsibble::index_var(.), tsibble::key_vars(.),
            industry_id_var()
          ),
          tidyselect::all_of(focus_var())
        )
    })

    long_tsbl_stock <- reactive({
      tsbl_stock() %>%
        tidyr::pivot_longer(
          cols = -c(
            tsibble::index_var(.), tsibble::key_vars(.),
            industry_id_var()
          ),
          names_to = "variable", values_to = "value"
        )
    })

    tsbl_industry <- reactive({
      tsbl_vars_industry_raw() %>%
        dplyr::select(
          c(tsibble::index_var(.), tsibble::key_vars(.)),
          tidyselect::all_of(focus_var())
        )
    })

    long_tsbl_industry <- reactive({
      tsbl_industry() %>%
        tidyr::pivot_longer(
          cols = -c(tsibble::index_var(.), tsibble::key_vars(.)),
          names_to = "variable", values_to = "value"
        )
    })

    long_tsbl_stock_industry <- reactive({
      long_tsbl_stock() %>%
        dplyr::left_join(
          long_tsbl_industry(),
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

    # Feature of time series

    # Compute feature statistics by feature function
    compute_feats_stats <- function(feats_fun) {
      assertive::is_function(feats_fun)

      # Data setting for computing by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_data <- long_tsbl_stock()
          id_var <- stock_id_var()
          id_name <- "stkname"
        },
        "industry" = {
          tsbl_data <- long_tsbl_industry()
          id_var <- industry_id_var()
          id_name <- "indname"
        }
      )

      # Compute result
      tsbl_data %>%
        dplyr::group_by(.data[[id_var]], .data$variable) %>%
        fabletools::features(
          .var = .data$value,
          features = feats_fun
        ) %>%
        dplyr::arrange(.data$variable, .data[[id_var]]) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(id_name := ifelse(!purrr::is_empty(.data[[id_var]]),
          code2name(.data[[id_var]]), character(0)
        )) %>%
        dplyr::select(
          c("variable", id_var, id_name),
          tidyselect::everything()
        )
    }

    feat_stl_stats <- reactive({
      compute_feats_stats(feasts::feat_stl)
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

    # Handler to clear selection in DT table
    clear_dt_select <- function(DT_tableId) {

      # Clear selection in table
      proxy <- DT::dataTableProxy(DT_tableId)
      DT::selectRows(proxy, selected = NULL)

      shinyjs::click(id = DT_tableId)
    }

    # Click to clear focus inputs for stock and industry
    observeEvent(input$clear_focus, ignoreInit = TRUE, {
      # Clear focus inputs according to features
      switch(input$ts_feature,
        "STL Decomposition" = {
          clear_dt_select("stl_stats_table")
        }
      )
    })

    # Handler to show selected data of in DT table
    show_foucs_data <- function(tsbl_focus) {
      if (NROW(tsbl_focus) > 0) {

        # Define data Modal to show
        data_modal <- modalDialog(
          title = "Original data of selection ",
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Original Data table",
              # Show focus data in table
              DT::renderDataTable({
                tbl_focus <- tsbl_focus %>%
                  tibble::as_tibble() %>%
                  dplyr::mutate(date = as.character(date))

                numeric_vars <- zstmodelr::expect_type_fields(
                  tbl_focus,
                  expect_type = "numeric"
                )

                tbl_focus %>%
                  DT::datatable(
                    filter = "top",
                    extensions = "Scroller",
                    options = list(
                      columnDefs = list(list(className = "dt-center")),
                      pageLength = 10,
                      dom = "ltir",
                      deferRender = TRUE,
                      scrollY = 250,
                      scrollX = TRUE,
                      scroller = TRUE
                    )
                  ) %>%
                  DT::formatRound(columns = numeric_vars, digits = 3)
              }),
            ),
            tabPanel(
              title = "Original Data plot",
              # Show focus data in plot
              plotly::renderPlotly({
                tsbl_focus %>%
                  fabletools::autoplot(.data$value_stock) +
                  fabletools::autolayer(tsbl_focus,
                    .data$value_industry,
                    # color = "blue",
                    alpha = 0.5,
                    linetype = "dotted"
                  )
              }),
            )
          ),

          size = "l",
          easyClose = TRUE
        )

        # Show data Modal
        showModal(data_modal)
      }
    }

    # Click to show data of focus inputs for stock and industry
    observeEvent(input$show_focus, ignoreInit = TRUE, {

      # Data setting for showing data by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- long_tsbl_stock_industry()
          id_var <- stock_id_var()
        },
        "industry" = {
          tsbl_focus <- long_tsbl_industry()
          id_var <- industry_id_var()
        }
      )



      # Filter data by user selection
      focus_key <- switch(input$ts_feature,
        "STL Decomposition" = {
          stl_focus_key()
        }
      )
      if (!is.null(focus_key)) {
        tsbl_focus <- tsbl_focus %>%
          dplyr::filter(
            .data[[id_var]] %in% focus_key[[id_var]],
            .data[["variable"]] %in% focus_key[["variable"]]
          )
      }

      # Show original data of focus
      show_foucs_data(tsbl_focus)
    })

    # Handler for user to select key codes from table
    get_dt_select_keys <- function(DT_tableId, ds_info, key_vars) {
      select_index <- input[[glue::glue("{DT_tableId}_rows_selected")]]
      select_keys <- ds_info[select_index, key_vars]

      select_keys
    }

    # Get key of user selection in stl_stats_table
    stl_focus_key <- reactive({
      if (length(input$stl_stats_table_cell_clicked) > 0) {
        switch(input$ts_type,
          "stock" = {
            get_dt_select_keys(
              DT_tableId = "stl_stats_table",
              ds_info = feat_stl_stats(),
              key_var = c(stock_id_var(), "variable")
            )
          },
          "industry" = {
            get_dt_select_keys(
              DT_tableId = "stl_stats_table",
              ds_info = feat_stl_stats(),
              key_var = c(industry_id_var(), "variable")
            )
          }
        )
      }
    })


    ## Output of features ----

    # >> STL output ----

    output$stl_stats_table <- DT::renderDataTable({
      numeric_vars <- zstmodelr::expect_type_fields(
        feat_stl_stats(),
        expect_type = "numeric"
      )

      feat_stl_stats() %>%
        DT::datatable(
          filter = "top",
          extensions = "Scroller",
          options = list(
            columnDefs = list(list(className = "dt-center")),
            pageLength = 10,
            dom = "ltir",
            deferRender = TRUE,
            scrollY = 250,
            scrollX = TRUE,
            scroller = TRUE
          )
        ) %>%
        DT::formatRound(columns = numeric_vars, digits = 3)
    })

    # output$stl_component_plot <- plotly::renderPlotly({
    output$stl_component_plot <- renderPlot({

      # Prompt user to choose at least one series for plot
      validate(
        need(NROW(stl_focus_key()) > 0,
          message = "Please choose at least one series in above table to show plots."
        )
      )

      # Data setting for plot by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- long_tsbl_stock()
          id_var <- stock_id_var()
        },
        "industry" = {
          tsbl_focus <- long_tsbl_industry()
          id_var <- industry_id_var()
        }
      )

      # Filter data by user selection
      focus_key <- stl_focus_key()
      if (!is.null(focus_key)) {
        tsbl_focus <- tsbl_focus %>%
          dplyr::filter(
            .data[[id_var]] %in% focus_key[[id_var]],
            .data[["variable"]] %in% focus_key[["variable"]]
          )
      }

      # Plot STL components plot
      if (NROW(tsbl_focus) > 0) {
        p <- tsbl_focus %>%
          fabletools::model(model = feasts::STL(value)) %>%
          fabletools::components() %>%
          fabletools::autoplot() +
          ggplot2::theme(legend.position = "bottom")
      } else {
        p <- NULL
      }

      p
    })

    # output$stl_season_adjust_plot <- plotly::renderPlotly({
    output$stl_season_adjust_plot <- renderPlot({
      # Prompt user to choose at least one series for plot
      validate(
        need(NROW(stl_focus_key()) > 0,
          message = "Please choose at least one series in above table to show plots."
        )
      )

      # Data setting for plot by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- long_tsbl_stock()
          id_var <- stock_id_var()
        },
        "industry" = {
          tsbl_focus <- long_tsbl_industry()
          id_var <- industry_id_var()
        }
      )

      # Filter data by user selection
      focus_key <- stl_focus_key()
      if (!is.null(focus_key)) {
        tsbl_focus <- tsbl_focus %>%
          dplyr::filter(
            .data[[id_var]] %in% focus_key[[id_var]],
            .data[["variable"]] %in% focus_key[["variable"]]
          )
      }

      # Plot STL season adjust
      facet_fomula <- rlang::parse_expr(glue::glue("{id_var} ~ variable"))
      if (NROW(tsbl_focus) > 0) {
        p <- tsbl_focus %>%
          fabletools::model(model = feasts::STL(value)) %>%
          fabletools::components() %>%
          ggplot2::ggplot(aes(x = .data$date)) +
          ggplot2::geom_line(
            aes(y = .data$value, color = "origin_value"),
            size = 1.5,
            alpha = 0.5,
          ) +
          ggplot2::geom_line(
            aes(y = .data$season_adjust, color = "season_adjust"),
            size = 1.5,
            linetype = "dashed"
          ) +
          ggplot2::theme(legend.position = "bottom") +
          ggplot2::facet_grid({{ facet_fomula }}) +
          ggplot2::theme_light()
      } else {
        p <- NULL
      }

      p
    })
  })
}

#' Testing module app of ts_feat_stl_tidyverts
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn ts_feat_stl_tidyverts  Testing App of ts_feat_stl_tidyverts.
ts_feat_stl_tidyverts_app <- function(use_online_data = FALSE) {

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
    ts_feat_stl_tidyverts_ui("ts_feat_stl_tidyverts_module")
  )
  server <- function(input, output, session) {
    ts_feat_stl_tidyverts_server(
      "ts_feat_stl_tidyverts_module",
      tsbl_vars = reactive(tsbl_vars),
      tsbl_vars_average = reactive(tsbl_vars_average)
    )
  }
  shinyApp(ui, server)
}
