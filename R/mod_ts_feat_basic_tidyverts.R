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
#'     tsbl_vars = reactive(tsbl_vars),
#'     tsbl_vars_average = reactive(tsbl_vars_average)
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
        ),
        wellPanel(
          tabsetPanel(
            id = ns("setting_tabs"),
            type = "hidden",
            tabPanelBody(
              value = "general_stats_setting",
            ),
            tabPanelBody(
              value = "trend_stats_setting",
              checkboxInput(
                inputId = ns("trend_show_legend"),
                label = "Show legend", value = TRUE
              ),
            ),
            tabPanelBody(
              value = "distribution_setting",
            )
          )
        ),
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("ts_feature"),
          type = "tabs",
          tabPanel(
            "General stats",
            DT::dataTableOutput(ns("general_stats_table")),
            plotOutput(ns("general_stats_plot"), height = "300px")
          ),
          tabPanel(
            "Trend stats",
            DT::dataTableOutput(ns("trend_stats_table")),

            tabBox(
              width = 12, height = "300px", side = "left",
              tabPanel(
                "Long-term trendency",
                plotly::plotlyOutput(ns("trend_series_trendency_plot"),
                  height = "300px"
                )
              ),
              tabPanel(
                "Series comparison(A)",
                plotly::plotlyOutput(ns("trend_series_compare_a_plot"),
                  height = "300px"
                )
              ),
              tabPanel(
                "Series comparison(B)",
                plotly::plotlyOutput(ns("trend_series_compare_b_plot"),
                  height = "300px"
                )
              ),
              tabPanel(
                "Growth comparison(Sequential)",
                plotly::plotlyOutput(ns("trend_seq_growth_compare_plot"),
                  height = "300px"
                )
              ),
              tabPanel(
                "Growth comparison(YOY)",
                plotly::plotlyOutput(ns("trend_yoy_growth_compare_plot"),
                  height = "300px"
                )
              ),
              tabPanel(
                "Series & Growth",
                plotly::plotlyOutput(ns("trend_series_growth_plot"),
                  height = "300px"
                )
              )
            ),
          ),
          tabPanel(
            "Distribution",
            DT::dataTableOutput(ns("distribution_stats_table")),
            box(
              title = "Density Plot", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, collapsed = FALSE, width = 6,
              plotOutput(ns("distribution_density_plot"), height = "300px")
            ),
            box(
              title = "QQ Plot", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, collapsed = FALSE, width = 6,
              plotOutput(ns("distribution_qq_plot"), height = "300px")
            )
          )
        )
      )
    )
  )
}

#' Server function of ts_feat_basic_tidyverts
#'
#' @param tsbl_vars A tsibble of vars of time series.
#'
#' @param tsbl_vars_average A tsibble of average of vars of time series.
#'
#' @describeIn ts_feat_basic_tidyverts  Server function of ts_feat_basic_tidyverts.
#' @return * Server function doesn't return value.
ts_feat_basic_tidyverts_server <- function(id, tsbl_vars, tsbl_vars_average) {
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

    tsbl_focus_stock <- reactive({
      tsbl_vars_stock_raw()
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
      tsbl_vars_industry_raw()
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

    # Feature of time series

    # Compute feature statistics by feature function
    compute_feats_stats <- function(feats_fun) {
      assertive::is_function(feats_fun)

      # Data setting for computing by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- long_tsbl_focus_stock()
          id_var <- stock_id_var()
          id_name <- "stkname"
        },
        "industry" = {
          tsbl_focus <- long_tsbl_focus_industry()
          id_var <- industry_id_var()
          id_name <- "indname"
        }
      )

      # Compute result
      tsbl_focus %>%
        dplyr::group_by(.data[[id_var]], .data$variable) %>%
        fabletools::features(
          .var = .data$value,
          features = feats_fun
        ) %>%
        dplyr::arrange(.data$variable, .data[[id_var]]) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(id_name := ifelse(!purrr::is_empty(.data[[id_var]]),
          zstexplorer::code2name(.data[[id_var]]), character(0)
        )) %>%
        dplyr::select(
          c("variable", id_var, id_name),
          tidyselect::everything()
        )
    }


    feats_general_stats <- reactive({

      # Function definition for General stats
      stats_fun <- list(
        Obs = ~ length(.),
        NAs = ~ sum(is.na(.), na.rm = TRUE),
        mean = ~ mean(., na.rm = TRUE),
        median = ~ median(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE),
        Q = ~ quantile(., na.rm = TRUE)
      )

      compute_feats_stats(stats_fun)
    })


    feats_trend_stats <- reactive({
      avg_growth <- function(x, type = c("arithmetic", "geometric")) {
        type <- match.arg(type)
        switch(type,
          "arithmetic" = {
            # growth_x <- (x - dplyr::lag(x)) / dplyr::lag(x)
            growth_x <- tidyquant::PCT_CHANGE(x, n = 1, fill_na = NA)
            avg_growth <- mean(growth_x, na.rm = TRUE)
          },
          "geometric" = {
            # growth_x <- log(x) - log(dplyr::lag(x))
            growth_x <- tidyquant::PCT_CHANGE(x, n = 1, fill_na = NA)
            growth_x <- tidyquant::LOG(1 + growth_x)
            avg_growth <- mean(growth_x, na.rm = TRUE)
            avg_growth <- exp(avg_growth) - 1
          }
        )

        avg_growth
      }

      # Function definition for trend stats
      stats_fun <- list(
        mean = ~ mean(., na.rm = TRUE),
        median = ~ median(., na.rm = TRUE),
        min = ~ min(., na.rm = TRUE),
        max = ~ max(., na.rm = TRUE),
        arith_growth = ~ avg_growth(., type = "arithmetic"),
        geom_growth = ~ avg_growth(., type = "geometric")
      )

      compute_feats_stats(stats_fun)
    })


    feats_distribution_stats <- reactive({

      # Function definition for distribution stats
      stats_fun <- list(
        mean = ~ mean(., na.rm = TRUE),
        median = ~ median(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE),
        skewness = ~ PerformanceAnalytics::skewness(.x, na.rm = TRUE),
        kurtosis = ~ PerformanceAnalytics::kurtosis(.x, na.rm = TRUE)
      )

      distribution_stats <- compute_feats_stats(stats_fun)

      # Brief info about distribution
      dist_desc <- function(skewness, kurtosis) {
        if (!is.na(skewness)) {
          if (skewness == 0) {
            skew_desc <- "Symmetrical"
          } else if (skewness < 0) {
            skew_desc <- "negative(left) skew"
          } else {
            skew_desc <- "positive(right) skew"
          }
        } else {
          skew_desc <- "unknown skew"
        }

        if (!is.na(skewness)) {
          if (kurtosis == 0) {
            kurt_desc <- "Normal tail"
          } else if (kurtosis < 0) {
            kurt_desc <- "light tail"
          } else {
            kurt_desc <- "heavy tail"
          }
        } else {
          kurt_desc <- "unknown tail"
        }

        desc <- glue::glue("{skew_desc}, {kurt_desc} distribution")

        desc
      }
      distribution_stats <- distribution_stats %>%
        dplyr::mutate(description = dist_desc(.data$skewness, .data$kurtosis))

      distribution_stats
    })


    # Controls interaction ----

    # Update UI with dataset and user inputs
    observe({


      # Refresh output
      # shinyjs::click(id = "update_output")
    })

    # Update UI when user choose feature tabs
    observeEvent(input$ts_feature, ignoreInit = TRUE, {

      # Update setting_tabs according to features
      switch(input$ts_feature,
        "General stats" = {
          updateTabsetPanel(session,
            inputId = "setting_tabs",
            selected = "general_stats_setting"
          )
        },
        "Trend stats" = {
          updateTabsetPanel(session,
            inputId = "setting_tabs",
            selected = "trend_stats_setting"
          )
        },
        "Distribution" = {
          updateTabsetPanel(session,
            inputId = "setting_tabs",
            selected = "distribution_setting"
          )
        },
      )
    })

    # Handler for user to clear focus input for stock and industry
    clear_focus <- function() {

      # Clear selections in table
      dt_table <- "general_stats_table"
      proxy <- DT::dataTableProxy(dt_table)
      DT::selectRows(proxy, selected = NULL)
    }

    # Click to clear focus inputs for stock and industry
    observeEvent(input$clear_focus, ignoreInit = TRUE, {
      clear_focus()
    })

    # Handler for user to select key codes from table
    user_select_keys <- function(DT_tableId, ds_info, key_vars) {
      select_index <- input[[glue::glue("{DT_tableId}_rows_selected")]]
      select_keys <- ds_info[select_index, key_vars]

      select_keys
    }


    # Get key of user selection in general_stats_table
    general_stats_focus_key <- eventReactive(input$general_stats_table_cell_clicked,
      ignoreInit = FALSE,
      ignoreNULL = FALSE,
      {
        if (length(input$general_stats_table_cell_clicked) > 0) {
          switch(input$ts_type,
            "stock" = {
              user_select_keys(
                DT_tableId = "general_stats_table",
                ds_info = feats_general_stats(),
                key_var = c(stock_id_var(), "variable")
              )
            },
            "industry" = {
              user_select_keys(
                DT_tableId = "general_stats_table",
                ds_info = feats_general_stats(),
                key_var = c(industry_id_var(), "variable")
              )
            }
          )
        }
      }
    )

    # Get key of user selection in trend_stats_table
    trend_stats_focus_key <- eventReactive(input$trend_stats_table_cell_clicked,
      ignoreInit = FALSE,
      ignoreNULL = FALSE,
      {
        if (length(input$trend_stats_table_cell_clicked) > 0) {
          switch(input$ts_type,
            "stock" = {
              user_select_keys(
                DT_tableId = "trend_stats_table",
                ds_info = feats_trend_stats(),
                key_var = c(stock_id_var(), "variable")
              )
            },
            "industry" = {
              user_select_keys(
                DT_tableId = "trend_stats_table",
                ds_info = feats_trend_stats(),
                key_var = c(industry_id_var(), "variable")
              )
            }
          )
        }
      }
    )

    # Get key of user selection in distribution_stats_table
    distribution_stats_focus_key <- eventReactive(input$distribution_stats_table_cell_clicked,
      ignoreInit = FALSE,
      ignoreNULL = FALSE,
      {
        if (length(input$distribution_stats_table_cell_clicked) > 0) {
          switch(input$ts_type,
            "stock" = {
              user_select_keys(
                DT_tableId = "distribution_stats_table",
                ds_info = feats_distribution_stats(),
                key_var = c(stock_id_var(), "variable")
              )
            },
            "industry" = {
              user_select_keys(
                DT_tableId = "distribution_stats_table",
                ds_info = feats_distribution_stats(),
                key_var = c(industry_id_var(), "variable")
              )
            }
          )
        }
      }
    )

    ## Output of features ----

    # >> General stats output ----

    output$general_stats_table <- DT::renderDataTable({
      numeric_vars <- zstmodelr::expect_type_fields(
        feats_general_stats(),
        expect_type = "numeric"
      )

      feats_general_stats() %>%
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

    output$general_stats_plot <- renderPlot({

      # Prompt user to choose at least one series for plot
      validate(
        need(NROW(general_stats_focus_key()) > 0,
          message = "Please choose at least one series in above table to showplot."
        )
      )

      # Data setting for plot by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- long_tsbl_focus()
          id_var <- stock_id_var()
        },
        "industry" = {
          tsbl_focus <- long_tsbl_focus_industry()
          id_var <- industry_id_var()
        }
      )

      # Filter data by user selection
      focus_key <- general_stats_focus_key()
      if (!is.null(focus_key)) {
        tsbl_focus <- tsbl_focus %>%
          dplyr::filter(
            .data[[id_var]] %in% focus_key[[id_var]],
            .data[["variable"]] %in% focus_key[["variable"]]
          )
      }


      # Plot General stats by stock or industry
      if (NROW(tsbl_focus) > 0) {
        p <- switch(input$ts_type,
          "stock" = {
            tsbl_focus %>%
              ggplot2::ggplot() +
              ggplot2::geom_boxplot(ggplot2::aes(
                x = .data[[stock_id_var()]],
                y = .data$value_stock,
                color = .data[[industry_id_var()]]
              )) +
              ggplot2::geom_boxplot(ggplot2::aes(
                x = .data[[industry_id_var()]],
                y = .data$value_industry,
                color = .data[[industry_id_var()]],
                fill = .data[[industry_id_var()]]
              ),
              alpha = 0.5
              ) +
              ggplot2::facet_wrap(ggplot2::vars(.data$variable), scales = "free")
          },
          "industry" = {
            tsbl_focus %>%
              ggplot2::ggplot() +
              ggplot2::geom_boxplot(ggplot2::aes(
                x = .data[[industry_id_var()]],
                y = .data$value,
                color = .data[[industry_id_var()]],
                fill = .data[[industry_id_var()]]
              ),
              alpha = 0.5
              ) +
              ggplot2::facet_wrap(ggplot2::vars(.data$variable), scales = "free")
          }
        )

        p <- p + ggplot2::coord_flip() +
          ggplot2::labs(x = NULL, y = NULL) +
          ggplot2::theme_light()
      } else {
        p <- NULL
      }

      p
    })

    # >> Trend stats output ----
    output$trend_stats_table <- DT::renderDataTable({
      numeric_vars <- zstmodelr::expect_type_fields(
        feats_trend_stats(),
        expect_type = "numeric"
      )

      feats_trend_stats() %>%
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
        DT::formatRound(columns = numeric_vars, digits = 3) %>%
        DT::formatPercentage(
          columns = c("arith_growth", "geom_growth"),
          digits = 2
        )
    })

    # output$trend_stats_plot <- renderPlot({
    output$trend_series_trendency_plot <- plotly::renderPlotly({

      # Prompt user to choose at least one series for plot
      validate(
        need(NROW(trend_stats_focus_key()) > 0,
          message = "Please choose at least one series in above table to show plot."
        )
      )

      # Data setting for plot by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- long_tsbl_focus()
          id_var <- stock_id_var()
        },
        "industry" = {
          tsbl_focus <- long_tsbl_focus_industry()
          id_var <- industry_id_var()
        }
      )

      # Filter data by user selection
      focus_key <- trend_stats_focus_key()
      if (!is.null(focus_key)) {
        tsbl_focus <- tsbl_focus %>%
          dplyr::filter(
            .data[[id_var]] %in% focus_key[[id_var]],
            .data[["variable"]] %in% focus_key[["variable"]]
          )
      }

      # Draw the plot
      p <- switch(input$ts_type,
        "stock" = {
          tsbl_focus %>%
            fabletools::autoplot(.data$value_stock) +
            fabletools::autolayer(tsbl_focus,
              .data$value_industry,
              # color = "blue",
              alpha = 0.5,
              linetype = "dotted"
            )
        },
        "industry" = {
          tsbl_focus %>%
            fabletools::autoplot(.data$value)
        }
      )

      # Add smooth lines
      p <- p +
        ggplot2::geom_smooth(
          formula = y ~ x, method = "loess",
          se = FALSE, color = "blue", size = 1,
          alpha = 0.5, linetype = "dashed"
        ) +
        ggplot2::scale_y_continuous(labels = scales::label_number_auto()) +
        ggplot2::facet_grid(
          cols = ggplot2::vars(.data[["variable"]]),
          rows = ggplot2::vars(.data[[id_var]]),
          scales = "free"
        )

      # Set theme of plot
      p <- p + ggplot2::labs(y = NULL) +
        ggplot2::theme_light() +
        # tidyquant::theme_tq() +
        ggplot2::theme(legend.position = "bottom")

      p <- plotly::ggplotly(p, tooltip = c("x", "y", "colour")) %>%
        plotly::layout(showlegend = input$trend_show_legend)

      p
    })

    output$trend_series_compare_a_plot <- plotly::renderPlotly({

      # Prompt user to choose at least one series for plot
      validate(
        need(NROW(trend_stats_focus_key()) > 0,
          message = "Please choose at least one series in above table to show plot."
        )
      )

      # Data setting for plot by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- long_tsbl_focus()
          id_var <- stock_id_var()
        },
        "industry" = {
          tsbl_focus <- long_tsbl_focus_industry()
          id_var <- industry_id_var()
        }
      )

      # Filter data by user selection
      focus_key <- trend_stats_focus_key()
      if (!is.null(focus_key)) {
        tsbl_focus <- tsbl_focus %>%
          dplyr::filter(
            .data[[id_var]] %in% focus_key[[id_var]],
            .data[["variable"]] %in% focus_key[["variable"]]
          )
      }

      # Draw the plot
      p <- switch(input$ts_type,
        "stock" = {
          tsbl_focus %>%
            fabletools::autoplot(.data$value_stock) +
            fabletools::autolayer(tsbl_focus,
              .data$value_industry,
              # color = "blue",
              alpha = 0.5,
              linetype = "dotted"
            )
        },
        "industry" = {
          tsbl_focus %>%
            fabletools::autoplot(.data$value)
        }
      )

      # Add points of series
      p <- p + ggplot2::geom_point() +
        ggplot2::scale_y_continuous(labels = scales::label_number_auto())

      # Set theme of plot
      p <- p + ggplot2::labs(y = NULL) +
        ggplot2::theme_light() +
        ggplot2::theme(legend.position = "bottom")

      p <- plotly::ggplotly(p, tooltip = c("x", "y", "colour")) %>%
        plotly::layout(showlegend = input$trend_show_legend)

      p
    })

    output$trend_series_compare_b_plot <- plotly::renderPlotly({

      # Prompt user to choose at least one series for plot
      validate(
        need(NROW(trend_stats_focus_key()) > 0,
          message = "Please choose at least one series in above table to show plot."
        )
      )

      # Data setting for plot by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- long_tsbl_focus_stock()
          id_var <- stock_id_var()
        },
        "industry" = {
          tsbl_focus <- long_tsbl_focus_industry()
          id_var <- industry_id_var()
        }
      )

      # Filter data by user selection
      focus_key <- trend_stats_focus_key()
      if (!is.null(focus_key)) {
        tsbl_focus <- tsbl_focus %>%
          dplyr::filter(
            .data[[id_var]] %in% focus_key[[id_var]],
            .data[["variable"]] %in% focus_key[["variable"]]
          )
      }

      # Add median for comparing
      tsbl_focus <- tsbl_focus %>%
        tsibble::group_by_key() %>%
        dplyr::mutate(
          average = median(.data$value, na.rm = TRUE),
          key = paste(.data[[id_var]], .data$variable, sep = "/")
        )

      # Draw the plot
      p <- tsbl_focus %>%
        ggplot2::ggplot(aes(
          x = .data$date, y = .data$value,
          fill = .data$key, color = .data$key
        )) +
        ggplot2::geom_col(position = ggplot2::position_dodge()) +
        ggplot2::geom_line(aes(y = .data$average),
          alpha = 0.5,
          linetype = "twodash"
        ) +
        ggplot2::scale_y_continuous(labels = scales::label_number_auto())

      # Set theme of plot
      p <- p + ggplot2::labs(y = NULL) +
        ggplot2::theme_light() +
        ggplot2::theme(legend.position = "bottom")

      p <- plotly::ggplotly(p, tooltip = c("x", "y", "colour")) %>%
        plotly::layout(showlegend = input$trend_show_legend)

      p
    })

    output$trend_seq_growth_compare_plot <- plotly::renderPlotly({

      # Prompt user to choose at least one series for plot
      validate(
        need(NROW(trend_stats_focus_key()) > 0,
          message = "Please choose at least one series in above table to show plot."
        )
      )

      # Data setting for plot by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- long_tsbl_focus_stock()
          id_var <- stock_id_var()
        },
        "industry" = {
          tsbl_focus <- long_tsbl_focus_industry()
          id_var <- industry_id_var()
        }
      )

      # Filter data by user selection
      focus_key <- trend_stats_focus_key()
      if (!is.null(focus_key)) {
        tsbl_focus <- tsbl_focus %>%
          dplyr::filter(
            .data[[id_var]] %in% focus_key[[id_var]],
            .data[["variable"]] %in% focus_key[["variable"]]
          )
      }

      # Add growth rate column
      tsbl_focus <- tsbl_focus %>%
        tsibble::group_by_key() %>%
        dplyr::mutate(
          growth = tidyquant::PCT_CHANGE(.data$value, n = 1, fill_na = NA),
          average = median(.data$growth, na.rm = TRUE),
        ) %>%
        dplyr::mutate(
          key = paste(.data[[id_var]], .data$variable, sep = "/")
        )

      # Draw the plot
      p <- tsbl_focus %>%
        ggplot2::ggplot(aes(
          x = .data$date, y = .data$growth,
          fill = .data$key, color = .data$key
        )) +
        ggplot2::geom_col(position = ggplot2::position_dodge()) +
        ggplot2::geom_line(aes(y = .data$average),
          alpha = 0.5,
          linetype = "twodash"
        ) +
        ggplot2::scale_y_continuous(labels = scales::label_percent())

      # Set theme of plot
      p <- p + ggplot2::labs(y = NULL) +
        ggplot2::theme_light() +
        ggplot2::theme(legend.position = "bottom")

      p <- plotly::ggplotly(p, tooltip = c("x", "y", "colour")) %>%
        plotly::layout(showlegend = input$trend_show_legend)

      p
    })

    output$trend_yoy_growth_compare_plot <- plotly::renderPlotly({

      # Prompt user to choose at least one series for plot
      validate(
        need(NROW(trend_stats_focus_key()) > 0,
          message = "Please choose at least one series in above table to show plot."
        )
      )

      # Data setting for plot by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- long_tsbl_focus_stock()
          id_var <- stock_id_var()
        },
        "industry" = {
          tsbl_focus <- long_tsbl_focus_industry()
          id_var <- industry_id_var()
        }
      )

      # Filter data by user selection
      focus_key <- trend_stats_focus_key()
      if (!is.null(focus_key)) {
        tsbl_focus <- tsbl_focus %>%
          dplyr::filter(
            .data[[id_var]] %in% focus_key[[id_var]],
            .data[["variable"]] %in% focus_key[["variable"]]
          )
      }


      # Add growth rate column

      # Compute lag number for compute YOY growth rate
      date_index <- tsbl_focus[["date"]]
      if (tsibble::is_yearquarter(date_index)) {
        lag_n <- 4
      } else if (tsibble::is_yearmonth(date_index)) {
        lag_n <- 12
      } else if (tsibble::is_yearweek(date_index)) {
        lag_n <- 7
      } else {
        lag_n <- 1
      }

      tsbl_focus <- tsbl_focus %>%
        tsibble::group_by_key() %>%
        dplyr::mutate(
          growth = tidyquant::PCT_CHANGE(.data$value, n = lag_n, fill_na = NA),
          average = median(.data$growth, na.rm = TRUE),
        ) %>%
        dplyr::mutate(
          key = paste(.data[[id_var]], .data$variable, sep = "/")
        )

      # Draw the plot
      p <- tsbl_focus %>%
        ggplot2::ggplot(aes(
          x = .data$date, y = .data$growth,
          fill = .data$key, color = .data$key
        )) +
        ggplot2::geom_col(position = ggplot2::position_dodge()) +
        ggplot2::geom_line(aes(y = .data$average),
          alpha = 0.5,
          linetype = "twodash"
        ) +
        ggplot2::scale_y_continuous(labels = scales::label_percent())

      # Set theme of plot
      p <- p + ggplot2::labs(y = NULL) +
        ggplot2::theme_light() +
        ggplot2::theme(legend.position = "bottom")

      p <- plotly::ggplotly(p, tooltip = c("x", "y", "colour")) %>%
        plotly::layout(showlegend = input$trend_show_legend)

      p
    })

    output$trend_series_growth_plot <- plotly::renderPlotly({

      # Prompt user to choose at least one series for plot
      validate(
        need(NROW(trend_stats_focus_key()) > 0,
          message = "Please choose at least one series in above table to show plot."
        )
      )

      # Data setting for plot by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- long_tsbl_focus_stock()
          id_var <- stock_id_var()
        },
        "industry" = {
          tsbl_focus <- long_tsbl_focus_industry()
          id_var <- industry_id_var()
        }
      )

      # Filter data by user selection
      focus_key <- trend_stats_focus_key()
      if (!is.null(focus_key)) {
        tsbl_focus <- tsbl_focus %>%
          dplyr::filter(
            .data[[id_var]] %in% focus_key[[id_var]],
            .data[["variable"]] %in% focus_key[["variable"]]
          )
      }

      # Add growth rate column

      # Compute lag number for compute YOY growth rate
      date_index <- tsbl_focus[["date"]]
      if (tsibble::is_yearquarter(date_index)) {
        lag_n <- 4
      } else if (tsibble::is_yearmonth(date_index)) {
        lag_n <- 12
      } else if (tsibble::is_yearweek(date_index)) {
        lag_n <- 7
      } else {
        lag_n <- 1
      }

      tsbl_focus <- tsbl_focus %>%
        tsibble::group_by_key() %>%
        dplyr::mutate(
          seq_growth = tidyquant::PCT_CHANGE(.data$value, n = 1, fill_na = NA),
          yoy_growth = tidyquant::PCT_CHANGE(.data$value, n = lag_n, fill_na = NA),
        ) %>%
        dplyr::mutate(
          key = paste(.data[[id_var]], .data$variable, sep = "/")
        )

      # Transform growth to fit second y axis
      y_sec_axis_scale <- max(tsbl_focus$value, na.rm = TRUE) /
        max(c(tsbl_focus$seq_growth, tsbl_focus$yoy_growth), na.rm = TRUE)
      tsbl_focus <- tsbl_focus %>%
        tsibble::group_by_key() %>%
        dplyr::mutate(
          scaled_seq_growth = .data$seq_growth * y_sec_axis_scale,
          scaled_yoy_growth = .data$yoy_growth * y_sec_axis_scale
        )

      # Draw the plot
      p <- tsbl_focus %>%
        ggplot2::ggplot(aes(
          x = .data$date,
          group = .data[[id_var]],
        )) +
        ggplot2::geom_col(
          aes(
            y = .data$value,
            fill = .data$key, color = "",
            text = paste0(
              "value:",
              prettyNum(.data$value, big.mark = ",")
            )
          ),
          position = ggplot2::position_dodge(),
        ) +
        ggplot2::geom_line(
          aes(
            y = .data$scaled_seq_growth,
            text = paste0(
              "seq_growth:",
              scales::percent(.data$seq_growth, big.mark = ",")
            ),
            color = "seq growth"
          ),
          alpha = 0.5,
          size = 1
        ) +
        ggplot2::geom_line(
          aes(
            y = .data$scaled_yoy_growth,
            text = paste0(
              "yoy_growth:",
              scales::percent(.data$yoy_growth, big.mark = ",")
            ),
            color = "yoy growth"
          ),
          alpha = 0.5,
          size = 1
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::label_number_auto(),
          sec.axis = ggplot2::sec_axis(~ . / y_sec_axis_scale)
        ) +
        ggplot2::facet_grid(
          cols = ggplot2::vars(.data[["variable"]]),
          rows = ggplot2::vars(.data[[id_var]]),
          scales = "free",
          switch = "y"
        )

      # Set theme of plot
      p <- p + ggplot2::labs(y = NULL) +
        ggplot2::theme_light() +
        ggplot2::theme(legend.position = "bottom")

      p <- plotly::ggplotly(p, tooltip = c("x", "colour", "text")) %>%
        plotly::layout(showlegend = input$trend_show_legend)

      p
    })

    # >> Distribution stats output ----
    output$distribution_stats_table <- DT::renderDataTable({
      numeric_vars <- zstmodelr::expect_type_fields(
        feats_distribution_stats(),
        expect_type = "numeric"
      )

      feats_distribution_stats() %>%
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

    output$distribution_density_plot <- renderPlot({

      # Prompt user to choose at least one series for plot
      validate(
        need(NROW(distribution_stats_focus_key()) > 0,
          message = "Please choose at least one series in above table to show plot."
        )
      )

      # Data setting for plot by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- long_tsbl_focus_stock()
          id_var <- stock_id_var()
        },
        "industry" = {
          tsbl_focus <- long_tsbl_focus_industry()
          id_var <- industry_id_var()
        }
      )

      # Filter data by user selection
      focus_key <- distribution_stats_focus_key()

      if (!is.null(focus_key)) {
        tsbl_focus <- tsbl_focus %>%
          dplyr::filter(
            .data[[id_var]] %in% focus_key[[id_var]],
            .data[["variable"]] %in% focus_key[["variable"]]
          )
      }

      # Plot stats
      if (NROW(tsbl_focus) > 0) {
        p <- tsbl_focus %>%
          ggplot2::ggplot(ggplot2::aes(x = .data$value)) +
          ggplot2::geom_histogram(
            ggplot2::aes(y = ggplot2::after_stat(density)),
            alpha = 0.5
          ) +
          ggplot2::geom_density(ggplot2::aes(color = "Kernel Estimates")) +
          ggplot2::geom_rug(alpha = 0.1) +

          # Add theoretical distribution with same mean/sd as a reference
          ggh4x::stat_theodensity(ggplot2::aes(color = "Theoretical"),
            distri = "norm"
          ) +

          # Add distribution summary: mean/median
          ggplot2::stat_summary(
            ggplot2::aes(x = 1, y = .data$value, linetype = "mean"),
            fun.data = ~ data.frame(xintercept = mean(.x, na.rm = TRUE)),
            geom = "vline", color = "blue"
          ) +
          ggplot2::stat_summary(
            ggplot2::aes(x = 1, y = .data$value, linetype = "median"),
            fun.data = ~ data.frame(xintercept = median(.x, na.rm = TRUE)),
            geom = "vline", color = "darkgreen"
          ) +
          ggplot2::facet_grid(
            cols = ggplot2::vars(.data[["variable"]]),
            rows = ggplot2::vars(.data[[id_var]]),
            scales = "free"
          )

        p <- p +
          ggplot2::labs(x = NULL, y = NULL) +
          ggplot2::theme_light() +
          ggplot2::theme(legend.position = "bottom")
      } else {
        p <- NULL
      }

      p
    })

    output$distribution_qq_plot <- renderPlot({

      # Prompt user to choose at least one series for plot
      validate(
        need(NROW(distribution_stats_focus_key()) > 0,
          message = "Please choose at least one series in above table to show plot."
        )
      )

      # Data setting for plot by stock or industry
      switch(input$ts_type,
        "stock" = {
          tsbl_focus <- long_tsbl_focus_stock()
          id_var <- stock_id_var()
        },
        "industry" = {
          tsbl_focus <- long_tsbl_focus_industry()
          id_var <- industry_id_var()
        }
      )

      # Filter data by user selection
      focus_key <- distribution_stats_focus_key()
      if (!is.null(focus_key)) {
        tsbl_focus <- tsbl_focus %>%
          dplyr::filter(
            .data[[id_var]] %in% focus_key[[id_var]],
            .data[["variable"]] %in% focus_key[["variable"]]
          )
      }

      # Plot stats
      if (NROW(tsbl_focus) > 0) {
        p <- tsbl_focus %>%
          ggplot2::ggplot(ggplot2::aes(sample = .data$value)) +
          qqplotr::stat_qq_band(alpha = 0.5) +
          qqplotr::stat_qq_line() +
          qqplotr::stat_qq_point() +
          ggplot2::facet_grid(
            cols = ggplot2::vars(.data[["variable"]]),
            rows = ggplot2::vars(.data[[id_var]]),
            scales = "free"
          )

        p <- p +
          ggplot2::labs(x = NULL, y = NULL) +
          ggplot2::theme_light()
      } else {
        p <- NULL
      }

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
    ts_feat_basic_tidyverts_ui("ts_feat_basic_tidyverts_module")
  )
  server <- function(input, output, session) {
    ts_feat_basic_tidyverts_server(
      "ts_feat_basic_tidyverts_module",
      tsbl_vars = reactive(tsbl_vars),
      tsbl_vars_average = reactive(tsbl_vars_average)
    )
  }

  shinyApp(ui, server)
}
