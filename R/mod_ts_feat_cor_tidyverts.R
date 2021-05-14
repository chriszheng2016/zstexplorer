#' ts_feat_cor_tidyverts
#'
#' @description A shiny module for ts_feat_cor_tidyverts.
#'
#' @details
#'  The module is an UI for user to display correlation features of time series
#'  tidyverts family packages.
#'
#' @name ts_feat_cor_tidyverts
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   ts_feat_cor_tidyverts_ui("ts_feat_cor_tidyverts_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   ts_feat_cor_tidyverts <- ts_feat_cor_tidyverts_server(
#'     "ts_feat_cor_tidyverts_module",
#'     tsbl_vars = reactive(tsbl_vars),
#'     tsbl_vars_average = reactive(tsbl_vars_average)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' ts_feat_cor_tidyverts_app()
#' }
#'
NULL

#' UI function of ts_feat_cor_tidyverts
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn ts_feat_cor_tidyverts  UI function of ts_feat_cor_tidyverts.
#' @importFrom shiny NS tagList
ts_feat_cor_tidyverts_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
        wellPanel(
          tabsetPanel(
            id = ns("setting_tabs"),
            type = "hidden",
            selected = "auto_cor_setting",
            tabPanelBody(
              value = "auto_cor_setting",
              selectInput(
                inputId = ns("auto_cor_show_stats"),
                label = strong("Show stats:"),
                choices = c("ACF", "PACF")
              )
            ),
            tabPanelBody(
              value = "cross_cor_setting",
              selectInput(
                inputId = ns("cross_cor_show_stats"),
                label = strong("Show stats:"),
                choices = c(
                  "Scatter correlation", "CCF", "Rolling correlation"
                )
              ),
              selectInput(
                inputId = ns("cross_cor_base_series"),
                label = strong("Basing series:"),
                choices = ""
              ),
              selectInput(
                inputId = ns("cross_cor_compare_series"),
                label = strong("Comparing series:"),
                multiple = TRUE,
                choices = ""
              ),
            )
          )
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("ts_feature"),
          type = "tabs",
          tabPanel(
            "Auto correlation",
            fluidRow(
              tabsetPanel(
                id = ns("auto_cor_stats"),
                type = "hidden",
                selected = "auto_cor_acf_stats",
                tabPanelBody(
                  value = "auto_cor_acf_stats",
                  DT::dataTableOutput(ns("auto_cor_acf_table"))
                ),
                tabPanelBody(
                  value = "auto_cor_pacf_stats",
                  DT::dataTableOutput(ns("auto_cor_pacf_table"))
                )
              )
            ),
            fluidRow(
              box(
                title = "ACF Plot", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE, width = 6,
                plotOutput(ns("auto_cor_acf_plot"), height = "300px")
              ),
              box(
                title = "PACF Plot", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE, width = 6,
                plotOutput(ns("auto_cor_pacf_plot"), height = "300px")
              )
            )
          ),
          tabPanel(
            "Cross correlation",
            br(),
            fluidRow(
              column(
                width = 6,
                plotOutput(ns("cross_cor_compare_pair_plot"), height = "600px"),
              ),
              column(
                width = 6,

                tabsetPanel(
                  id = ns("cross_cor_stats"),
                  type = "hidden",
                  selected = "cross_cor_pairs_stats",
                  tabPanelBody(
                    value = "cross_cor_pairs_stats",
                    plotOutput(ns("cross_cor_scatter_plot"), height = "600px")
                  ),
                  tabPanelBody(
                    value = "cross_cor_ccf_stats",
                    plotOutput(ns("cross_cor_ccf_plot"), height = "600px")
                  ),
                  tabPanelBody(
                    value = "cross_cor_rolling_stats",
                    plotOutput(ns("cross_cor_rolling_plot"), height = "600px")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Server function of ts_feat_cor_tidyverts
#'
#' @param tsbl_vars A tsibble of vars of time series.
#'
#' @param tsbl_vars_average A tsibble of average of vars of time series.
#'
#' @describeIn ts_feat_cor_tidyverts  Server function of ts_feat_cor_tidyverts.
#' @return * Server function doesn't return value.
ts_feat_cor_tidyverts_server <- function(id, tsbl_vars, tsbl_vars_average) {
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

    focus_stock <- reactive({
      unique(tsbl_vars_stock_raw()[[stock_id_var()]])
    })

    focus_industry <- reactive({
      unique(tsbl_vars_stock_raw()[[industry_id_var()]])
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
          cols = focus_var(),
          names_to = "variable", values_to = "value"
        )
    })

    nest_varible_focus_stock <- reactive({
      long_tsbl_focus_stock() %>%
        dplyr::nest_by(.data$variable, .key = "tsbl_variable") %>%
        dplyr::ungroup()
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
          cols = focus_var(),
          names_to = "variable", values_to = "value"
        )
    })

    nest_varible_focus_industry <- reactive({
      long_tsbl_focus_industry() %>%
        dplyr::nest_by(.data$variable, .key = "tsbl_variable") %>%
        dplyr::ungroup()
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
      variable_codes <- stats::setNames(variable_codes, variable_names)

      variable_codes
    })

    # Available industries for choices
    available_industry_codes <- reactive({
      tsbl_vars <- tsbl_vars_stock_raw()
      industry_codes <- sort(unique(tsbl_vars[[industry_id_var()]]))
      industry_names <- paste0(
        code2name(industry_codes, exact_match = TRUE),
        "(", industry_codes, ")"
      )
      industry_codes <- stats::setNames(industry_codes, industry_names)

      industry_codes
    })

    # Available stocks for choices
    available_stock_codes <- reactive({
      tsbl_vars <- tsbl_vars_stock_raw()
      stock_codes <- sort(unique(tsbl_vars[[stock_id_var()]]))
      stock_names <- paste0(
        code2name(stock_codes, exact_match = TRUE),
        "(", stock_codes, ")"
      )
      stock_codes <- stats::setNames(stock_codes, stock_names)

      stock_codes
    })

    # Available base series for comparison
    available_base_series <- reactive({
      switch(input$ts_type,
        "stock" = {
          series_codes <- available_stock_codes()
        },
        "industry" = {
          series_codes <- available_industry_codes()
        }
      )

      series_names <- paste0(
        code2name(series_codes, exact_match = TRUE),
        "(", series_codes, ")"
      )
      series_codes <- stats::setNames(series_codes, series_names)

      series_codes
    })

    # Available comparing series for comparison
    available_compare_series <- reactive({
      series_codes <- setdiff(
        available_base_series(),
        input$cross_cor_base_series
      )

      if (length(series_codes) > 0) {
        series_names <- paste0(
          code2name(series_codes, exact_match = TRUE),
          "(", series_codes, ")"
        )
        series_codes <- stats::setNames(series_codes, series_names)
      }

      series_codes
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
          code2name(.data[[id_var]]), character(0)
        )) %>%
        dplyr::select(
          c("variable", id_var, id_name),
          tidyselect::everything()
        )

    }

    feat_acf_stats <- reactive({
      compute_feats_stats(feasts::feat_acf)
    })

    feat_pacf_stats <- reactive({
      compute_feats_stats(feasts::feat_pacf)
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


      updateSelectInput(
        session = session, inputId = "cross_cor_base_series",
        choices = available_base_series(),
        # Set selected with current value to ensure not clear current input
        selected = input$cross_cor_base_series
      )

      updateSelectInput(
        session = session, inputId = "cross_cor_compare_series",
        choices = available_compare_series(),
        # Set selected with current value to ensure not clear current input
        selected = input$cross_cor_compare_series
      )
    })

    # Update UI when user choose feature tabs
    observeEvent(input$ts_feature, ignoreInit = TRUE, {

      # Update setting_tabs according to features
      if (input$ts_feature %in% c("Auto correlation")) {
        updateTabsetPanel(session,
          inputId = "setting_tabs",
          selected = "auto_cor_setting"
        )
      } else {
        updateTabsetPanel(session,
          inputId = "setting_tabs",
          selected = "cross_cor_setting"
        )
      }
    })

    # Handler for user to clear focus input for stock and industry
    clear_focus <- function() {

      # Clear selections in tab;e
      dt_table <- "simple_stats_table"
      proxy <- DT::dataTableProxy(dt_table)
      DT::selectRows(proxy, selected = NULL)

      updateTextInput(
        session = session,
        inputId = "cross_cor_base_series",
        value = ""
      )

      updateTextInput(
        session = session,
        inputId = "cross_cor_compare_series",
        value = ""
      )

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

    # Get key of user selection in acf_stats_table
    acf_focus_key <- eventReactive(input$auto_cor_acf_table_cell_clicked,
      ignoreInit = FALSE,
      ignoreNULL = FALSE,
      {
        if (length(input$auto_cor_acf_table_cell_clicked) > 0) {
          switch(input$ts_type,
            "stock" = {
              user_select_keys(
                DT_tableId = "auto_cor_acf_table",
                ds_info = feat_acf_stats(),
                key_var = c(stock_id_var(), "variable")
              )
            },
            "industry" = {
              user_select_keys(
                DT_tableId = "auto_cor_acf_table",
                ds_info = feat_acf_stats(),
                key_var = c(industry_id_var(), "variable")
              )
            }
          )
        }
      }
    )

    # Get key of user selection in pacf_stats_table
    pacf_focus_key <- eventReactive(input$auto_cor_pacf_table_cell_clicked,
      ignoreInit = FALSE,
      ignoreNULL = FALSE,
      {
        if (length(input$auto_cor_pacf_table_cell_clicked) > 0) {
          switch(input$ts_type,
            "stock" = {
              user_select_keys(
                DT_tableId = "auto_cor_pacf_table",
                ds_info = feat_pacf_stats(),
                key_var = c(stock_id_var(), "variable")
              )
            },
            "industry" = {
              user_select_keys(
                DT_tableId = "auto_cor_pacf_table",
                ds_info = feat_pacf_stats(),
                key_var = c(industry_id_var(), "variable")
              )
            }
          )
        }
      }
    )

    # Get key of user selection in acf_stats_table and pacf_stats_table
    acf_pacf_focus_key <- reactive({
      focus_key <- NULL
      if (!is.null(acf_focus_key()) || !is.null(pacf_focus_key())) {
        focus_key <- dplyr::bind_rows(acf_focus_key(), pacf_focus_key())
        focus_key <- unique(focus_key)
      }

      focus_key
    })


    # User select to show auto-correlation stats
    observeEvent(input$auto_cor_show_stats, ignoreInit = TRUE, {
      switch(input$auto_cor_show_stats,
        "ACF" = {
          updateTabsetPanel(session,
            inputId = "auto_cor_stats",
            selected = "auto_cor_acf_stats"
          )
        },
        "PACF" = {
          updateTabsetPanel(session,
            inputId = "auto_cor_stats",
            selected = "auto_cor_pacf_stats"
          )
        },
      )
    })


    # User select to show cross-correlation stats

    observeEvent(input$cross_cor_show_stats, ignoreInit = TRUE, {
      switch(input$cross_cor_show_stats,
        "Scatter correlation" = {
          updateTabsetPanel(session,
            inputId = "cross_cor_stats",
            selected = "cross_cor_pairs_stats"
          )
        },
        "CCF" = {
          updateTabsetPanel(session,
            inputId = "cross_cor_stats",
            selected = "cross_cor_ccf_stats"
          )
        },
        "Rolling correlation" = {
          updateTabsetPanel(session,
            inputId = "cross_cor_stats",
            selected = "cross_cor_rolling_stats"
          )
        },
      )
    })



    ## Output of features ----

    # >> Auto correlation output ----

    output$auto_cor_acf_table <- DT::renderDataTable({
      numeric_vars <- zstmodelr::expect_type_fields(
        feat_acf_stats(),
        expect_type = "numeric"
      )

      feat_acf_stats() %>%
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

    output$auto_cor_pacf_table <- DT::renderDataTable({
      numeric_vars <- zstmodelr::expect_type_fields(
        feat_pacf_stats(),
        expect_type = "numeric"
      )

      feat_pacf_stats() %>%
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

    output$auto_cor_acf_plot <- renderPlot({

      # Prompt user to choose at least one series for plot
      validate(
        need(NROW(acf_pacf_focus_key()) > 0,
          message = "Please choose at least one series in above table for ACF/PACF plots."
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
      focus_key <- acf_pacf_focus_key()
      if (!is.null(focus_key)) {
        tsbl_focus <- tsbl_focus %>%
          dplyr::filter(
            .data[[id_var]] %in% focus_key[[id_var]],
            .data[["variable"]] %in% focus_key[["variable"]]
          )
      }

      # Plot ACF
      facet_fomula <- rlang::parse_expr(glue::glue("{id_var} ~ variable"))
      if (NROW(tsbl_focus) > 0) {
        p <- tsbl_focus %>%
          feasts::ACF(.data$value) %>%
          fabletools::autoplot() +
          ggplot2::facet_grid({{ facet_fomula }}) +
          ggplot2::theme_light()
      } else {
        p <- NULL
      }

      p
    })

    output$auto_cor_pacf_plot <- renderPlot({

      # Prompt user to choose at least one series for plot
      validate(
        need(NROW(acf_pacf_focus_key()) > 0,
          message = "Please choose at least one series in above table for ACF/PACF plots."
        )
      )
      # Plot setting by stock or industry
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
      focus_key <- acf_pacf_focus_key()
      if (!is.null(focus_key)) {
        tsbl_focus <- tsbl_focus %>%
          dplyr::filter(
            .data[[id_var]] %in% focus_key[[id_var]],
            .data[["variable"]] %in% focus_key[["variable"]]
          )
      }

      # Plot PACF
      facet_fomula <- rlang::parse_expr(glue::glue("{id_var} ~ variable"))
      if (NROW(tsbl_focus) > 0) {
        p <- tsbl_focus %>%
          feasts::PACF(.data$value) %>%
          fabletools::autoplot() +
          ggplot2::facet_grid({{ facet_fomula }}) +
          ggplot2::theme_light()
      } else {
        p <- NULL
      }

      p
    })

    # >> Cross correlation output ----

    # Functions to avoid duplicated code

    # Validate inputs for cross correlation
    vaildate_cross_cor_inputs <- function() {
      max_compare_series <- 5
      validate(
        need(input$cross_cor_base_series,
          message = "Please choose a basing series."
        ),
        need(input$cross_cor_compare_series,
          message = "Please to choose at least one comparing series."
        ),
        need(length(input$cross_cor_compare_series) <= max_compare_series,
          message = glue::glue(
            "Too many comparing series(Maximum number:{max_compare_series})"
          )
        )
      )
    }

    # Generate cross correlation plot by specific plot generator function
    generate_cross_cor_plot <- function(cross_cor_plot_fun,
                                        tile_template = "Plot") {
      # Setting for plot
      switch(input$ts_type,
        "stock" = {
          nest_varaible_focus <- nest_varible_focus_stock()
          id_field <- stock_id_var()
        },
        "industry" = {
          nest_varaible_focus <- nest_varible_focus_industry()
          id_field <- industry_id_var()
        }
      )

      # Generate plot
      vaildate_cross_cor_inputs()
      nest_varaible_focus <-
        nest_varaible_focus %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          panel_plot = list(
            cross_cor_plot_fun(.data$tsbl_variable,
              date_field = "date",
              id_field = id_field,
              value_field = "value",
              base_series = input$cross_cor_base_series,
              compare_series = input$cross_cor_compare_series
            )
          ),
          panel_title = glue::glue("{tile_template} - {.data$variable}")
        )



      p <- nest_varaible_focus$panel_plot[[1]]

      p <- p +
        ggplot2::labs(title = nest_varaible_focus$panel_title[[1]]) +
        ggplot2::theme_light()


      p
    }

    output$cross_cor_scatter_plot <- renderPlot({


      # Function to draw pairs scatter plot
      pairs_scatter_plot <- function(tsbl_variable,
                                     date_field = "date",
                                     id_field = "stkcd",
                                     value_field = "value",
                                     base_series = NULL,
                                     compare_series = NULL) {

        # Convert wider format for comparing
        tsbl_series <- tsbl_variable %>%
          dplyr::select(tidyselect::all_of(
            c(date_field, id_field, value_field)
          )) %>%
          tidyr::pivot_wider(
            names_from = tidyselect::all_of(id_field),
            values_from = tidyselect::all_of(value_field),
            id_cols = tidyselect::all_of(date_field)
          )

        # Filter series for comparing
        series <- c(base_series, compare_series)
        if (isTruthy(series)) {
          if (all(series %in% names(tsbl_series))) {
            tsbl_series <- tsbl_series %>%
              dplyr::select(tidyselect::all_of(
                c(date_field, series)
              )) %>%
              tsibble::as_tsibble(index = {{ date_field }})
          } else {
            tsbl_series <- NULL
          }
        }

        # Output scatter plot by ggparis
        if (length(tsbl_series) > 0) {
          tsbl_series %>%
            tibble::as_tibble() %>%
            dplyr::select(-tidyselect::all_of(c(date_field))) %>%
            GGally::ggpairs()
        }
      }

      # Main function

      # Generate cross correlation plot
      p <- generate_cross_cor_plot(
        pairs_scatter_plot,
        "Pairs Scatter between series"
      )

      p
    })

    output$cross_cor_ccf_plot <- renderPlot({

      # Function to draw pairs ccf plot
      pairs_ccf_plot <- function(tsbl_variable,
                                 date_field = "date",
                                 id_field = "stkcd",
                                 value_field = "value",
                                 base_series = NULL,
                                 compare_series = NULL) {

        # Convert wider format for comparing
        tsbl_series <- tsbl_variable %>%
          dplyr::select(tidyselect::all_of(
            c(date_field, id_field, value_field)
          )) %>%
          tidyr::pivot_wider(
            names_from = tidyselect::all_of(id_field),
            values_from = tidyselect::all_of(value_field),
            id_cols = tidyselect::all_of(date_field)
          )

        # Filter series for comparing
        series <- c(base_series, compare_series)
        if (isTruthy(series)) {
          if (all(series %in% names(tsbl_series))) {
            tsbl_series <- tsbl_series %>%
              dplyr::select(tidyselect::all_of(
                c(date_field, series)
              )) %>%
              tsibble::as_tsibble(index = {{ date_field }})
          } else {
            tsbl_series <- NULL
          }
        }

        # Output ccf plots by ggmatrix
        if (length(tsbl_series) > 0) {
          tbl_ccf <- tibble::tibble(
            base_series = base_series,
            compare_series = compare_series
          )

          # Function to compute CCF for a pair of series
          ccf_fun <- function(tsbl_series, base_series, compare_series) {
            base_series <- rlang::sym(base_series)
            compare_series <- rlang::sym(compare_series)
            feasts::CCF(tsbl_series, !!base_series, !!compare_series)
          }

          # Function to plot CCF for a pair of series
          ccf_plot <- function(ccf_data, title = "CCF plot") {
            p <- fabletools::autoplot(ccf_data)
            p + ggplot2::labs(title = title)
          }

          # Create individual plots in batch
          tbl_ccf <- tbl_ccf %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              ccf_data = list(ccf_fun(
                tsbl_series, .data$base_series,
                .data$compare_series
              )),
              ccf_plot = list(ccf_plot(.data$ccf_data,
                title = glue::glue("{.data$compare_series} vs {.data$base_series}")
              ))
            )

          # Combine small plots into ggmatrix
          p <- GGally::ggmatrix(tbl_ccf$ccf_plot,
            ncol = 1,
            nrow = length(tbl_ccf$ccf_plot),
            xAxisLabels = glue::glue("Cross correlation with {tbl_ccf$base_series}"),
            yAxisLabels = tbl_ccf$compare_series,
            switch = "y"
          )

          p
        }
      }

      # Main function

      # Generate cross correlation plot
      p <- generate_cross_cor_plot(
        pairs_ccf_plot,
        "CCF between series"
      )

      p
    })

    output$cross_cor_rolling_plot <- renderPlot({

      # Function to draw pairs ccf plot
      pairs_rolling_cor_plot <- function(tsbl_variable,
                                         date_field = "date",
                                         id_field = "stkcd",
                                         value_field = "value",
                                         base_series = NULL,
                                         compare_series = NULL) {

        # Convert wider format for comparing
        tsbl_series <- tsbl_variable %>%
          dplyr::select(tidyselect::all_of(
            c(date_field, id_field, value_field)
          )) %>%
          tidyr::pivot_wider(
            names_from = tidyselect::all_of(id_field),
            values_from = tidyselect::all_of(value_field),
            id_cols = tidyselect::all_of(date_field)
          )

        # Filter series for comparing
        series <- c(base_series, compare_series)
        if (isTruthy(series)) {
          if (all(series %in% names(tsbl_series))) {
            tsbl_series <- tsbl_series %>%
              dplyr::select(tidyselect::all_of(
                c(date_field, series)
              )) %>%
              tsibble::as_tsibble(index = {{ date_field }})
          } else {
            tsbl_series <- NULL
          }
        }

        # Output ccf plots by ggmatrix
        if (length(tsbl_series) > 0) {
          tbl_roll_cor <- tibble::tibble(
            base_series = base_series,
            compare_series = compare_series
          )

          # Function to compute Rolling correlationrelation for a pair of series
          roll_cor_fun <- function(tsbl_series, base_series, compare_series) {

            # Function to compute correlation between series
            cor_fun <- function(data, x, y) {
              safe_cor <- purrr::possibly(cor, otherwise = NA)
              safe_cor(data[x], data[y], use = "complete.obs")[1]
            }

            # Use index frequency as moving windows size
            index_data <- tsbl_series[[tsibble::index_var(tsbl_series)]]
            moving_window_size <- tsibble::guess_frequency(index_data)

            roll_cor <- slider::slide_dbl(
              tsbl_series,
              .f = ~ cor_fun(.x, base_series, compare_series),
              .before =  moving_window_size - 1,
              .complete = TRUE
            )

            tsbl_roll_cor <- tsbl_series %>%
              dplyr::transmute(roll_cor = roll_cor)

            tsbl_roll_cor
          }

          # Function to plot Rolling correlation for a pair of series
          roll_cor_plot <- function(roll_cor_data,
                                    title = "Rolling correlationrelation") {
            p <- roll_cor_data %>%
              fabletools::autoplot(.data$roll_cor)
            p + ggplot2::labs(title = title)
          }

          # Create individual plots in batch
          tbl_roll_cor <- tbl_roll_cor %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              roll_cor_data = list(
                roll_cor_fun(tsbl_series, .data$base_series, .data$compare_series)
              ),
              roll_cor_plot = list(roll_cor_plot(.data$roll_cor_data,
                title = glue::glue("{.data$compare_series} vs {.data$base_series}")
              ))
            )

          # Combine small plots into ggmatrix
          p <- GGally::ggmatrix(tbl_roll_cor$roll_cor_plot,
            ncol = 1,
            nrow = length(tbl_roll_cor$roll_cor_plot),
            xAxisLabels = glue::glue("Rolling correlationrelation with {tbl_roll_cor$base_series}"),
            yAxisLabels = tbl_roll_cor$compare_series,
            switch = "y"
          )

          p
        }
      }

      # Main function

      # Generate cross correlation plot
      p <- generate_cross_cor_plot(
        pairs_rolling_cor_plot,
        "Rolling correlationrelation between series"
      )

      p
    })

    output$cross_cor_compare_pair_plot <- renderPlot({

      # Function to draw pair compare series plot
      pairs_compare_pair_plot <- function(tsbl_variable,
                                          date_field = "date",
                                          id_field = "stkcd",
                                          value_field = "value",
                                          base_series = NULL,
                                          compare_series = NULL) {

        # Convert wider format for comparing
        tsbl_series <- tsbl_variable %>%
          dplyr::select(tidyselect::all_of(
            c(date_field, id_field, value_field)
          )) %>%
          tidyr::pivot_wider(
            names_from = tidyselect::all_of(id_field),
            values_from = tidyselect::all_of(value_field),
            id_cols = tidyselect::all_of(date_field)
          )

        # Filter series for comparing
        series <- c(base_series, compare_series)
        if (isTruthy(series)) {
          if (all(series %in% names(tsbl_series))) {
            tsbl_series <- tsbl_series %>%
              dplyr::select(tidyselect::all_of(
                c(date_field, series)
              )) %>%
              tsibble::as_tsibble(index = {{ date_field }})
          } else {
            tsbl_series <- NULL
          }
        }

        # Output compare pair plots by ggmatrix
        if (length(tsbl_series) > 0) {
          tbl_compare_pair <- tibble::tibble(
            base_series = base_series,
            compare_series = compare_series
          )


          # Function to plot Rolling correlation for a pair of series
          compare_pair_plot <- function(tsbl_series,
                                        base_series,
                                        compare_series,
                                        title = "Comparing pairs") {
            p <- tsbl_series %>%
              fabletools::autoplot(.data[[compare_series]],
                color = "black"
              ) +
              fabletools::autolayer(tsbl_series,
                .data[[base_series]],
                color = "darkgrey",
                linetype = "longdash"
              )
            p + ggplot2::labs(title = title)
          }

          # Create individual plots in batch
          tbl_compare_pair <- tbl_compare_pair %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              compare_pair_plot = list(
                compare_pair_plot(
                  tsbl_series, .data$base_series, .data$compare_series,
                  title = glue::glue("{.data$compare_series} vs {.data$base_series}")
                )
              )
            )

          # Combine small plots into ggmatrix
          p <- GGally::ggmatrix(tbl_compare_pair$compare_pair_plot,
            ncol = 1,
            nrow = length(tbl_compare_pair$compare_pair_plot),
            xAxisLabels = glue::glue("Compare series with {tbl_compare_pair$base_series}"),
            yAxisLabels = glue::glue("{tbl_compare_pair$compare_series} vs {tbl_compare_pair$base_series}"),
            switch = "y"
          )

          p
        }
      }

      # Main function

      # Generate cross correlation plot
      p <- generate_cross_cor_plot(
        pairs_compare_pair_plot,
        "Comparing series in pair"
      )

      p
    })
  })
}

#' Testing module app of ts_feat_cor_tidyverts
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn ts_feat_cor_tidyverts  Testing App of ts_feat_cor_tidyverts.
ts_feat_cor_tidyverts_app <- function(use_online_data = FALSE) {

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
    ts_feat_cor_tidyverts_ui("ts_feat_cor_tidyverts_module")
  )
  server <- function(input, output, session) {
    ts_feat_cor_tidyverts_server(
      "ts_feat_cor_tidyverts_module",
      tsbl_vars = reactive(tsbl_vars),
      tsbl_vars_average = reactive(tsbl_vars_average)
    )
  }
  shinyApp(ui, server)
}
