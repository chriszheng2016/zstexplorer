#' load_data
#'
#' @description A shiny module for load_data.
#'
#' @details
#'  The module is an UI for user to ...
#'
#' @name load_data
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   load_data_ui("load_data_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   load_data <- load_data_server("load_data_module")
#' }
#'
#' # Run testing App for integration testing
#' load_data_app()
#' }
#'
NULL



#' UI function of load_data
#'
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn load_data  UI function of load_data.
#' @importFrom shiny NS tagList
load_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    # tabsetPanel(
    shinydashboard::tabBox(
      id = ns("tabset_load"),
      type = "tabs",
      width = 12,
      tabPanel(
        "Load variables",
        fluidRow(
          column(
            9,
            selectInput(
              inputId = ns("vars_type"),
              label = strong("Variable type:"),
              choices = c("factor", "indicator"),
              width = "100%"
            ),
            selectInput(
              inputId = ns("vars_groups"),
              label = strong("Varaible groups:"),
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              width = "100%"
            ),
            textInput(
              inputId = ns("select_vars"),
              label = strong("Selected varables:"),
              width = "100%"
            )
          ),
          column(
            3,
            br(),
            wellPanel(
              actionButton(
                inputId = ns("clear_vars"),
                label = strong("Clear selection"),
                width = "100%"
              ),
              br(),
              br(),
              actionButton(
                inputId = ns("load_vars"),
                label = strong("Load variables"),
                width = "100%"
              )
            )
          )
        ),

        # Output: Tabset of output tables
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Varables info",
            DT::dataTableOutput(outputId = ns("vars_info_table"))
          ),
          tabPanel(
            "Loaded varables",

            fluidRow(
              column(
                6,
                selectInput(
                  inputId = ns("select_vars_dataset"),
                  label = strong("Select dataset:"),
                  choices = NULL,
                  selected = NULL,
                  width = "100%"
                ),
              ),
              column(
                6,
                br(),
                actionButton(
                  inputId = ns("remove_vars_dataset"),
                  label = strong("Remove selected dataset"),
                  width = "45%"
                ),
                actionButton(
                  inputId = ns("clear_all_vars_dataset"),
                  label = strong("Clear all datasets"),
                  width = "45%"
                )
              ),
            ),
            DT::dataTableOutput(outputId = ns("vars_data_table"))
          )
        )
      ),

      tabPanel(
        "Output data",
        fluidRow(
          column(
            9,
            fluidRow(
              column(
                6,
                uiOutput(outputId = ns("output_date_range_text")),
                wellPanel(
                  fluidRow(
                    column(
                      3,
                      checkboxInput(
                        inputId = ns("output_all_dates"),
                        label = strong("All dates"),
                        value = FALSE
                      ),
                    ),
                    column(
                      9,
                      dateRangeInput(
                        inputId = ns("output_date_range"),
                        label = strong("Select range:"),
                        start = Sys.Date() - 365,
                        end = Sys.Date(),
                        width = "100%"
                      ),
                    ),
                  ),
                ),
              ),
              column(
                6,
                h4("Period of output"),
                wellPanel(
                  selectInput(
                    inputId = ns("output_unified_period"),
                    label = strong("Unified period:"),
                    choices = c("day", "month", "quarter", "year"),
                    selected = "quarter",
                    width = "100%"
                  ),
                ),
              )
            ),
            h4("Setting for datasets for output"),
            wellPanel(
              fluidRow(
                column(
                  9,
                  textInput(
                    inputId = ns("dataset_setting_name"),
                    label = strong("Dataset name:"),
                    value = "",
                    width = "100%"
                  ),
                  fluidRow(
                    column(
                      6,
                      checkboxInput(
                        inputId = ns("dataset_setting_enable_output"),
                        label = strong("Enable output"),
                        value = TRUE,
                        width = "100%"
                      ),
                      sliderInput(
                        inputId = ns("dataset_setting_lag_unfied_periods"),
                        label = strong("Lag unfied periods:"),
                        min = -12,
                        max = 12, value = 0,
                        width = "100%"
                      ),
                    ),
                    column(
                      6,
                      selectInput(
                        inputId = ns("dataset_setting_down_sample_method"),
                        label = strong("down-sample(High freq to low freq) method:"),
                        choices = NULL,
                        selected = NULL,
                        width = "100%"
                      ),

                      selectInput(
                        inputId = ns("dataset_setting_up_sample_method"),
                        label = strong("up-sample(low freq to high freq) method:"),
                        choices = NULL,
                        selected = NULL,
                        width = "100%"
                      )
                    ),
                  ),
                ),
                column(
                  3,
                  wellPanel(
                    actionButton(
                      inputId = ns("modify_dataset_setting"),
                      label = strong("Modify dataset setting "),
                      width = "100%"
                    ),
                    br(),
                    br(),
                    actionButton(
                      inputId = ns("reset_dataset_setting"),
                      label = strong("Reset dataset setting"),
                      width = "100%"
                    ),
                    br(),
                    br(),
                    actionButton(
                      inputId = ns("reset_all_dataset_setting"),
                      label = strong("Reset settings of all datasets"),
                      width = "100%"
                    ),
                    br(),
                    br(),
                    actionButton(
                      inputId = ns("preview_dataset_output"),
                      label = strong("Preview dataset output"),
                      width = "100%"
                    ),
                  ),
                ),
              ),
            ),
          ),
          column(
            3,
            br(),
            wellPanel(
              actionButton(
                inputId = ns("output_data"),
                label = strong("Output data"),
                width = "100%"
              ),
              br(),
              br(),
              downloadButton(
                outputId = ns("download_data"),
                label = "Download Data",
                style = "width: 100%"
              )
            )
          )
        ),

        # Output: Tabset of output tables
        tabsetPanel(
          id = ns("output_data_tabs"),
          tabPanel(
            "Dataset setting",
            DT::dataTableOutput(outputId = ns("output_dataset_setting_table"))
          ),
          tabPanel(
            "Preview ouput dataset",
            DT::dataTableOutput(outputId = ns("preview_dataset_output_table"))
          ),
          tabPanel(
            "Output data",
            DT::dataTableOutput(outputId = ns("all_dataset_output_table"))
          )
        )
      )
    )
  )
}

#' Server function of load_data
#'
#'
#' @describeIn load_data  Server function of load_data.
#' @return * Server function return a data frame of ...
load_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Logical reactive ----

    # >> Load_data ----

    # Repository of load data
    load_data <- reactiveValues(
      vars_dataset = NULL,
      vars_dataset_seq_number = 0
    )

    # Add a new dataset with default output setting into load_data
    add_dataset <- function(load_vars) {

      # Build a new dataset with default output setting for load_vars
      load_data$vars_dataset_seq_number <- load_data$vars_dataset_seq_number + 1
      default_output_setting <- default_output_setting()
      new_vars_dataset <- load_vars %>%
        dplyr::group_by(.data$period) %>%
        tidyr::nest() %>%
        dplyr::mutate(
          name = glue::glue("dataset{load_data$vars_dataset_seq_number}-{.data$period}"),
          enable_output = default_output_setting$enable_output,
          lag_unfied_periods = default_output_setting$lag_unfied_periods,
          down_sample_method = default_output_setting$down_sample_method,
          up_sample_method = default_output_setting$up_sample_method
        ) %>%
        dplyr::select(c("name", "period"), dplyr::everything())

      # Add the new dataset into dataset of load_data
      if (is.null(load_data$vars_dataset)) {
        load_data$vars_dataset <- new_vars_dataset
      } else {
        load_data$vars_dataset <- load_data$vars_dataset %>%
          dplyr::bind_rows(new_vars_dataset)
      }
    }

    # Modify output setting of specified vars_dataset in load_data
    modify_dataset <- function(dataset_name,
                               enable_output = TRUE,
                               lag_unfied_periods = 0,
                               down_sample_method = "",
                               up_sample_method = "") {
      matched <- load_data$vars_dataset$name == dataset_name
      if (any(matched)) {
        load_data$vars_dataset[matched, "enable_output"] <- enable_output
        load_data$vars_dataset[matched, "lag_unfied_periods"] <- lag_unfied_periods
        load_data$vars_dataset[matched, "down_sample_method"] <- down_sample_method
        load_data$vars_dataset[matched, "up_sample_method"] <- up_sample_method
      }
    }

    # Reset output setting of specified vars_dataset in load_data
    reset_dataset <- function(dataset_name) {
      default_output_setting <- default_output_setting()
      modify_dataset(dataset_name,
        enable_output = default_output_setting$enable_output,
        lag_unfied_periods = default_output_setting$lag_unfied_periods,
        down_sample_method = default_output_setting$down_sample_method,
        up_sample_method = default_output_setting$up_sample_method
      )
    }

    # Reset output setting of all vars_dataset in load_data
    reset_all_dataset <- function() {
      dataset_names <- load_data$vars_dataset$name
      dataset_names %>%
        purrr::iwalk(.f = ~ reset_dataset(.x))
    }

    # Remove specified vars_dataset in load_data
    remove_dataset <- function(dataset_name) {
      load_data$vars_dataset <- load_data$vars_dataset %>%
        dplyr::filter(.data$name != dataset_name)
    }

    # Remove all vars_dataset in load_data
    remove_all_dataset <- function() {
      load_data$vars_dataset <- load_data$vars_dataset %>%
        dplyr::filter(.data$name == NA)
    }

    # >> Load vars ----

    vars_info <- reactive({
      stock_db <- stock_db()
      switch(input$vars_type,
        "factor" = {
          zstmodelr::get_factors_info(stock_db)
        },
        "indicator" = {
          zstmodelr::get_indicators_info(stock_db)
        }
      )
    })

    stock_info <- reactive({
      stock_db <- stock_db()
      zstmodelr::get_stock_info(stock_db)
    })

    vars_info_in_group <- reactive({
      if (!is.null(input$vars_groups) && (input$vars_groups != "")) {
        vars_info() %>%
          dplyr::filter(.data[[group_var()]] %in% input$vars_groups)
      } else {
        vars_info()
      }
    })


    id_var <- reactive({
      switch(input$vars_type,
        "factor" = {
          "factor_code"
        },
        "indicator" = {
          "ind_code"
        }
      )
    })

    group_var <- reactive({
      switch(input$vars_type,
        "factor" = {
          "factor_group"
        },
        "indicator" = {
          "ind_category"
        }
      )
    })

    var_groups <- reactive({
      vars_info() %>%
        dplyr::select(group_var()) %>%
        dplyr::pull(group_var()) %>%
        unique() %>%
        sort()
    })

    # Load variables
    load_vars <- eventReactive(input$load_vars, {
      vars_list <- stringr::str_split(req(input$select_vars),
        pattern = "\\s*,\\s*|\\s+"
      )[[1]]

      withProgress(message = "Load vars data", value = 0, {
        incProgress(message = "connect stock_db")
        stock_db <- stock_db()

        # Get factors from database
        incProgress(message = "get data..")
        tbl_vars <-
          switch(req(input$vars_type),
            "factor" = {
              zstmodelr::get_factors(stock_db, factor_codes = vars_list) %>%
                tidyr::pivot_wider(
                  names_from = "factor_code",
                  values_from = "factor_value"
                )
            },
            "indicator" = {
              zstmodelr::get_indicators(stock_db,
                indicator_codes = vars_list
              ) %>%
                tidyr::pivot_wider(
                  names_from = "ind_code",
                  values_from = "ind_value"
                )
            }
          )

        incProgress(message = "convert to tsibble")

        if (!("indcd" %in% names(tbl_vars))) {
          if ("stkcd" %in% names(tbl_vars)) {
            tbl_vars <- tbl_vars %>%
              dplyr::left_join(stock_info(), by = "stkcd") %>%
              dplyr::select(c(names(tbl_vars), "indcd"))
          } else {
            tbl_vars <- tbl_vars %>%
              dplyr::mutate(indcd = "NA")
          }
        }

        if (!("stkcd" %in% names(tbl_vars))) {
          tbl_vars <- tbl_vars %>%
            dplyr::mutate(stkcd = "NA")
        }

        # Turn into tsibble
        tsbl_vars <-
          tbl_vars %>%
          dplyr::select(c("date", "period", "stkcd", "indcd"), everything()) %>%
          dplyr::filter(!(is.na(.data[["date"]]))
          && (!is.na(.data[["stkcd"]]))
          && (!is.na(.data[["indcd"]]))) %>%
          tsibble::as_tsibble(
            index = date,
            key = c("period", "stkcd")
          )
      })

      showNotification("Load variables successfully.")

      return(tsbl_vars)
    })

    # Aggregate load_vars() into load_data
    observeEvent(load_vars(), {
      # Add new loading vars into dataset of load_data
      add_dataset(load_vars())
    })

    # >> Output data ----

    # Default output setting of vars_dataset
    default_output_setting <- reactive({
      list(
        enable_output = TRUE,
        lag_unfied_periods = 0,
        down_sample_method = available_down_sample_methods()["Accumulated Return"],
        up_sample_method = available_up_sample_methods()["None"]
      )
    })

    # Available choices for down_sample_method
    available_down_sample_methods <- reactive({
      methods <- c(
        "None" = "",
        "Accumulated Return" = "~(prod(1 + .x, na.rm = TRUE) - 1)",
        "Mean" = "~(mean(1 + .x, na.rm = TRUE) - 1)",
        "Median" = "~(media(1 + .x, na.rm = TRUE) - 1)",
        "Sum" = "~(sum(1 + .x, na.rm = TRUE) - 1)"
      )

      methods
    })

    # Available choices for up_sample_method
    available_up_sample_methods <- reactive({
      methods <- c(
        "None" = ""
      )

      methods
    })

    # Date range choosed by user
    select_date_range <- reactive({
      start_date <- input$output_date_range[1]
      end_date <- input$output_date_range[2]


      date_unit <- input$output_unified_period
      start_date <- lubridate::floor_date(start_date, unit = date_unit)
      end_date <- lubridate::ceiling_date(end_date, unit = date_unit) - 1

      date_range <- list(
        start_date = start_date,
        end_date = end_date
      )

      date_range
    })

    # User selected setting in output_dataset_setting_table
    select_dataset_setting <- eventReactive(
      input$output_dataset_setting_table_cell_clicked,
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
      {
        select_setting <- NULL

        key_var <- c("name")
        select_key <- user_select_data(
          DT_tableId = "output_dataset_setting_table",
          tbl_data = load_data$vars_dataset,
          output_columns = key_var
        )

        select_setting <- load_data$vars_dataset %>%
          dplyr::filter(.data$name %in% select_key[[key_var]])

        select_setting
      }
    )

    # Function to build output data by processing datasets of vars
    build_output_data <- function(vars_dataset,
                                  unified_period) {

      # Function to unify period of dataset change frequency of timeseries
      .unify_data_period <- function(data,
                                     current_period,
                                     new_period,
                                     down_sample_method = "",
                                     up_sample_method = "") {
        period_level <- c("day", "month", "quarter", "year")
        current_period <- factor(current_period,
          levels = period_level,
          ordered = TRUE
        )
        new_period <- factor(new_period,
          levels = period_level,
          ordered = TRUE
        )

        if (current_period != new_period) {
          date_var <- tsibble::index_var(data)
          key_vars <- tsibble::key_vars(data)

          if (current_period < new_period) {
            # Transform from high freq to low freq
            agg_fun <- if (down_sample_method == "") {
              NULL
            } else {
              rlang::parse_expr(down_sample_method)
            }

            result <- zstmodelr::ts_resample(
              ts_dataset = data,
              freq_rule = as.character(new_period),
              fillna_method = "nfill",
              agg_fun = agg_fun,
              date_index_field = date_var,
              key_fields = key_vars,
              parallel = TRUE
            )
          } else {
            # Transform from low freq to high freq
            result <- zstmodelr::ts_asfreq(
              ts_dataset = data,
              freq_rule = as.character(new_period),
              fillna_method = "ffill",
              date_index_field = date_var,
              key_fields = key_vars,
              parallel = TRUE
            )
          }

          result <- result %>%
            dplyr::mutate(period = new_period) %>%
            tsibble::as_tsibble(index = date_var, key = key_vars)
        } else {
          # Don't transform for same period
          result <- data
        }

        return(result)
      }

      # Function to lag dataset with specified periods
      .lag_data_period <- function(data, lag_periods) {
        date_var <- tsibble::index_var(data)
        key_vars <- tsibble::key_vars(data)

        if (lag_periods != 0) {
          result <- zstmodelr::ts_lag(
            ts_dataset = data,
            k = lag_periods,
            date_index_field = date_var,
            key_fields = key_vars,
            parallel = TRUE
          ) %>%
            tsibble::as_tsibble(key = key_vars, index = date_var)
        } else {
          result <- data
        }

        result
      }

      # Function to combine a list of dataset into one dataset
      .combine_data_list <- function(data_list) {

        # List of by vars to 'by' argument of full_join
        by_vars_list <- purrr::map(
          data_list,
          .f = function(.x) {
            date_var <- tsibble::index_var(.x)
            key_vars <- tsibble::key_vars(.x)
            if (all(.x[key_vars] == "NA")) {
              by_vars <- date_var
            } else {
              by_vars <- c(date_var, key_vars)
            }
          }
        )

        # It should be less than data_list by one element according to reduce2
        by_vars_list <- by_vars_list[-1]

        # Reduce data_list into one dataset
        result <- purrr::reduce2(
          data_list,
          by_vars_list, # to 'by' argument full_join
          .f = dplyr::inner_join,
          suffix = c("", ".y")
        ) %>%
          dplyr::select(-tidyselect::contains(".y"))

        return(result)
      }

      # Main function

      output_data <- vars_dataset

      withProgress(
        message = "Build output for loaded variables",
        value = 0,
        {

          # Unify datasets by same period
          incProgress(
            message = glue::glue("Unify datasets by period of {unified_period}..")
          )
          output_data <- output_data %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              data = list(.unify_data_period(
                .data$data,
                current_period = .data$period,
                new_period = unified_period,
                down_sample_method = .data$down_sample_method,
                up_sample_method = .data$up_sample_method
              )),
              period = unified_period
            ) %>%
            dplyr::ungroup()

          # Lag datasets if needed
          incProgress(
            message = glue::glue("Lag unfied dataset by specified periods..")
          )
          output_data <- output_data %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              data = list(.lag_data_period(
                .data$data,
                lag_periods = .data$lag_unfied_periods
              ))
            ) %>%
            dplyr::ungroup()


          # Combine datasets with unified period into one dataset
          incProgress(
            message = glue::glue("Combine datasets with unified period..")
          )
          output_data <- output_data %>%
            dplyr::summarise(
              data = list(.combine_data_list(.data$data))
            )

          # Build output tsibble
          incProgress(
            message = glue::glue("Convert into tsibble..")
          )

          # Down-stream analysis need a tsibble with index of "date",
          # and key vars of "stkcd" and "period" as input
          tsbl_output_data <- purrr::pluck(output_data$data, 1) %>%
            dplyr::mutate(period = unified_period) %>%
            dplyr::select(c("date", "period"), dplyr::everything()) %>%
            tsibble::as_tsibble(index = "date", key = c("stkcd", "period"))
        }
      )

      showNotification("Build output for loaded variables successfully.")

      return(tsbl_output_data)
    }

    # Preview output of selected dataset of vars
    preview_dataset_output <- eventReactive(input$preview_dataset_output, {
      vars_dataset <- load_data$vars_dataset

      # Only output selected dataset
      req(input$dataset_setting_name)
      vars_dataset <- vars_dataset %>%
        dplyr::filter(.data$name == input$dataset_setting_name)

      # Filter out_data by date range if needed
      if (!input$output_all_dates) {
        start_date <- select_date_range()$start_date
        end_date <- select_date_range()$end_date
        vars_dataset <- vars_dataset %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            data = list(dplyr::filter(
              .data$data,
              .data$date >= start_date,
              .data$date <= end_date
            ))
          ) %>%
          dplyr::ungroup()
      }

      # Filter output by some sample stocks to accelerate building results
      vars_dataset <- vars_dataset %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          data = list(dplyr::filter(
            .data$data,
            .data$stkcd %in% sample(.data$stkcd, 1)
          ))
        ) %>%
        dplyr::ungroup()

      output_data <- build_output_data(
        vars_dataset,
        unified_period = input$output_unified_period
      )

      save_debug_data(output_data, output_file = "preview_dataset_output")

      return(output_data)
    })

    # Output data of all datasets of vars
    all_dataset_output <- eventReactive(input$output_data, {
      vars_dataset <- load_data$vars_dataset

      # Only output enabled datasets
      vars_dataset <- vars_dataset %>%
        dplyr::filter(.data$enable_output == TRUE)

      # Filter out_data by date range if needed
      if (!input$output_all_dates) {
        start_date <- select_date_range()$start_date
        end_date <- select_date_range()$end_date
        vars_dataset <- vars_dataset %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            data = list(dplyr::filter(
              .data$data,
              .data$date >= start_date,
              .data$date <= end_date
            ))
          ) %>%
          dplyr::ungroup()
      }

      output_data <- build_output_data(
        vars_dataset,
        unified_period = input$output_unified_period
      )

     save_debug_data(output_data, output_file = "all_dataset_output")

      return(output_data)
    })

    # Controls interaction ----

    # >> Load vars ----

    # Update UI by user input in load variables
    observe({

      # Update choices of data_group
      updateSelectizeInput(session,
        inputId = "vars_groups",
        choices = var_groups()
      )

      if (NROW(load_data$vars_dataset) > 0) {
        updateSelectizeInput(session,
          inputId = "select_vars_dataset",
          choices = load_data$vars_dataset$name
        )
      } else {
        updateSelectizeInput(session,
          inputId = "select_vars_dataset",
          choices = ""
        )
      }
    })


    # Select codes from vars_info_table
    observeEvent(input$vars_info_table_cell_clicked, ignoreInit = TRUE, {
      if (length(input$vars_info_table_cell_clicked) > 0) {
        select_codes <- user_select_data(
          DT_tableId = "vars_info_table",
          tbl_data = vars_info_in_group(),
          output_columns = id_var()
        ) %>% dplyr::pull()

        updateTextInput(
          session = session,
          inputId = "select_vars",
          value = select_codes
        )
      }
    })

    # Get user's selecting data by mapping selection of DT table to data table
    user_select_data <- function(DT_tableId, tbl_data, output_columns) {
      select_index <- input[[glue::glue("{DT_tableId}_rows_selected")]]
      select_data <- tbl_data[select_index, output_columns]

      select_data
    }

    # Clear user select codes
    clear_select_codes <- function() {

      # Select codes input
      updateTextInput(
        session = session,
        inputId = "select_vars",
        value = ""
      )

      # Clear selections in tables
      dt_table <- "vars_info_table"
      proxy <- DT::dataTableProxy(dt_table)
      DT::selectRows(proxy, selected = NULL)
    }

    # Clear selections in info table
    observeEvent(input$clear_vars, ignoreInit = TRUE, {
      clear_select_codes()
    })

    # Change vars_type
    observeEvent(input$vars_type, ignoreInit = TRUE, {
      # Clear selection when changing vars_type
      clear_select_codes()
    })

    # Remove selected dataset of loading vars
    observeEvent(input$remove_vars_dataset, ignoreInit = TRUE, {
      remove_dataset(input$select_vars_dataset)
    })

    # Remove all datasets of loading vars
    observeEvent(input$clear_all_vars_dataset, ignoreInit = TRUE, {
      remove_all_dataset()
    })

    # >> Output data ----

    # Update UI by user input in load variables
    observe({

      # Set control values of output setting of vars_dataset
      if (NROW(select_dataset_setting()) > 0) {
        # Set values of output setting for selected vars_dataset

        updateTextInput(session,
          inputId = "dataset_setting_name",
          value = select_dataset_setting()$name
        )

        updateCheckboxInput(session,
          inputId = "dataset_setting_enable_ouput",
          value = select_dataset_setting()$enable_output
        )

        updateSliderInput(session,
          inputId = "dataset_setting_lag_unfied_periods",
          value = select_dataset_setting()$lag_unfied_periods
        )

        updateSelectizeInput(session,
          inputId = "dataset_setting_down_sample_method",
          choices = available_down_sample_methods(),
          selected = select_dataset_setting()$down_sample_method
        )

        updateSelectizeInput(session,
          inputId = "dataset_setting_up_sample_method",
          choices = available_up_sample_methods(),
          selected = select_dataset_setting()$up_sample_method
        )
      } else {

        # Set default value for non-selected vars_dataset
        updateTextInput(session,
          inputId = "dataset_setting_name",
          value = ""
        )

        updateCheckboxInput(session,
          inputId = "dataset_setting_enable_ouput",
          value = default_output_setting()$enable_ouput
        )

        updateSliderInput(session,
          inputId = "dataset_setting_lag_unfied_periods",
          value = default_output_setting()$lag_unfied_periods
        )

        updateSelectizeInput(session,
          inputId = "dataset_setting_down_sample_method",
          choices = available_down_sample_methods(),
          selected = default_output_setting()$down_sample_method
        )

        updateSelectizeInput(session,
          inputId = "dataset_setting_up_sample_method",
          choices = available_up_sample_methods(),
          selected = default_output_setting()$up_sample_method
        )
      }
    })

    # Enable/disable out_date_range by output_data_data
    observeEvent(input$output_all_dates, {
      shinyjs::toggleState(
        id = "output_date_range",
        condition = input$output_all_dates == FALSE
      )
      # # enable the download button
      # shinyjs::enable("download_data")
    })

    # Modify setting of selected vars_dataset
    observeEvent(input$modify_dataset_setting, ignoreInit = TRUE, {
      modify_dataset(
        input$dataset_setting_name,
        input$dataset_setting_enable_output,
        input$dataset_setting_lag_unfied_periods,
        input$dataset_setting_down_sample_method,
        input$dataset_setting_up_sample_method
      )
    })

    # Reset setting of selected vars_dataset
    observeEvent(input$reset_dataset_setting, ignoreInit = TRUE, {
      reset_dataset(input$dataset_setting_name)
    })

    # Reset setting of all vars_dataset
    observeEvent(input$reset_all_dataset_setting, ignoreInit = TRUE, {
      reset_all_dataset()
    })

    # Active tab to show preview dataset output
    observeEvent(input$preview_dataset_output, ignoreInit = TRUE, {
      updateTabsetPanel(
        session,
        inputId = "output_data_tabs",
        selected = "Preview ouput dataset"
      )
    })

    # Active tab to show output data
    observeEvent(input$output_data, ignoreInit = TRUE, {
      updateTabsetPanel(
        session,
        inputId = "output_data_tabs",
        selected = "Output data"
      )
    })

    # Enable/Disable download_data button
    observe({
      # Enable download_data button if output data is available
      shinyjs::disable("download_data")
      if (NROW(all_dataset_output()) > 0) {
        shinyjs::enable("download_data")
      } else {
        shinyjs::disable("download_data")
      }
    })

    # Output UI -----

    # >> Load vars  ----

    # Display vars Info
    output$vars_info_table <- DT::renderDataTable({
      DT::datatable(
        vars_info_in_group(),
        filter = "top",
        extensions = "Scroller",
        options = list(
          columnDefs = list(list(className = "dt-center")),
          pageLength = 20,
          dom = "ltir",
          deferRender = TRUE,
          scrollY = 350,
          scrollX = TRUE,
          scroller = TRUE
        )
      )
    })

    output$vars_data_table <- DT::renderDataTable({
      req(input$select_vars_dataset)
      req(NROW(load_data$vars_dataset) > 0)

      # Show selected dataset of loading data
      focus_load_vars <- load_data$vars_dataset %>%
        dplyr::filter(.data$name == input$select_vars_dataset)

      # Prompt user to choose a dataset for show
      validate(
        need(NROW(focus_load_vars) > 0,
          message = "Please choose a dataset to show."
        )
      )

      # Display select dataset
      vars_period <- purrr::pluck(focus_load_vars$period, 1)
      vars_data <- purrr::pluck(focus_load_vars$data, 1) %>%
        dplyr::mutate(period = vars_period) %>%
        dplyr::select(
          c(tsibble::index(.), tsibble::key_vars(.), "period"),
          dplyr::everything()
        )

      numeric_vars <- zstmodelr::expect_type_fields(
        vars_data,
        expect_type = "numeric"
      )

      DT::datatable(
        vars_data,
        filter = "top",
        extensions = "Scroller",
        options = list(
          columnDefs = list(list(className = "dt-center")),
          pageLength = 20,
          dom = "ltir",
          deferRender = TRUE,
          scrollY = 350,
          scrollX = TRUE,
          scroller = TRUE
        )
      ) %>%
        DT::formatRound(columns = numeric_vars, digits = 2)
    })

    # >> Output data  ----

    output$output_date_range_text <- renderUI({
      start_date <- select_date_range()$start_date
      end_date <- select_date_range()$end_date

      if (input$output_all_dates) {
        msg_text <- glue::glue("Date range: all dates")
      } else {
        msg_text <- glue::glue("Date range: {start_date} to {end_date}")
      }

      msg_text

      tagList(
        h4(msg_text)
      )
    })

    output$output_dataset_setting_table <- DT::renderDataTable({
      req(load_data$vars_dataset)
      vars_dataset_setting <- load_data$vars_dataset %>%
        dplyr::select(-c("data"))

      DT::datatable(
        vars_dataset_setting,
        filter = "top",
        selection = list(mode = "single", selected = 1, target = "row"),
        extensions = "Scroller",
        options = list(
          columnDefs = list(list(className = "dt-center")),
          pageLength = 20,
          dom = "ltir",
          deferRender = TRUE,
          scrollY = 350,
          scrollX = TRUE,
          scroller = TRUE
        )
      )
    })

    output$preview_dataset_output_table <- DT::renderDataTable({
      show_data <- preview_dataset_output()

      # Prompt user to choose a dataset for show
      validate(
        need(NROW(show_data) > 0,
          message = "No dataset to preview."
        )
      )

      numeric_vars <- zstmodelr::expect_type_fields(
        show_data,
        expect_type = "numeric"
      )

      DT::datatable(
        show_data,
        filter = "top",
        extensions = "Scroller",
        options = list(
          columnDefs = list(list(className = "dt-center")),
          pageLength = 20,
          dom = "ltir",
          deferRender = TRUE,
          scrollY = 350,
          scrollX = TRUE,
          scroller = TRUE
        )
      ) %>%
        DT::formatRound(columns = numeric_vars, digits = 2)
    })

    output$all_dataset_output_table <- DT::renderDataTable({
      show_data <- all_dataset_output()

      numeric_vars <- zstmodelr::expect_type_fields(
        show_data,
        expect_type = "numeric"
      )

      DT::datatable(
        show_data,
        filter = "top",
        extensions = "Scroller",
        options = list(
          columnDefs = list(list(className = "dt-center")),
          pageLength = 20,
          dom = "ltir",
          deferRender = TRUE,
          scrollY = 350,
          scrollX = TRUE,
          scroller = TRUE
        )
      ) %>%
        DT::formatRound(columns = numeric_vars, digits = 2)
    })

    output$download_data <- downloadHandler(
      filename = function() {
        glue::glue({
          "output_data({Sys.Date()}).csv"
        })
      },
      content = function(file) {
        # Save output data in output file
        output_data <- all_dataset_output()
        readr::write_csv(output_data, file = file)
      }
    )

    return(all_dataset_output)
  })
}

#' Testing module app of load_data
#'
#' @describeIn load_data  Testing App of load_data.
load_data_app <- function() {
  zstmodelr::enable_parallel()

  ui <- fluidPage(
    load_data_ui("load_data_module")
  )
  server <- function(input, output, session) {
    load_data <- load_data_server("load_data_module")
  }
  shinyApp(ui, server)
}
