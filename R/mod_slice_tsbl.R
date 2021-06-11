# Module functions --------------------------------------------------------

#' Slice tibble of time series by user inputs
#'
#' @description A shiny module to slice tibble of time series(tsibble)
#'  by user's filter.
#' conditions.
#'
#' @details
#'  The module is an UI for user to choose some conditions for slice(filter)
#'  tibble of time series.
#'
#'  User will promote to use following filter conditions to filter:
#'
#'  * **Industry codes**: one or many industry codes.
#'
#'  * **Stock codes**: one or many stock codes.
#'
#'  * **Variable coes**:  one or many variables to focus.
#'
#'  * **Period**: Period type of time series, such as "quarter", "month",
#'   "week", etc.
#'
#'  * **Date range**:
#'
#'    * Single Period:  specify report date.
#'
#'    * Multiple Periods: specify start/end date.
#'
#'  * **Industry average**: options for computing industry average.
#'
#'    * Average by: a variable used to compute industry average.
#'
#'    * Average method: use mean or median to compute industry average.
#'
#'
#'
#'
#' @name slice_tsbl
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#' @param tsbl_vars A tsibble of time series to be sliced.
#' @param debug A logic to enable/disable output for debug. Default FALSE
#'  means to disable output of debug.
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   slice_tsbl_ui("slice_tsbl_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'
#'   # Slice tsibble of variables by timeseries and show result in debug mode
#'   slice_result <- slice_tsbl_server("slice_tsbl_module",
#'     tsbl_vars = reactive(tsbl_vars),
#'     slice_type = "time_series",
#'     debug = TRUE
#'     )
#'
#'  # A tsibble of variables sliced by user
#'  slice_tsbl_vars <- slice_result$slice_vars
#'
#'  # A tsibble of industry average of variables sliced by user
#'  slice_vars_average <-  slice_result$slice_vars_average

#' }
#'
#' # Run testing App for integration testing
#' slice_tsbl_app( use_online_data = FALSE,
#'                 slice_type = "time_series",
#                  debug = TRUE
#       )
#' }
#'
NULL

#' UI function of slice_tsbl
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn slice_tsbl  UI function of slicing tibble of time series.

#' @importFrom shiny NS tagList
slice_tsbl_ui <- function(id, debug = FALSE) {
  ns <- NS(id)
  tagList(

    selectInput(
      inputId = ns("indcd"),
      label = strong("Industry codes:"),
      choices = "", multiple = TRUE
    ),

    selectInput(
      inputId = ns("stkcd"),
      label = strong("Stock codes:"),
      choices = "", multiple = TRUE
    ),

    selectInput(
      inputId = ns("focus_vars"),
      label = strong("Variable codes:"),
      choices = "", multiple = TRUE
    ),

    selectInput(
      inputId = ns("period"),
      label = strong("Period:"),
      choices = "", multiple = TRUE
    ),

    radioButtons(
      inputId = ns("date_type"),
      label = strong("Date range"),
      choices = list(
        "Single Period" = "single_period",
        "Multiple Periods" = "multi_period"
      ),
      selected = "single_period"
    ),

    conditionalPanel(
      condition = "input.date_type == 'single_period'",
      sliderInput(
        inputId = ns("report_date"),
        label = strong("Report Date:"),
        min = 0,
        max = 0,
        value = 0,
        step = NULL,
        timeFormat = "%F"
      ),
      ns = ns
    ),

    conditionalPanel(
      condition = "input.date_type == 'multi_period'",
      selectInput(
        inputId = ns("start_date"),
        label = strong("Start date:"),
        choices = ""
      ),
      selectInput(
        inputId = ns("end_date"),
        label = strong("End date:"),
        choices = ""
      ),
      ns = ns
    ),


    h4("Industry average"),

    selectInput(
      inputId = ns("average_by"),
      label = strong("Average by:"),
      choices = ""
    ),

    radioButtons(
      inputId = ns("average_method"),
      label = strong("Average method:"),
      choices = list(
        "Mean" = "mean",
        "Median" = "median"
      ),
      selected = "median",
      inline = TRUE
    ),


    h5("Please set filter and click Apply to slice data for analyzing"),

    actionButton(
      inputId = ns("apply_filter"),
      label = strong("Apply"),
      width = "100%"
    ),

    br(),
    textOutput(
      outputId = ns("status_text")
    ),

    br(),
    # Debug Control for output
    if (debug) {
      tabsetPanel(
        id = ns("debug_output"),
        type = "tabs",
        tabPanel(
          "sliced vars",
          DT::dataTableOutput(outputId = ns("debug_output_slice_vars"))
        ),
        tabPanel(
          "Sliced vars average",
          DT::dataTableOutput(outputId = ns("debug_output_slice_vars_average"))
        )
      )
    }
  )
}

#' Server Function of slice_tsbl
#'
#' @param  slice_type  A character indicator about how to slice data, e.g.,
#'  "cross_section", "time_series". Default is cross_section.
#'
#' @return
#' * Server function return a list of tsibbles sliced by
#'   user's filter conditions, which contains following fields:
#'
#'    + slice_vars : a tsibble of variables sliced by user.
#'
#'    + slice_vars_average: a tsibble of average of variables sliced by user.
#'
#'
#' @describeIn slice_tsbl  Server function of slicing tibble of time series.
#'
slice_tsbl_server <- function(id, tsbl_vars,
                              slice_type = c("cross_section", "time_series"),
                              debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(tsbl_vars))

    # Logic reactive ----

    # Available variables for choices
    available_variable_codes <- reactive({
      tsbl_vars <- tsbl_vars()
      date_var <- tsibble::index_var(tsbl_vars)
      key_vars <- tsibble::key_vars(tsbl_vars)

      variable_codes <- setdiff(names(tsbl_vars), c(date_var, key_vars))
      variable_codes <- sort(unique(variable_codes))

      variable_names <- paste0(
        variable_codes,
        "(", code2name(variable_codes, exact_match = TRUE), ")"
      )

      variable_codes <- stats::setNames(variable_codes, variable_names)

      variable_codes
    })

    # Available industries for choices
    available_industry_codes <- reactive({
      tsbl_vars <- tsbl_vars()
      industry_codes <- sort(unique(tsbl_vars$indcd))
      industry_names <- paste0(
        code2name(industry_codes, exact_match = TRUE),
        "(", industry_codes, ")"
      )
      industry_codes <- stats::setNames(industry_codes, industry_names)

      industry_codes
    })

    # Available stocks for choices
    available_stock_codes <- reactive({
      if (is.null(input$indcd)) {
        tsbl_vars <- tsbl_vars()
      } else {
        tsbl_vars <- tsbl_vars() %>%
          dplyr::filter(.data$indcd %in% input$indcd)
      }

      stock_codes <- sort(unique(tsbl_vars$stkcd))
      stock_names <- paste0(
        code2name(stock_codes, exact_match = TRUE),
        "(", stock_codes, ")"
      )
      stock_codes <- stats::setNames(stock_codes, stock_names)

      stock_codes
    })

    # Available average by variables for choice
    avaliable_average_by_variables <- reactive({
      tsbl_vars <- tsbl_vars()
      date_var <- tsibble::index_var(tsbl_vars)
      key_vars <- tsibble::key_vars(tsbl_vars)

      # Only character or factor variable can be used for
      # computing industry average
      character_vairables <- zstmodelr::expect_type_fields(
        tsbl_vars, expect_type = c("character"))

      factor_vairables <- zstmodelr::expect_type_fields(
        tsbl_vars, expect_type = c("factor"))

      average_by_variables <- unique(character_vairables, factor_vairables)
      average_by_variables <- setdiff(average_by_variables,
                                      c(date_var, key_vars ))

      average_by_variables
    })

    # Select date range
    select_date_range <- reactive({
      tsbl_vars <- tsbl_vars()

      date_type <- input$date_type

      switch(date_type,
        "single_period" = {
          req(input$report_date > 0)

          report_dates <- sort(unique(tsbl_vars$date))
          start_date <- min(report_dates[report_dates >= input$report_date],
            na.rm = TRUE
          )

          end_date <- start_date
          start_date <- format(start_date, "%Y-%m-%d")
          end_date <- format(end_date, "%Y-%m-%d")
        },
        "multi_period" = {
          start_date <- req(input$start_date)
          end_date <- req(input$end_date)
          if (end_date < start_date) end_date <- start_date
        }
      )

      if (debug) {
        golem::cat_dev(
          "selected date:", start_date, ":", end_date, "\n"
        )
      }

      return(list(start_date = start_date, end_date = end_date))
    })

    # Filter data according user inputs
    slice_vars <- eventReactive(input$apply_filter, {

      slice_vars <- tsbl_vars()
      date_var <- tsibble::index_var(slice_vars)
      key_vars <- tsibble::key_vars(slice_vars)
      focus_vars <- setdiff(names(slice_vars), c(date_var, key_vars))

      # Slice dataset in term of user's inputs
      if (!is.null(input$indcd)) {
        select_indcd <- input$indcd
        slice_vars <- slice_vars %>%
          dplyr::filter(.data$indcd %in% select_indcd)
      }

      if (!is.null(input$stkcd)) {
        select_stkcd <- input$stkcd
        slice_vars <- slice_vars %>%
          dplyr::filter(.data$stkcd %in% select_stkcd)
      }

      if (!is.null(input$period)) {
        select_period <- input$period
        slice_vars <- slice_vars %>%
          dplyr::filter(.data$period %in% select_period)
      }

      select_date_range <- select_date_range()
      if ((select_date_range$start_date != "") &&
        (select_date_range$end_date != "")) {
        slice_vars <- slice_vars %>%
          dplyr::filter(.data$date >= select_date_range$start_date &
            .data$date <= select_date_range$end_date)
      }

      if (!is.null(input$focus_vars)) {
        select_vars <- input$focus_vars
        slice_vars <- slice_vars %>%
          dplyr::select(c({{ date_var }}, {{ key_vars }},
                          input$average_by, {{ select_vars }}))
      }

      # Remove any column with all NAs which contains meaningless information
      slice_vars <- slice_vars %>%
        dplyr::select(where(~ !all(is.na(.x))))

      return(slice_vars)
    })

    # Filter average data by industry
    slice_vars_average <- eventReactive(input$apply_filter, {

      if (isTruthy(input$average_by)) {

        slice_vars_average <- switch(input$average_method,
           "mean" = {
             tsbl_vars() %>%
               aggregate_tsbl_vars(by = input$average_by,
                                   .fun = mean,
                                   na.rm = TRUE)
           },
           "median" = {
             tsbl_vars() %>%
               aggregate_tsbl_vars(by = input$average_by,
                                   .fun = median,
                                   na.rm = TRUE)
           })
      }


      date_var <- tsibble::index_var(slice_vars_average)
      key_vars <- tsibble::key_vars(slice_vars_average)
      focus_vars <- setdiff(names(slice_vars_average), c(date_var, key_vars))

      # Slice dataset in term of user's inputs

      if (!is.null(input$indcd)) {
        select_indcd <- input$indcd
        slice_vars_average <- slice_vars_average %>%
          dplyr::filter(.data$indcd %in% select_indcd)
      }

      if (!is.null(input$period)) {
        select_period <- input$period
        slice_vars_average <- slice_vars_average %>%
          dplyr::filter(.data$period %in% select_period)
      }

      select_date_range <- select_date_range()
      if ((select_date_range$start_date != "") &&
        (select_date_range$end_date != "")) {
        slice_vars_average <- slice_vars_average %>%
          dplyr::filter(.data$date >= select_date_range$start_date &
            .data$date <= select_date_range$end_date)
      }

      if (!is.null(input$focus_vars)) {
        select_vars <- input$focus_vars
        slice_vars_average <- slice_vars_average %>%
          dplyr::select(c({{ date_var }}, {{ key_vars }}, {{ select_vars }}))
      }

      # Remove column with all NAs which contains meaningless information
      slice_vars_average <- slice_vars_average %>%
        dplyr::select(where(~ !all(is.na(.x))))

      slice_vars_average
    })

    # Controls interaction ----

    # Set up initial state of UI controls
    observe({
      tsbl_vars <- tsbl_vars()
      date_var <- tsibble::index_var(tsbl_vars)
      key_vars <- tsibble::key_vars(tsbl_vars)
      assertive::assert_all_are_true(date_var == "date")
      assertive::assert_all_are_true(key_vars %in% c("period", "stkcd"))


      # Set choices for select inputs

      updateSelectInput(
        session = session, inputId = "indcd",
        choices = available_industry_codes(),
        # Set selected with current value to ensure not clear current input
        selected = input$indcd
      )

      updateSelectInput(
        session = session, inputId = "stkcd",
        choices = available_stock_codes()
      )

      updateSelectInput(
        session = session, inputId = "focus_vars",
        choices = available_variable_codes()
      )

      updateSelectInput(
        session = session, inputId = "period",
        choices = sort(unique(tsbl_vars$period))
      )

      # Set initial state of date_type(only once at start-up)
      if (isolate(isTRUE(input$report_date == 0))) {
        slice_type <- match.arg(slice_type,
          choices = c("cross_section", "time_series")
        )
        if (slice_type == "cross_section") {
          updateRadioButtons(
            session = session, inputId = "date_type",
            selected = "single_period"
          )
        } else {
          updateRadioButtons(
            session = session, inputId = "date_type",
            selected = "multi_period"
          )
        }
      }

      updateSliderInput(
        session = session, inputId = "report_date",
        min = min(unique(tsbl_vars$date), na.rm = TRUE),
        max = max(unique(tsbl_vars$date), na.rm = TRUE),
        value = max(unique(tsbl_vars$date), na.rm = TRUE)
      )

      updateSelectInput(
        session = session, inputId = "start_date",
        choices = sort(unique(tsbl_vars$date)),
        selected = min(unique(tsbl_vars$date), na.rm = TRUE)
      )

      updateSelectInput(
        session = session, inputId = "end_date",
        choices = sort(unique(tsbl_vars$date)),
        selected = max(unique(tsbl_vars$date), na.rm = TRUE)
      )

      updateSelectInput(
        session = session, inputId = "average_by",
        choices = avaliable_average_by_variables()
      )

    })

    # Output ----

    output$status_text <- renderText(sep = "\n", {

      if (input$apply_filter == 0) {
        # User never click apply_filter
        vars_status <- glue::glue(
          "Vars:{NROW(tsbl_vars())}x{NCOL(tsbl_vars())}."
        )
        vars_average_status <- ""
        data_state <- "Input"

      } else {
        # User has clicked apply_filter several times
        vars_status <- glue::glue(
          "Vars:{NROW(slice_vars())}x{NCOL(slice_vars())}."
        )

        vars_average_status <- glue::glue(
          "Vars avg:{NROW(slice_vars_average())}x{NCOL(slice_vars_average())}."
        )
        data_state <- "Output"
      }

      status_msg <- glue::glue(
        "{data_state}: {vars_status}\n{vars_average_status}"
      )

      status_msg
    })

    # Render output for debug
    if (debug) {
      output$debug_output_slice_vars <- DT::renderDataTable({
        slice_data <- slice_vars()
        data_table <- DT::datatable(
          slice_data,
          filter = "top",
          extensions = "Scroller",
          options = list(
            pageLength = 5,
            dom = "ltir",
            deferRender = TRUE,
            scrollY = 180,
            scrollX = TRUE,
            scroller = TRUE
          )
        )

        # Format numeric columns
        numeric_vars <- zstmodelr::expect_type_fields(
          slice_data,
          expect_type = "numeric"
        )
        if (length(numeric_vars) > 0) {
          data_table <- data_table %>%
            DT::formatRound(columns = numeric_vars, digits = 3)
        }

        data_table
      })
      output$debug_output_slice_vars_average <- DT::renderDataTable({
        slice_data <- slice_vars_average()
        data_table <- DT::datatable(
          slice_data,
          filter = "top",
          extensions = "Scroller",
          options = list(
            pageLength = 5,
            dom = "ltir",
            deferRender = TRUE,
            scrollY = 180,
            scrollX = TRUE,
            scroller = TRUE
          )
        )

        # Format numeric columns
        numeric_vars <- zstmodelr::expect_type_fields(
          slice_data,
          expect_type = "numeric"
        )
        if (length(numeric_vars) > 0) {
          data_table <- data_table %>%
            DT::formatRound(columns = numeric_vars, digits = 3)
        }

        data_table
      })
    }

    # Result returned by Server function
    slice_result <- list(
      slice_vars = slice_vars,
      slice_vars_average = slice_vars_average
    )
    return(slice_result)
  })
}

#' Testing module app of slice_tsbl
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @param  slice_type  A character indicator about how to slice data, e.g.,
#'  "cross_section", "time_series". Default is cross_section.
#'
#' @param debug A logic to enable debug or not, default is on_debug() which
#'  returns DEBUG environment variable.
#'
#' @describeIn slice_tsbl  Testing App for slicing tibble of time series.
slice_tsbl_app <- function(use_online_data = FALSE,
                           slice_type = c("cross_section", "time_series"),
                           debug = on_debug()) {

  # Prepare data
  tsbl_vars <- load_tsbl_vars(use_online_data)

  slice_type <- match.arg(slice_type)

  # Launch App
  ui <- fluidPage(
    slice_tsbl_ui("slice_tsbl_module", debug = debug)
  )

  server <- function(input, output, session) {
    slice_result <- slice_tsbl_server("slice_tsbl_module",
      tsbl_vars = reactive(tsbl_vars),
      slice_type = slice_type,
      debug = debug
    )
  }
  testApp <- shinyApp(ui, server)


  return(testApp)
}


# Internal functions ------------------------------------------------------
