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
#'  * **Industry**: one or many industry codes.
#'
#'  * **Stock Code**: one or many stock codes.
#'
#'  * **Focus Variable**:  one or many variables to focus.
#'
#'  * **Date Range**:
#'
#'    * Single Period:  specify report date.
#'
#'    * Multiple Periods: specify start/end date.
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
#'   load_factors <- slice_tsbl_server("slice_tsbl_module",
#'     tsbl_vars = reactive(tsbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' slice_tsbl_app(tsbl_vars)
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
      label = strong("Industry:"),
      choices = "", multiple = TRUE
    ),

    selectInput(
      inputId = ns("stkcd"),
      label = strong("Stock Code:"),
      choices = "", multiple = TRUE
    ),

    selectInput(
      inputId = ns("focus_vars"),
      label = strong("Focus Varable:"),
      choices = "", multiple = TRUE
    ),

    radioButtons(
      inputId = ns("date_type"),
      label = strong("Date Range"),
      choices = list(
        "Single Period" = "single_period",
        "Multiple Periods" = "multi_period"
      ),
      selected = "multi_period"
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
        label = strong("Start Date:"),
        choices = ""
      ),
      selectInput(
        inputId = ns("end_date"),
        label = strong("End Date:"),
        choices = ""
      ),
      ns = ns
    ),

    # Debug Control for output
    if (debug) tableOutput(outputId = ns("debug_output"))

  )
}

#' Server Function of slice_tsbl
#'
#' @return * Server function return a tsibble sliced by user's filter conditions.
#'
#' @describeIn slice_tsbl  Server function of slicing tibble of time series.
#'
slice_tsbl_server <- function(id, tsbl_vars, debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(tsbl_vars))

    # Define constant string for all selection option
    # all_tag <- reactive("ALL")

    # Update UI with dataset and user inputs
    observe({
      tsbl_vars <- tsbl_vars()
      date_var <- tsibble::index_var(tsbl_vars)
      key_vars <- tsibble::key_vars(tsbl_vars)
      assertive::assert_all_are_true(date_var == "date")
      assertive::assert_all_are_true(key_vars %in% c("period", "stkcd", "indcd"))
      focus_vars <- setdiff(names(tsbl_vars), c(date_var, key_vars))

      # Set choices for select inputs
      updateSelectInput(
        session = session, inputId = "indcd",
        choices = sort(unique(tsbl_vars$indcd)),
        # Set selected with current value to ensure not clear current input
        selected = input$indcd
      )

      # Update stkcd choices by input of indcd
      if (is.null(input$indcd)) {
        avaiable_stkcds <- sort(unique(tsbl_vars$stkcd))
      } else {
        avaiable_stkcds <-
          tsbl_vars %>%
          dplyr::filter(indcd %in% input$indcd) %>%
          dplyr::distinct(stkcd) %>%
          dplyr::arrange(stkcd) %>%
          dplyr::pull()
      }
      updateSelectInput(
        session = session, inputId = "stkcd",
        choices = avaiable_stkcds
        # Don't set selected value to clear current input
        # selected = input$stkcd
      )

      updateSelectInput(
        session = session, inputId = "focus_vars",
        choices = sort(unique(focus_vars)),
        # Set selected with current value to ensure not clear current input
        selected = input$focus_vars
      )

      updateSliderInput(
        session = session, inputId = "report_date",
        min = min(unique(tsbl_vars$date)),
        max = max(unique(tsbl_vars$date)),
        value = max(unique(tsbl_vars$date))
      )

      updateSelectInput(
        session = session, inputId = "start_date",
        choices = unique(tsbl_vars$date),
        selected = min(unique(tsbl_vars$date))
      )

      updateSelectInput(
        session = session, inputId = "end_date",
        choices = unique(tsbl_vars$date),
        selected = max(unique(tsbl_vars$date))
      )
    })

    # Select date range
    select_date_range <- reactive({
      tsbl_vars <- tsbl_vars()

      date_type <- input$date_type

      switch(date_type,
        "single_period" = {
          report_dates <- sort(unique(tsbl_vars$date))
          start_date <- min(report_dates[report_dates >= input$report_date])
          end_date <- start_date
          start_date <- format(start_date, "%Y-%m-%d")
          end_date <- format(end_date, "%Y-%m-%d")
        },
        "multi_period" = {
          start_date <- input$start_date
          end_date <- input$end_date
          if (end_date < start_date) end_date <- start_date
        }
      )

      # golem::cat_dev(
      #   "selected date:", start_date, ":", end_date, "\n"
      # )

      return(list(start_date = start_date, end_date = end_date))
    })

    # Filter data according user inputs
    slice_dataset <- reactive({
      tsbl_vars <- tsbl_vars()
      date_var <- tsibble::index_var(tsbl_vars)
      key_vars <- tsibble::key_vars(tsbl_vars)
      focus_vars <- setdiff(names(tsbl_vars), c(date_var, key_vars))

      # Slice dataset in term of user's inputs

      slice_dataset <- tsbl_vars
      if (!is.null(input$indcd)) {
        select_indcd <- input$indcd
        slice_dataset <-
          slice_dataset %>%
          dplyr::filter(indcd %in% select_indcd)
      }

      if (!is.null(input$stkcd)) {
        select_stkcd <- input$stkcd
        slice_dataset <-
          slice_dataset %>%
          dplyr::filter(stkcd %in% select_stkcd)
      }

      select_date_range <- select_date_range()
      if ((select_date_range$start_date != "") &&
        (select_date_range$end_date != "")) {
        slice_dataset <-
          slice_dataset %>%
          dplyr::filter(date >= select_date_range$start_date &
            date <= select_date_range$end_date)
      }

      if (!is.null(input$focus_vars)) {
        select_vars <- input$focus_vars
        slice_dataset <-
          slice_dataset %>%
          dplyr::select(c({{ date_var }}, {{ key_vars }}, {{ select_vars }}))
      }

      return(slice_dataset)
    })

    # Render output for debug
    if (debug) {
      output$debug_output <- renderTable(head(slice_dataset()))
    }

    return(slice_dataset)
  })
}

#' Testing module app of slice_tsbl
#'
#' @describeIn slice_tsbl  Testing App for slicing tibble of time series.
slice_tsbl_app <- function() {

  # Prepare data
  stock_db <- zstmodelr::stock_db(
    zstmodelr::gta_db,
    get_golem_config("database_dsn")
  )
  zstmodelr::open_stock_db(stock_db)
  zstmodelr::init_stock_db(stock_db)

  # Fetch selected factors from database
  select_factors <- c("CR", "QR")
  ds_factors <-
    zstmodelr::get_factors(stock_db, factor_codes = select_factors) %>%
    tidyr::pivot_wider(
      names_from = "factor_name",
      values_from = "factor_value"
    )

  # Turn into tsibble
  tsbl_vars <- tsibble::as_tsibble(ds_factors,
    index = date,
    key = c(period, stkcd, indcd)
  )
  zstmodelr::close_stock_db(stock_db)

  # Launch App
  ui <- fluidPage(
    slice_tsbl_ui("slice_tsbl_module", debug = TRUE)
  )
  server <- function(input, output, session) {
    load_factors <- slice_tsbl_server("slice_tsbl_module",
      tsbl_vars = reactive(tsbl_vars),
      debug = TRUE
    )
  }
  testApp <- shinyApp(ui, server)

  # enable_temp_controls <<- FALSE

  return(testApp)
}


# Internal functions ------------------------------------------------------