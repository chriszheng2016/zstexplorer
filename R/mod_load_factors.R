#' Loading factors
#'
#' @description A shiny module for loading factors specified by user from database.
#'
#' @details
#'  The module is an UI for user to choose some factors to load from
#'  database.
#'
#'  To use this module, you should assign same id parameter for
#'  calling UI and Server function. Another parameter is factors_info returned
#'  by [zstmodelr::get_factors_info()], for example:
#'
#'  ```
#'    # Open database
#'    stock_db <- zstmodelr::stock_db(zstmodelr::gta_db,
#'                                    get_golem_config("database_dsn"))
#'    zstmodelr::open_stock_db(stock_db)
#'
#'    # Fetch factors information
#'    factors_info <- zstmodelr::get_factors_info(stock_db, factor_groups = NULL)
#'
#'    # Close database
#'    zstmodelr::close_stock_db(stock_db)
#'
#'  ```
#'  To conduct integration testing, you could run `load_factors_app()`,
#'  for example:
#'
#'  ```
#'
#'    # Fetch factors information from database
#'    stock_db <- zstmodelr::stock_db(zstmodelr::gta_db,
#'                                    get_golem_config("database_dsn"))
#'
#'    zstmodelr::open_stock_db(stock_db)
#'    factors_info <- zstmodelr::get_factors_info(stock_db, factor_groups = NULL)
#'
#'    # Run Testing App
#'    load_factors_app(factors_info)
#'
#'  ```
#'
#' @name load_factors
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#' @param factors_info  A data frame of factor information for user to choose.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   load_factors_ui("load_factors_module", factors_info = factors_info)
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   load_factors <- load_factors_server("load_factors_module",
#'     factors_info = reactive(factors_info)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' load_factors_app(factors_info)
#' }
#'
NULL

#' UI function of load_factors
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn load_factors  UI function of loading factors.
#' @importFrom shiny NS tagList
load_factors_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinydashboard::box(
      status = "primary",
      title = "Parameters",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,

      selectInput(
        inputId = ns("factor_groups"),
        label = strong("Factor Groups:"),
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),

      selectizeInput(
        inputId = ns("factors_in_group"),
        label = strong("Factors in Group:"),
        choices = NULL,
        multiple = TRUE
      ),

      selectInput(
        inputId = ns("select_factors"),
        label = strong("Selected Factors:"),
        choices = NULL,
        multiple = TRUE
      ),

      actionButton(
        inputId = ns("load_factors"),
        label = strong("Load Factors Data")
      )
    ),

    # Output: Tabset of output tables
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Factor Info",
        DT::dataTableOutput(outputId = ns("factors_info_table"))
      ),
      tabPanel(
        "Factor Data",
        DT::dataTableOutput(outputId = ns("factors_data_table"))
      )
    )
  )
}

#' Server function of load_factors
#'
#' @describeIn load_factors  Server function of loading factors.
#' @return * Server function return a data frame of selected factors from database.
load_factors_server <- function(id, factors_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(factors_info))

    # Initial update controls (Only run when initializing values of controls)
    observe({
      factors_info <- factors_info()

      # Update setting of factors_groups
      factor_groups <- sort(unique(factors_info$factor_group))
      updateSelectizeInput(session,
        inputId = "factor_groups",
        choices = unique(factor_groups),
        selected = head(factor_groups, n = 1)
      )
    })

    # Update factors_in_group input according selected factor group
    observe({

      # Get new selected factor group
      selected_factor_groups <- req(input$factor_groups)

      factors_info <- factors_info()

      # Prepare factor groups/factor list
      factors_in_selected_group <-
        factors_info %>%
        dplyr::filter(.data$factor_group %in% !!selected_factor_groups)

      candidate_factor_code <- factors_in_selected_group$factor_code
      names(candidate_factor_code) <- factors_in_selected_group$factor_name

      # Update factors in group input by specified groups
      updateSelectizeInput(session,
        inputId = "factors_in_group",
        choices = candidate_factor_code
      )
    })

    # Update select_factors input according new selected factors
    observe({

      # Get new selected factors
      new_select_factors <- req(input$factors_in_group)

      # Construct current select factors by new and old selected factors
      current_select_factors <- isolate(input$select_factors)
      current_select_factors <- unique(c(current_select_factors, new_select_factors))

      # Update factors in group by specified groups
      updateSelectInput(session,
        inputId = "select_factors",
        selected = current_select_factors,
        choices = current_select_factors
      )
    })

    # Load factors
    load_factors <- reactive({

      req(input$load_factors)

      factors_list <- isolate(req(input$select_factors))

      withProgress(message = "Load Factors", value = 0, {
        incProgress(message = "open_stock_db")

        # Open stock database
        stock_db <- zstmodelr::stock_db(
          zstmodelr::gta_db,
          get_golem_config("database_dsn")
        )
        zstmodelr::open_stock_db(stock_db)

        incProgress(message = "init_stock_db")

        # Initiate the stock database
        invisible(zstmodelr::init_stock_db(stock_db))

        incProgress(message = "get_factor_indicator")

        # Get factors from database
        ds_factors <-
          zstmodelr::get_factors(stock_db, factor_codes = factors_list) %>%
          tidyr::pivot_wider(
            names_from = "factor_name",
            values_from = "factor_value"
          )

        incProgress(message = "convert to tsibble")

        # Turn into tsibble
        tsbl_factors <-
          ds_factors %>%
          dplyr::filter(!(is.na(date)) & (!is.na(stkcd)) & (!is.na(indcd))) %>%
          tsibble::as_tsibble(
            index = date,
            key = c("stkcd", "period")
          )

        zstmodelr::close_stock_db(stock_db)
      })

      showNotification("Load factors successfully.")

      return(tsbl_factors)
    })

    # Display factors Info
    output$factors_info_table <- DT::renderDataTable(
      DT::datatable(
        factors_info(),
        options = list(
          columnDefs = list(list(className = "dt-center")),
          pageLength = 10
        )
      )
    )

    output$factors_data_table <- DT::renderDataTable(
       load_factors(),
      options = list(
        autoWidth = FALSE,
        columnDefs = list(
          list(className = "dt-center"),
          list(width = "20px")
          ),
        pageLength = 10
      )
    )

    return(load_factors)
  })
}

#' Testing module app of load_factors
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn load_factors  Testing App of loading factors.
load_factors_app <- function(use_online_data = FALSE) {

  # Prepare data
  factors_info <- load_factors_info(use_online_data)

  ui <- fluidPage(
    load_factors_ui("load_factors_module")
  )
  server <- function(input, output, session) {
    load_factors <- load_factors_server("load_factors_module",
      factors_info = reactive(factors_info)
    )
  }
  shinyApp(ui, server)
}
