#' data_dictionary
#'
#' @description A shiny module for data_dictionary.
#'
#' @details
#'  The module is an UI for user to ...
#'
#' @name data_dictionary
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   data_dictionary_ui("data_dictionary_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   data_dictionary <- data_dictionary_server("data_dictionary_module")
#' }
#'
#' # Run testing App for integration testing
#' data_dictionary_app()
#' }
#'
NULL

#' UI function of data_dictionary
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn data_dictionary  UI function of data_dictionary.
#' @importFrom shiny NS tagList
data_dictionary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        1,
        h4(strong("select codes:"))
      ),
      column(
        10,
        textInput(
          inputId = ns("select_codes"),
          label = NULL,
          value = NULL,
          width = "100%"
        )
      ),
      column(
        1,
        actionButton(
          inputId = ns("clear_codes"),
          label = strong("Clear")
        )
      )
    ),
    tabsetPanel(
      id = ns("dictionary_tabs"),
      type = "tabs",
      tabPanel(
        "Stock Info",
        DT::dataTableOutput(outputId = ns("stock_info_table"))
      ),
      tabPanel(
        "Industry Info",
        DT::dataTableOutput(outputId = ns("industry_info_table"))
      ),
      tabPanel(
        "Factor Info",
        DT::dataTableOutput(outputId = ns("factors_info_table"))
      ),
      tabPanel(
        "Indicator Info",
        DT::dataTableOutput(outputId = ns("indicators_info_table"))
      )
    )
  )
}

#' Server function of data_dictionary
#'
#' @describeIn data_dictionary  Server function of data_dictionary.
#' @return * Server function return a data frame of ...
data_dictionary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Data Source Reactive ----

    stock_info <- reactive({
      stock_db <- stock_db()
      stock_info <- zstmodelr::get_stock_info(stock_db)
    })

    industry_info <- reactive({
      stock_db <- stock_db()
      industry_info <- zstmodelr::get_industry_info(stock_db)
    })

    factors_info <- reactive({
      stock_db <- stock_db()
      factors_info <- zstmodelr::get_factors_info(stock_db)
    })

    indicators_info <- reactive({
      stock_db <- stock_db()
      indicators_info <- zstmodelr::get_indicators_info(stock_db)
    })

    # Handle user interaction ----

    # Handler for user to select code from table
    user_select_codes <- function(DT_tableId, ds_info, id_var) {
      select_index <- input[[glue::glue("{DT_tableId}_rows_selected")]]
      select_codes <- dplyr::pull(ds_info[select_index, ], {{ id_var }})
      if (length(select_codes) > 0) {
        updateTextInput(
          session = session,
          inputId = "select_codes",
          value = select_codes
        )
      }
    }

    observeEvent(input$stock_info_table_cell_clicked, ignoreInit = TRUE, {
      if (length(input$stock_info_table_cell_clicked) > 0) {
        user_select_codes(
          DT_tableId = "stock_info_table",
          ds_info = stock_info(),
          id_var = "stkcd"
        )
      }
    })

    observeEvent(input$industry_info_table_cell_clicked, ignoreInit = TRUE, {
      if (length(input$industry_info_table_cell_clicked) > 0) {
        user_select_codes(
          DT_tableId = "industry_info_table",
          ds_info = industry_info(),
          id_var = "indcd"
        )
      }
    })

    observeEvent(input$factors_info_table_cell_clicked, ignoreInit = TRUE, {
      if (length(input$factors_info_table_cell_clicked) > 0) {
        user_select_codes(
          DT_tableId = "factors_info_table",
          ds_info = factors_info(),
          id_var = "factor_code"
        )
      }
    })

    observeEvent(input$indicators_info_table_cell_clicked, ignoreInit = TRUE, {
      if (length(input$indicators_info_table_cell_clicked) > 0) {
        user_select_codes(
          DT_tableId = "indicators_info_table",
          ds_info = indicators_info(),
          id_var = "ind_code"
        )
      }
    })

    observeEvent(input$clear_codes, ignoreInit = TRUE, {

      # Select codes input
      updateTextInput(
        session = session,
        inputId = "select_codes",
        value = ""
      )

      dt_tables <- c(
        "stock_info_table", "industry_info_table",
        "factors_info_table", "indicators_info_table"
      )

      # Clear selections in tables
      for (dt_table in dt_tables) {
        proxy <- DT::dataTableProxy(dt_table)
        DT::selectRows(proxy, selected = NULL)
      }
    })


    # Output UI -----
    output$stock_info_table <- DT::renderDataTable({
      DT::datatable(
        stock_info(),
        filter = "top",
        extensions = "Scroller",
        options = list(
          columnDefs = list(list(className = "dt-center")),
          pageLength = 20,
          dom = "ltir",
          deferRender = TRUE,
          scrollY = 650,
          scrollX = TRUE,
          scroller = TRUE
        )
      )
    })

    output$industry_info_table <- DT::renderDataTable({
      DT::datatable(
        industry_info(),
        filter = "top",
        extensions = "Scroller",
        options = list(
          columnDefs = list(list(className = "dt-center")),
          pageLength = 20,
          dom = "ltir",
          deferRender = TRUE,
          scrollY = 650,
          scrollX = TRUE,
          scroller = TRUE
        )
      )
    })

    output$factors_info_table <- DT::renderDataTable({
      DT::datatable(
        factors_info(),
        filter = "top",
        extensions = "Scroller",
        options = list(
          columnDefs = list(list(className = "dt-center")),
          pageLength = 20,
          dom = "ltir",
          deferRender = TRUE,
          scrollY = 650,
          scrollX = TRUE,
          scroller = TRUE
        )
      )
    })

    output$indicators_info_table <- DT::renderDataTable({
      DT::datatable(
        indicators_info(),
        filter = "top",
        extensions = "Scroller",
        options = list(
          columnDefs = list(list(className = "dt-center")),
          pageLength = 20,
          dom = "ltir",
          deferRender = TRUE,
          scrollY = 650,
          scrollX = TRUE,
          scroller = TRUE
        )
      )
    })
  })
}

#' Testing module app of data_dictionary
#'
#' @describeIn data_dictionary  Testing App of data_dictionary.
data_dictionary_app <- function() {
  ui <- fluidPage(
    data_dictionary_ui("data_dictionary_module")
  )
  server <- function(input, output, session) {
    data_dictionary <- data_dictionary_server("data_dictionary_module")
  }
  shinyApp(ui, server)
}
