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
#' @return * UI function doesn't return value.
#'
#' @describeIn load_data  UI function of load_data.
#' @importFrom shiny NS tagList
load_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      status = "primary",
      title = "Parameters",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,

      fluidRow(
        column(
          9,
          selectInput(
            inputId = ns("data_type"),
            label = strong("Data Type:"),
            choices = c("factor", "indicator"),
            width = "100%"
          ),
          selectInput(
            inputId = ns("data_groups"),
            label = strong("Data Groups:"),
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            width = "100%"
          ),
          textInput(
            inputId = ns("select_vars"),
            label = strong("Selected Varables:"),
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
              width = "300px"
            ),
            br(),
            br(),
            actionButton(
              inputId = ns("load_data"),
              label = strong("Load Data"),
              width = "300px"
            )
          )
        )
      )
    ),

    # Output: Tabset of output tables
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Varables Info",
        DT::dataTableOutput(outputId = ns("vars_info_table"))
      ),
      tabPanel(
        "Varables Data",
        DT::dataTableOutput(outputId = ns("vars_data_table"))
      )
    )
  )
}

#' Server function of load_data
#'
#' @describeIn load_data  Server function of load_data.
#' @return * Server function return a data frame of ...
load_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Data Source Reactive ----
    data_info <- reactive({
      stock_db <- stock_db()
      switch(input$data_type,
        "factor" = {
          zstmodelr::get_factors_info(stock_db)
        },
        "indicator" = {
          zstmodelr::get_indicators_info(stock_db)
        }
      )
    })

    data_info_in_group <- reactive({
      if (!is.null(input$data_groups) && (input$data_groups != "")) {
        data_info() %>%
          dplyr::filter(.data[[group_var()]] %in% input$data_groups)
      } else {
        data_info()
      }
    })


    id_var <- function() {
      switch(input$data_type,
        "factor" = {
          "factor_code"
        },
        "indicator" = {
          "ind_code"
        }
      )
    }

    group_var <- function() {
      switch(input$data_type,
        "factor" = {
          "factor_group"
        },
        "indicator" = {
          "ind_category"
        }
      )
    }

    var_groups <- function() {
      data_info() %>%
        dplyr::select(group_var()) %>%
        dplyr::pull(group_var()) %>%
        unique() %>%
        sort()
    }

    # Handle user interaction ----

    # Update UI by user input
    observe({

      # Update choices of data_group
      updateSelectizeInput(session,
        inputId = "data_groups",
        choices = var_groups()
      )
    })


    # Handler for user to select code from table
    user_select_codes <- function(DT_tableId, ds_info, id_var) {
      select_index <- input[[glue::glue("{DT_tableId}_rows_selected")]]
      select_codes <- dplyr::pull(ds_info[select_index, ], {{ id_var }})
      if (length(select_codes) > 0) {
        updateTextInput(
          session = session,
          inputId = "select_vars",
          value = select_codes
        )
      }
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

    observeEvent(input$vars_info_table_cell_clicked,
      {
        if (length(input$vars_info_table_cell_clicked) > 0) {
          user_select_codes(
            DT_tableId = "vars_info_table",
            ds_info = data_info_in_group(),
            id_var = id_var()
          )
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )


    # Clear selections in info table
    observeEvent(input$clear_vars, {
      clear_select_codes()
    })

    # Change data_type
    observeEvent(input$data_type, {
      # Clear selection when changing data_type
      clear_select_codes()
    })

    # Load data
    load_data <- eventReactive(input$load_data, {

      vars_list <- stringr::str_split(input$select_vars,
        pattern = "\\s*,\\s*|\\s+"
      )[[1]]

      withProgress(message = "Load vars data", value = 0, {
        incProgress(message = "connect stock_db")
        stock_db <- stock_db()

        # Get factors from database
        incProgress(message = "get data..")
        ds_vars <-
          switch(isolate(input$data_type),
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

        # Fix stkcd and incd for missing
        if (!("stkcd" %in% names(ds_vars))) {
          ds_vars <- ds_vars %>%
            dplyr::mutate(stkcd = "NA")
        }
        if (!("indcd" %in% names(ds_vars))) {
          ds_vars <- ds_vars %>%
            dplyr::mutate(indcd = "NA")
        }

        # Turn into tsibble
        tsbl_vars <-
          ds_vars %>%
          dplyr::select(c("date","period","stkcd", "indcd"), everything()) %>%
          dplyr::filter(!(is.na(.data[["date"]]))
          && (!is.na(.data[["stkcd"]]))
          && (!is.na(.data[["indcd"]]))) %>%
          tsibble::as_tsibble(
            index = date,
            key = c("stkcd", "period")
          )
      })

      showNotification("Load data successfully.")

      return(tsbl_vars)
    })


    # Output UI -----

    # Display vars Info
    output$vars_info_table <- DT::renderDataTable({
      DT::datatable(
        data_info_in_group(),
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
      numeric_vars <- setdiff(
        names(load_data()),
        c("date", "period", "stkcd", "indcd")
      )
      DT::datatable(
        load_data(),
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

    return(load_data)
  })
}

#' Testing module app of load_data
#'
#' @describeIn load_data  Testing App of load_data.
load_data_app <- function() {
  ui <- fluidPage(
    load_data_ui("load_data_module")
  )
  server <- function(input, output, session) {
    load_data <- load_data_server("load_data_module")
  }
  shinyApp(ui, server)
}
