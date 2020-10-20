#' cs_missing_vim
#'
#' @description A shiny module for cs_missing_vim.
#'
#' @details
#'  The module is an UI for user to display plots of missing pattern
#'    by [`VIM`][VIM::VIM] package.
#'
#' @name cs_missing_vim
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_missing_vim_ui("cs_missing_vim_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_missing_vim <- cs_missing_vim_server(
#'     "cs_missing_vim_module",
#'     csbl_vars = reactive(csbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_missing_vim_app()
#' }
#'
NULL

#' UI function of cs_missing_vim
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_missing_vim  UI function of cs_missing_vim.
#' @importFrom shiny NS tagList
cs_missing_vim_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(), # use for hide/show control with js
    sidebarLayout(
      position = "right",
      sidebarPanel(
        width = 3,
        checkboxInput(ns("show_onlymiss"),
          label = "Show Only Missing",
          value = FALSE
        ),
        tabsetPanel(
          id = ns("setting_tabs"),
          type = "hidden",
          tabPanelBody(
            value = "aggr_plot",

            checkboxInput(ns("show_numbers"),
              label = "Show Numbers",
              value = FALSE
            ),

            checkboxInput(ns("show_prop"),
              label = "Show Proptions",
              value = TRUE
            )
          ),

          tabPanelBody(
            value = "bar_hist_plot",

            selectInput(
              inputId = ns("target_var"),
              label = strong("Target variable:"),
              choices = ""
            )
          ),
          tabPanelBody(
            value = "matrix_plot",

            selectInput(
              inputId = ns("sortby_var"),
              label = strong("Sorted by:"),
              choices = ""
            )
          ),
          tabPanelBody(
            value = "scatter_margin_plot",
            selectInput(
              inputId = ns("x_var"),
              label = strong("x:"),
              choices = ""
            ),
            selectInput(
              inputId = ns("y_var"),
              label = strong("y:"),
              choices = ""
            ),
            selectInput(
              inputId = ns("scattmiss_side"),
              label = strong("line on side:"),
              choices = c("1", "2")
            )
          ),
          tabPanelBody(
            value = "scatter_matrix_plot",

            selectInput(
              inputId = ns("scattmatrix_plot_vars"),
              label = strong("Varables to plot:"),
              multiple = TRUE,
              choices = ""
            ),

            selectInput(
              inputId = ns("scattmatrix_highlight_vars"),
              label = strong("Varables to highlight:"),
              multiple = TRUE,
              choices = ""
            ),

            selectInput(
              inputId = ns("scattmatrix_selection"),
              label = strong("Selection Method:"),
              choices = c("any", "all")
            ),

            actionButton(
              inputId = ns("plot_scattmatrix"),
              label = strong("Plot")
            )
          ),
          tabPanelBody(
            value = "margin_matrix_plot",

            selectInput(
              inputId = ns("marginmatrix_plot_vars"),
              label = strong("Varables to plot:"),
              multiple = TRUE,
              choices = ""
            ),

            actionButton(
              inputId = ns("plot_marginmatrix"),
              label = strong("Plot")
            )
          )
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("plot_tabs"),
          type = "tabs",
          tabPanel(
            "Aggr plot",

            h4("A Aggregations plot for missing/imputed values"),

            plotOutput(ns("miss_aggr_plot"))
          ),
          tabPanel(
            "BarMiss plot",

            h4("A barplot with highlighting of missing/imputed values
               in other variables by splitting each bar into two parts"),

            plotOutput(ns("miss_bar_plot"))
          ),
          tabPanel(
            "HistMiss plot",

            h4("A histogram with highlighting the missing/imputed values in
               other variables by splitting each bin into two parts"),

            plotOutput(ns("miss_hist_plot"))
          ),
          tabPanel(
            "Matrix plot",

            h4("A matrix plot, in which all cells of a data matrix are
               visualized by rectangles."),

            plotOutput(ns("miss_matrix_plot"))
          ),
          tabPanel(
            "ScattMiss plot",

            h4("A scatter plots which lines for the missing missing/imputed
               values"),

            plotOutput(ns("miss_scatter_plot"))
          ),
          tabPanel(
            "Margin plot",

            h4("A scatter plot with additional information in the margins"),

            plotOutput(ns("miss_margin_plot"))
          ),
          tabPanel(
            "Scatter matrix plot",

            h4("A scatter matrix in which observations with missing/imputed
               values in certain variables are highlighted."),

            plotOutput(ns("miss_scatter_matrix_plot"))
          ),
          tabPanel(
            "Margin matrix plot",

            h4("A scatterplot matrix with information about missing/imputed
               values in the plot margins of each panel"),

            plotOutput(ns("miss_margin_matrix_plot"))
          )
        )
      )
    )
  )
}

#' Server function of cs_missing_vim
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn cs_missing_vim  Server function of cs_missing_vim.
#' @return * Server function return a data frame of ...
cs_missing_vim_server <- function(id, csbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(csbl_vars))

    # Logic reactive ----

    # User control interaction ----

    # > Initiate UI with dataset and user setting ----
    observe({
      target_vars <- names(csbl_vars())
      sortby_vars <- names(csbl_vars())
      numeric_vars <- zstmodelr::expect_type_fields(
        csbl_vars(),
        expect_type = "numeric"
      )
      x_vars <- numeric_vars
      y_vars <- setdiff(numeric_vars, input$x_var)

      updateSelectInput(
        session = session, inputId = "target_var",
        choices = target_vars
      )

      updateSelectInput(
        session = session, inputId = "sortby_var",
        choices = sortby_vars
      )

      updateSelectInput(
        session = session, inputId = "x_var",
        choices = x_vars
      )

      updateSelectInput(
        session = session, inputId = "y_var",
        choices = y_vars
      )

      updateSelectInput(
        session = session, inputId = "scattmatrix_plot_vars",
        choices = numeric_vars
      )

      updateSelectInput(
        session = session, inputId = "scattmatrix_highlight_vars",
        choices = numeric_vars
      )

      updateSelectInput(
        session = session, inputId = "marginmatrix_plot_vars",
        choices = numeric_vars
      )
    })

    # > Update UI when user choose plot tabs ----
    observeEvent(input$plot_tabs, ignoreInit = TRUE, {
      switch(input$plot_tabs,
        "Aggr plot" = {
          shinyjs::show(id = "show_onlymiss")
          updateTabsetPanel(session,
            inputId = "setting_tabs", # notice: don't use ns("setting_tabs")
            selected = "aggr_plot"
          )
        },
        "BarMiss plot" = {
          shinyjs::show(id = "show_onlymiss")
          updateTabsetPanel(session,
            inputId = "setting_tabs", # notice: don't use ns("setting_tabs")
            selected = "bar_hist_plot"
          )
        },
        "HistMiss plot" = {
          shinyjs::show(id = "show_onlymiss")
          updateTabsetPanel(session,
            inputId = "setting_tabs", # notice: don't use ns("setting_tabs")
            selected = "bar_hist_plot"
          )
        },
        "Matrix plot" = {
          shinyjs::hide(id = "show_onlymiss")
          updateTabsetPanel(session,
            inputId = "setting_tabs", # notice: don't use ns("setting_tabs")
            selected = "matrix_plot"
          )
        },
        "ScattMiss plot" = {
          shinyjs::hide(id = "show_onlymiss")
          shinyjs::show(id = "scattmiss_side")
          updateTabsetPanel(session,
            inputId = "setting_tabs", # notice: don't use ns("setting_tabs")
            selected = "scatter_margin_plot"
          )
        },
        "Margin plot" = {
          shinyjs::hide(id = "show_onlymiss")
          shinyjs::hide(id = "scattmiss_side")
          updateTabsetPanel(session,
            inputId = "setting_tabs", # notice: don't use ns("setting_tabs")
            selected = "scatter_margin_plot"
          )
        },
        "Scatter matrix plot" = {
          shinyjs::hide(id = "show_onlymiss")
          updateTabsetPanel(session,
            inputId = "setting_tabs", # notice: don't use ns("setting_tabs")
            selected = "scatter_matrix_plot"
          )
        },
        "Margin matrix plot" = {
          shinyjs::hide(id = "show_onlymiss")
          updateTabsetPanel(session,
            inputId = "setting_tabs", # notice: don't use ns("setting_tabs")
            selected = "margin_matrix_plot"
          )
        },
      )
    })



    # Render output ----

    # > Missing aggr plot ----
    output$miss_aggr_plot <- renderPlot({
      VIM::aggr(csbl_vars(),
        numbers = input$show_numbers,
        prop = input$show_prop,
        only.miss = input$show_onlymiss
      )
    })

    # > Missing bar plot ----
    output$miss_bar_plot <- renderPlot({
      target_var_pos <- grep(
        paste0("^", input$target_var, "$"),
        names(csbl_vars())
      )

      VIM::barMiss(csbl_vars(),
        only.miss = input$show_onlymiss,
        pos = target_var_pos
      )
    })

    # > Missing hist plot ----
    output$miss_hist_plot <- renderPlot({
      target_var_pos <- grep(
        paste0("^", input$target_var, "$"),
        names(csbl_vars())
      )

      VIM::histMiss(csbl_vars(),
        only.miss = input$show_onlymiss,
        pos = target_var_pos
      )
    })

    # > Missing matrix plot ----
    output$miss_matrix_plot <- renderPlot({
      VIM::matrixplot(csbl_vars(),
        sortby = input$sortby_var
      )
    })

    # > Missing scatter plot ----
    output$miss_scatter_plot <- renderPlot({
      df_target <- csbl_vars() %>%
        dplyr::select(input$x_var, input$y_var)

      VIM::scattMiss(
        df_target,
        side = as.numeric(input$scattmiss_side)
      )
    })

    # > Missing margin plot ----
    output$miss_margin_plot <- renderPlot({
      df_target <- csbl_vars() %>%
        dplyr::select(input$x_var, input$y_var)

      VIM::marginplot(
        df_target,
        numbers = TRUE
      )
    })

    # > Missing scatter matrix plot ----
    output$miss_scatter_matrix_plot <- renderPlot({

      # Notice: how use actionBution to initiate plotting.
      # 1. req() only make sure not execute when UI initialize
      # 2. you should isolate() to isolate other inputs to make sure
      #    plot only depend on clicking of plot_scattmarix.

      req(input$plot_scattmatrix)

      df_target <- csbl_vars() %>%
        dplyr::select(where(is.numeric))

      VIM::scattmatrixMiss(
        df_target,
        highlight = isolate(input$scattmatrix_highlight_vars),
        selection = isolate(input$scattmatrix_selection),
        plotvars = isolate(req(input$scattmatrix_plot_vars))
      )
    })

    # > Missing margin matrix plot ----
    output$miss_margin_matrix_plot <- renderPlot({
      req(input$plot_marginmatrix)

      df_target <- csbl_vars() %>%
        dplyr::select(isolate(req(input$marginmatrix_plot_vars)))

      VIM::marginmatrix(
        df_target,
        alpha = 0.5
      )
    })
  })
}

#' Testing module app of cs_missing_vim
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_missing_vim  Testing App of cs_missing_vim.
cs_missing_vim_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data = FALSE)

  ui <- fluidPage(
    cs_missing_vim_ui("cs_missing_vim_module")
  )
  server <- function(input, output, session) {
    cs_missing_vim_server(
      "cs_missing_vim_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}
