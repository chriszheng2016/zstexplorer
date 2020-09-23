#' cs_missing_naniar
#'
#' @description A shiny module for cs_missing_naniar.
#'
#' @details
#'  The module is an UI for user to display plots of missing pattern
#'  by [`naniar`][naniar::naniar] package.
#'
#' @name cs_missing_naniar
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_missing_naniar_ui("cs_missing_naniar_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_missing_naniar <- cs_missing_naniar_server(
#'     "cs_missing_naniar_module",
#'     csbl_vars = reactive(csbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_missing_naniar_app()
#' }
#'
NULL

#' UI function of cs_missing_naniar
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_missing_naniar  UI function of cs_missing_naniar.
#' @importFrom shiny NS tagList
cs_missing_naniar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      position = "right",
      sidebarPanel(
        width = 3,
        tabsetPanel(
          id = ns("setting_tabs"),
          type = "hidden",
          tabPanelBody(
            value = "upset_plot",
          ),
          tabPanelBody(
            value = "shadow_plot",
            selectInput(
              inputId = ns("target_var"),
              label = strong("Target variable:"),
              choices = ""
            ),
            selectInput(
              inputId = ns("shadow_var"),
              label = strong("Shadow variable:"),
              choices = ""
            )
          ),
          tabPanelBody(
            value = "miss_points",
            selectInput(
              inputId = ns("x_var"),
              label = strong("X variable:"),
              choices = ""
            ),
            selectInput(
              inputId = ns("y_var"),
              label = strong("Y variable:"),
              choices = ""
            ),
            checkboxInput(ns("plotly_miss_points"),
              label = "Plotly Plot",
              value = FALSE
            )
          ),
          tabPanelBody(
            value = "miss_vars",
            selectInput(
              inputId = ns("miss_across_fct"),
              label = strong("Missingness across factors:"),
              choices = ""
            )
          ),
          tabPanelBody(
            value = "miss_cases",
            h4("Ploting missing cases might take very long time, click to proceed."),
            actionButton(
              inputId = ns("plot_miss_cases"),
              label = strong("Plot missing cases")
            )
          )
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          # tabBox( width = 12,
          id = ns("plot_tabs"),
          type = "tabs",
          tabPanel(
            "Upset plot",
            plotOutput(ns("miss_upset_plot"))
          ),
          tabPanel(
            "Shadow plot",
            plotOutput(ns("miss_shadow_plot"))
          ),
          tabPanel(
            "Missing points",

            tabsetPanel(
              id = ns("missing_points_plot_tabs"),
              type = "hidden",
              tabPanelBody(
                value = "origin_plot",
                plotOutput(ns("miss_points_plot"))
              ),
              tabPanelBody(
                value = "plotly_plot",
                plotly::plotlyOutput(ns("miss_points_plotly"))
              )
            )


            # conditionalPanel(
            #   condition = "input$plotly_miss_points == 'FALSE'",
            #   plotOutput(ns("miss_points_plot"))
            # ),
            # conditionalPanel(
            #   condition = "input$plotly_miss_points == 'TRUE'",
            #   plotly::plotlyOutput(ns("miss_points_plotly"))
            # )
          ),
          tabPanel(
            "Missing vars",
            br(),
            fluidRow(
              box(
                title = "Missing Vars Plot", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, width = 8,
                plotOutput(ns("miss_vars_plot")),
              ),
              box(
                title = "Missing Vars Table", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, width = 4,
                tableOutput(ns("miss_vars_summary_table"))
              )
            ),
            fluidRow(
              box(
                title = "Missing vars by factor", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, width = 12,
                collapsed = TRUE,

                plotOutput(ns("miss_vars_byfct_plot"))
              )
            )
          ),
          tabPanel(
            "Missing cases",
            br(),
            fluidRow(
              box(
                title = "Missing Cases Table", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, width = 6,
                tableOutput(ns("miss_case_table"))
              ),
              box(
                title = "Missing Cases Plot", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, width = 6,
                collapsed = TRUE,
                plotOutput(ns("miss_cases_plot"))
              )
            )
          )
        )
      )
    )
  )
}

#' Server function of cs_missing_naniar
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn cs_missing_naniar  Server function of cs_missing_naniar.
#' @return * Server function return a data frame of ...
cs_missing_naniar_server <- function(id, csbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(csbl_vars))

    # Focus csbl_vars for analyzing
    csbl_vars_focus <- reactive({
      csbl_vars() %>%
        dplyr::select(-c("id"))
    })

    ## Update UI  ----

    # Update UI with dataset and user inputs
    observe({

      # Set setting controls for missing points
      x_vars <- names(csbl_vars_focus())
      y_vars <- setdiff(names(csbl_vars_focus()), input$x_var)

      updateSelectInput(
        session = session, inputId = "x_var",
        choices = x_vars,
        # Set selected with current value to ensure not clear current input
        selected = input$x_var
      )

      updateSelectInput(
        session = session, inputId = "y_var",
        choices = y_vars,
        # Set selected with current value to ensure not clear current input
        selected = input$y_var
      )

      # Set control for show plot
      target_vars <- grep("_NA$", names(csbl_vars_shadow()),
        value = TRUE, invert = TRUE
      )
      shadow_vars <- grep("_NA$", names(csbl_vars_shadow()),
        value = TRUE
      )

      updateSelectInput(
        session = session, inputId = "target_var",
        choices = target_vars,
        # Set selected with current value to ensure not clear current input
        selected = input$target_var
      )

      updateSelectInput(
        session = session, inputId = "shadow_var",
        choices = shadow_vars,
        # Set selected with current value to ensure not clear current input
        selected = input$shadow_var
      )

      # Set control for miss_vars_plot
      fact_vars <- csbl_vars_focus() %>%
        dplyr::select(where(~ !is.numeric(.x))) %>%
        names()

      updateSelectInput(
        session = session, inputId = "miss_across_fct",
        choices = fact_vars,
        # Set selected with current value to ensure not clear current input
        selected = input$miss_across_fct
      )
    })

    # Update UI when user choose plot tabs
    observeEvent(input$plot_tabs, ignoreInit = TRUE, {
      switch(input$plot_tabs,
        "Upset plot" = {
          updateTabsetPanel(session,
            inputId = "setting_tabs", # notice: don't use ns("setting_tabs")
            selected = "upset_plot"
          )
        },
        "Shadow plot" = {
          updateTabsetPanel(session,
            inputId = "setting_tabs", # notice: don't use ns("setting_tabs")
            selected = "shadow_plot"
          )
        },
        "Missing points" = {
          updateTabsetPanel(session,
            inputId = "setting_tabs", # notice: don't use ns("setting_tabs")
            selected = "miss_points"
          )
        },
        "Missing vars" = {
          updateTabsetPanel(session,
            inputId = "setting_tabs", # notice: don't use ns("setting_tabs")
            selected = "miss_vars"
          )
        },
        "Missing cases" = {
          updateTabsetPanel(session,
            inputId = "setting_tabs", # notice: don't use ns("setting_tabs")
            selected = "miss_cases"
          )
        }
      )
    })

    # Update tabs of missing points plot when user choose whether to apply plotly
    observeEvent(input$plotly_miss_points, ignoreInit = TRUE, {
      if (input$plotly_miss_points) {
        updateTabsetPanel(session,
          inputId = "missing_points_plot_tabs",
          selected = "plotly_plot"
        )
      } else {
        updateTabsetPanel(session,
          inputId = "missing_points_plot_tabs",
          selected = "origin_plot"
        )
      }
    })


    ## Upset plot ----
    output$miss_upset_plot <- renderPlot({
      naniar::gg_miss_upset(csbl_vars_focus(),
        nsets = naniar::n_var_miss(csbl_vars_focus())
      )
    })

    # csbl_vars with shadow matrix
    csbl_vars_shadow <- reactive({
      csbl_vars_focus() %>%
        naniar::bind_shadow()
    })

    ## Shadow plot ----
    output$miss_shadow_plot <- renderPlot({
      req(input$target_var, input$shadow_var)
      csbl_vars_shadow() %>%
        ggplot(aes(
          x = .data[[input$target_var]],
          fill = .data[[input$shadow_var]]
        )) +
        geom_density(alpha = 0.5)
    })

    ## Missing points plot ----

    output$miss_points_plot <- renderPlot({
      req(input$plotly_miss_points == FALSE)
      req(input$x_var, input$y_var)
      csbl_vars_focus() %>%
        ggplot(aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
        naniar::geom_miss_point()
    })

    output$miss_points_plotly <- plotly::renderPlotly({
      req(input$plotly_miss_points == TRUE)
      req(input$x_var, input$y_var)

      p <- csbl_vars_focus() %>%
        ggplot(aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
        naniar::geom_miss_point()

      plotly::ggplotly(p, dynamicTicks = TRUE)
    })


    ## Missing vars plot ----
    output$miss_vars_plot <- renderPlot({
      csbl_vars_focus() %>%
        naniar::gg_miss_var(show_pct = TRUE)
    })

    output$miss_vars_summary_table <- renderTable({
      csbl_vars_focus() %>%
        naniar::miss_var_summary()
    })

    output$miss_vars_byfct_plot <- renderPlot({
      csbl_vars_focus() %>%
        naniar::gg_miss_fct(fct = .data[[req(input$miss_across_fct)]]) +
        theme(axis.text.x = element_text(angle = 90))
    })

    ##  Missing cases plot ----
    output$miss_cases_plot <- renderPlot({
      req(input$plot_miss_cases)
      csbl_vars_focus() %>%
        naniar::gg_miss_case(order_cases = TRUE) +
        labs(x = "Number of Cases")
    })

    output$miss_case_table <- renderTable({
      csbl_vars_focus() %>%
        naniar::miss_case_table()
    })
  })
}

#' Testing module app of cs_missing_naniar
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_missing_naniar  Testing App of cs_missing_naniar.
cs_missing_naniar_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data = FALSE)

  ui <- fluidPage(
    cs_missing_naniar_ui("cs_missing_naniar_module")
  )
  server <- function(input, output, session) {
    cs_missing_naniar_server(
      "cs_missing_naniar_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}
