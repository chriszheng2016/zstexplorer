#' cs_cor_GGally
#'
#' @description A shiny module for cs_cor_GGally.
#'
#' @details
#'  The module is an UI for user to display plots of correlation
#'  by [`GGally`][GGally::GGally] package.
#'
#' @name cs_cor_GGally
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_cor_GGally_ui("cs_cor_GGally_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_cor_GGally <- cs_cor_GGally_server(
#'     "cs_cor_GGally_module",
#'     csbl_vars = reactive(csbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_cor_GGally_app()
#' }
#'
NULL

# Type of bivariate plots of GGally
bivar_plot_continuous <- c(
  "autopoint", "cor", "density", "points",
  "smooth", "smooth_lm", "smooth_loess"
)
bivar_plot_discrete <- c(
  "colbar", "autopoint", "count", "cross",
  "crosstable", "facetbar", "ratio",
  "rowbar", "table", "trends"
)
bivar_plot_combo <- c(
  "autoplot", "box", "box_no_facet", "denstrip",
  "dot", "dot_no_facet", "acetdensitystrip",
  "facethist", "trends"
)

# Type of diagonal plots of GGally
diag_plot_continuous <- c(
  "autopointDiag", "densityDiag", "barDiag",
  "blankDiag"
)
diag_plot_discrete <- c("autopointDiag", "barDiag", "blankDiag")

# default value of ggpairs of GGally
upper_continuous_default <- "cor"
upper_combo_default <- "box_no_facet"
upper_discrete_default <- "count"

lower_continuous_default <- "points"
lower_combo_default <- "facethist"
lower_discrete_default <- "facetbar"

diag_continuous_default <- "densityDiag"
diag_discrete_default <- "barDiag"

cardinality_threshold_default <- 15


#' UI function of cs_cor_GGally
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_cor_GGally  UI function of cs_cor_GGally.
#' @importFrom shiny NS tagList
cs_cor_GGally_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      position = "right",
      sidebarPanel(
        width = 3,
        fluidRow(
          h4("Click 'Refresh Plot' to display plot..."),
          column(
            width = 6,
            actionButton(
              inputId = ns("refresh_plot"),
              label = strong("Refresh Plot"),
              width = "100%"
            )
          ),
          column(
            width = 6,
            actionButton(
              inputId = ns("reset_default"),
              label = strong("Reset Default"),
              width = "100%"
            )
          )
        ),

        tabsetPanel(
          id = ns("setting_tabs"),
          type = "hidden",
          selected = "ggpairs_setting",
          tabPanelBody(
            value = "ggscatmat_setting",
            h3("ggscatmat setting"),
            selectInput(
              inputId = ns("cor_method"),
              label = strong("cor method:"),
              choices = c("pearson", "spearman")
            )
          ),
          tabPanelBody(
            value = "ggpairs_setting",
            h3("ggpairs setting"),

            selectInput(
              inputId = ns("vars_type"),
              label = strong("Varables type:"),
              choices = c("continuous", "discrete", "all")
            ),

            sliderInput(
              inputId = ns("cardinality_threshold"),
              label = strong("Max categories allowed for each feature:"),
              min = 1,
              max = 200,
              value = cardinality_threshold_default,
              step = 1
            ),

            tabsetPanel(
              id = ns("ggpairs_plots_setting"),
              type = "tabs",
              tabPanel(
                "Upper plot",
                selectInput(
                  inputId = ns("upper_continuous"),
                  label = strong("Continuous:"),
                  choices = bivar_plot_continuous,
                  selected = upper_continuous_default
                ),
                selectInput(
                  inputId = ns("upper_combo"),
                  label = strong("Combo:"),
                  choices = bivar_plot_combo,
                  selected = upper_combo_default
                ),
                selectInput(
                  inputId = ns("upper_discrete"),
                  label = strong("Discrete:"),
                  choices = bivar_plot_discrete,
                  selected = upper_discrete_default
                )
              ),
              tabPanel(
                "Lower plot",
                selectInput(
                  inputId = ns("lower_continuous"),
                  label = strong("Continuous:"),
                  choices = bivar_plot_continuous,
                  selected = lower_continuous_default
                ),
                selectInput(
                  inputId = ns("lower_combo"),
                  label = strong("Combo:"),
                  choices = bivar_plot_combo,
                  selected = lower_combo_default
                ),
                selectInput(
                  inputId = ns("lower_discrete"),
                  label = strong("Discrete:"),
                  choices = bivar_plot_discrete,
                  selected = lower_discrete_default
                )
              ),
              tabPanel(
                "Diag plot",
                selectInput(
                  inputId = ns("diag_continuous"),
                  label = strong("Continuous:"),
                  choices = diag_plot_continuous,
                  selected = diag_continuous_default
                ),
                selectInput(
                  inputId = ns("diag_discrete"),
                  label = strong("Discrete:"),
                  choices = diag_plot_discrete,
                  selected = diag_plot_discrete
                )
              )
            )
          )
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("plot_tabs"),
          type = "tabs",
          selected = "ggpairs plot",
          tabPanel(
            "ggscatmat plot",
            plotOutput(ns("ggscatmat_plot"))
          ),
          tabPanel(
            "ggpairs plot",
            plotOutput(ns("ggpairs_plot"))
          )
        )
      )
    )
  )
}

#' Server function of cs_cor_GGally
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn cs_cor_GGally  Server function of cs_cor_GGally.
#' @return * Server function doesn't return value.
cs_cor_GGally_server <- function(id, csbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(csbl_vars))

    # Focus csbl_vars for analyzing
    csbl_vars_focus <- reactive({
      csbl_vars() %>%
        dplyr::select(-c("id"))
    })

    # Update UI when user choose plot tabs
    observeEvent(input$plot_tabs, {

      # Update setting_tabs according to plot type
      if (input$plot_tabs %in% c("ggpairs plot")) {
        updateTabsetPanel(session,
          inputId = "setting_tabs", # notice: don't use ns("setting_tabs")
          selected = "ggpairs_setting"
        )
      } else {
        updateTabsetPanel(session,
          inputId = "setting_tabs",
          selected = "ggscatmat_setting"
        )
      }
    })

    # Reset default setting
    observeEvent(input$reset_default, {

      # Set default value of ggpairs of GGally

      updateSelectInput(
        session = session, inputId = "upper_continuous",
        selected = upper_continuous_default
      )
      updateSelectInput(
        session = session, inputId = "upper_combo",
        selected = upper_combo_default
      )
      updateSelectInput(
        session = session, inputId = "upper_discrete",
        selected = upper_discrete_default
      )

      updateSelectInput(
        session = session, inputId = "lower_continuous",
        selected = lower_continuous_default
      )
      updateSelectInput(
        session = session, inputId = "lower_combo",
        selected = lower_combo_default
      )
      updateSelectInput(
        session = session, inputId = "lower_discrete",
        selected = lower_discrete_default
      )

      updateSelectInput(
        session = session, inputId = "diag_continuous",
        selected = diag_continuous_default
      )
      updateSelectInput(
        session = session, inputId = "diag_discrete",
        selected = diag_discrete_default
      )

      updateSliderInput(
        session = session, inputId = "cardinality_threshold",
        value = cardinality_threshold_default
      )
    })


    output$ggscatmat_plot <- renderPlot({

      # Notice: how use actionBution to initiate plotting.
      # 1. req() only make sure not execute when UI initialize
      # 2. you should isolate() to isolate other inputs to make sure
      #    plot only depend on clicking of refresh_plot.

      req(input$refresh_plot)

      csbl_vars_focus() %>%
        dplyr::select(where(~ is.numeric(.x))) %>%
        GGally::ggscatmat(
          alpha = 0.3,
          corMethod = isolate(input$cor_method)
        )
      # },
      # cacheKeyExpr = {
      #   list(input$refresh_plot)
    })

    output$ggpairs_plot <- renderPlot({

      # Notice: how use actionBution to initiate plotting.
      # 1. req() only make sure not execute when UI initialize
      # 2. you should isolate() to isolate other inputs to make sure
      #    plot only depend on clicking of refresh_plot.

      req(input$refresh_plot)

      focus_vars <- switch(isolate(input$vars_type),
        "continuous" = {
          csbl_vars_focus() %>%
            dplyr::select(where(~ is.numeric(.x)))
        },
        "discrete" = {
          csbl_vars_focus() %>%
            dplyr::select(where(~ !is.numeric(.x)))
        },
        "all" = {
          csbl_vars_focus()
        }
      )

      focus_vars %>%

        GGally::ggpairs(
          upper = list(
            continuous = isolate(input$upper_continuous),
            combo = isolate(input$upper_combo),
            discrete = isolate(input$upper_discrete)
          ),
          lower = list(
            continuous = isolate(input$lower_continuous),
            combo = isolate(input$lower_combo),
            discrete = isolate(input$lower_discrete)
          ),
          diag = list(
            continuous = isolate(input$diag_continuous),
            discrete = isolate(input$diag_discrete)
          ),
          cardinality_threshold = isolate(input$cardinality_threshold),
          progress = FALSE
        )
      # },
      # cacheKeyExpr = {
      #   list(input$refresh_plot)
    })
  })
}

#' Testing module app of cs_cor_GGally
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_cor_GGally  Testing App of cs_cor_GGally.
cs_cor_GGally_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data)

  ui <- fluidPage(
    cs_cor_GGally_ui("cs_cor_GGally_module")
  )
  server <- function(input, output, session) {
    cs_cor_GGally_server(
      "cs_cor_GGally_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}
