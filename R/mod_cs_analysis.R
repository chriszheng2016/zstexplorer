#' cs_analysis
#'
#' @description A shiny module for cs_analysis.
#'
#' @details
#'  The module is an UI for user to analyze variables in cross-section.
#'
#' @name cs_analysis
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_analysis_ui("cs_analysis_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_analysis <- cs_analysis_server(
#'     "cs_analysis_module",
#'     tsbl_vars = reactive(tsbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_analysis_app()
#' }
#'
NULL

#' UI function of cs_analysis
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_analysis  UI function of cs_analysis.
#' @importFrom shiny NS tagList
cs_analysis_ui <- function(id, debug = FALSE) {
  ns <- NS(id)
  tagList(
    # Side panel for input
    sidebarPanel(
      width = 2,

      # Ui for slicing tsbl_vars
      slice_tsbl_ui(ns("slice_tsbl_module"), debug = debug)
    ),

    # MainPanel for Output
    mainPanel(
      width = 10,
      titlePanel(textOutput(outputId = ns("caption"))),
      navbarPage("Navigate:",
        id = ns("tabs"),
        navbarMenu(
          "Summary",
          tabPanel(
            "base::summary()",
            box(
              status = "primary", width = 12,
              verbatimTextOutput(ns("base_summary_text"))
            )
          ),
          tabPanel(
            "Hmisc::describe()",
            box(
              status = "primary", width = 12,
              verbatimTextOutput(ns("Hmisc_describe_text"))
            )
          ),
          tabPanel(
            "DataExplorer::introduce()",
            box(
              status = "primary", width = 12,
              tableOutput(ns("DataExplorer_intro_table")),
              br(),
              plotOutput(ns("DataExplorer_intro_plot"))
            )
          ),
          tabPanel(
            "skimr::skim()",
            box(
              status = "primary", width = 12,
              verbatimTextOutput(ns("skimr_skim_text"))
            )
          ),
          tabPanel(
            "fBasics::basicStats()",
            box(
              title = "Summary of continuous varables",
              status = "primary", width = 12,
              tableOutput(ns("fBasics_basic_table"))
            )
          ),
          tabPanel(
            "my_summary()",
            box(
              title = "Summary of continuous varables",
              status = "primary", width = 12,
              tableOutput(ns("my_summary_table"))
            )
          )
        ),
        navbarMenu(
          "Missing",
          tabPanel(
            "DataExplorer::profile_missing()",
            cs_missing_DataExplorer_ui(ns("cs_missing_DataExplorer_module"))
          ),
          tabPanel(
            "visdat::vis_miss()",
            cs_missing_visdat_ui(ns("cs_missing_visdat_module"))
          ),
          tabPanel(
            "mice::md.pattern()",
            cs_missing_mice_ui(ns("cs_missing_mice_module"))
          ),
          tabPanel(
            "naniar::funs()",
            cs_missing_naniar_ui(ns("cs_missing_naniar_module"))
          ),
          tabPanel(
            "vim::funs()",
            cs_missing_vim_ui(ns("cs_missing_vim_module"))
          )
        ),
        navbarMenu(
          "Distribution",
          tabPanel(
            "DataExplorer::funs()",
            cs_dist_DataExplorer_ui(ns("cs_dist_DataExplorer_module"))
          ),
          tabPanel(
            "plotly::funs()",
            cs_dist_plotly_ui(ns("cs_dist_plotly_module"))
          ),
          tabPanel(
            "PerformanceAnalytics::funs()",
            cs_dist_PerformanceAnalytics_ui(
              ns("cs_dist_PerformanceAnalytics_module")
            )
          )
        ),
        navbarMenu(
          "Correlation",
          tabPanel(
            "DataExplorer::plot_correlation()",
            cs_cor_DataExplorer_ui(ns("cs_cor_DataExplorer_module"))
          ),
          tabPanel(
            "corrplot::corrplot()",
            cs_cor_corrplot_ui(ns("cs_cor_corrplot_module"))
          ),
          tabPanel(
            "GGally::ggpairs()",
            cs_cor_GGally_ui(ns("cs_cor_GGally_module"))
          ),
          tabPanel(
            "correlationfunnel::plot_correlation_funnel()",
            cs_cor_correlationfunnel_ui(ns("cs_cor_correlationfunnel_module"))
          ),
          tabPanel(
            "plotly::funs()",
            cs_cor_plotly_ui(ns("cs_cor_plotly_module"))
          )
        ),

        navbarMenu(
          "Multivariate",
          tabPanel(
            "PCA:FactoMineR",
            cs_PCA_FactoMineR_ui(ns("cs_PCA_FactoMineR_module"))
          ),
          tabPanel(
            "Cluster:factoextra",
            cs_cluster_factoextra_ui(ns("cs_cluster_factoextra_module"))
          )
        )
      )
    )
  )
}

#' Server function of cs_analysis
#'
#' @param tsbl_vars A tsibble of vars for cross-sectional analysis.
#'
#' @param debug A logic to enable/disable output for debug. Default FALSE
#'  means to disable output of debug.
#'
#' @describeIn cs_analysis  Server function of cs_analysis.
#' @return * Server function dosen't return value.
cs_analysis_server <- function(id, tsbl_vars, debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(tsbl_vars))

    slice_result <- slice_tsbl_server("slice_tsbl_module",
      tsbl_vars = tsbl_vars,
      slice_type = "cross_section",
      debug = debug
    )

    slice_tsbl_vars <- slice_result$slice_vars
    slice_tsbl_vars_average <- slice_result$slice_vars_average

    slice_csbl_vars <- reactive({
      slice_csbl_vars <- tsbl2csbl(slice_tsbl_vars())
      return(slice_csbl_vars)
    })


    date_range <- reactive({
      start_date <- min(unique(slice_tsbl_vars()$date), na.rm = TRUE)
      start_date <- format(start_date, "%Y-%m-%d")
      end_date <- max(unique(slice_tsbl_vars()$date), na.rm = TRUE)
      end_date <- format(end_date, "%Y-%m-%d")

      return(list(start_date = start_date, end_date = end_date))
    })

    # Render captions
    output$caption <- renderText({
      start_date <- date_range()$start_date
      end_date <- date_range()$end_date

      caption <- sprintf("Cross-sectional Analysis(%s~%s)", start_date, end_date)

      return(caption)
    })

    # Summary output ----

    output$base_summary_text <- renderPrint({
      slice_csbl_vars() %>%
        summary()
    })

    output$Hmisc_describe_text <- renderPrint({
      slice_csbl_vars() %>%
        Hmisc::describe()
    })

    output$fBasics_basic_table <- renderTable({
      continuous_vars <- slice_csbl_vars() %>%
        dplyr::select(where(is.numeric))

      suppressMessages({
        continuous_vars_summary <- continuous_vars %>%
          purrr::map_dfc(.f = ~ fBasics::basicStats(.x))
        names(continuous_vars_summary) <- names(continuous_vars)
      })

      continuous_vars_summary <- tibble::as_tibble(t(continuous_vars_summary), rownames = "Variable")

      continuous_vars_summary <- continuous_vars_summary %>%
        dplyr::select(-c("Sum")) %>%
        dplyr::rename(
          Quartile.1 = .data$`1. Quartile`,
          Quartile.3 = .data$`3. Quartile`,
          Mean.SE = .data$`SE Mean`,
          Mean.LCL = .data$`LCL Mean`,
          Mean.UCL = .data$`UCL Mean`
        )

      return(continuous_vars_summary)
    })

    output$DataExplorer_intro_table <- renderTable({
      slice_csbl_vars() %>%
        DataExplorer::introduce()
    })

    output$DataExplorer_intro_plot <- renderPlot({
      slice_csbl_vars() %>%
        DataExplorer::plot_intro()
    })


    output$skimr_skim_text <- renderPrint({
      slice_csbl_vars() %>%
        skimr::skim()
    })

    output$my_summary_table <- renderTable({
      continuous_vars <- slice_csbl_vars() %>%
        dplyr::select(where(is.numeric)) %>%
        tidyr::pivot_longer(
          cols = everything(),
          names_to = "variable", values_to = "value"
        )

      continuous_vars_summary <- continuous_vars %>%
        dplyr::group_by(.data$variable) %>%
        dplyr::summarise(
          dplyr::across(.data$value,
            .fns = list(
              obs = ~ mean(length(.x)),
              NAs = ~ sum(is.na(.x)),
              mean = mean,
              median = median,
              sd = sd,
              mad = mad,
              min = min,
              Q1 = ~ quantile(.x, probs = 0.25, na.rm = TRUE),
              Q3 = ~ quantile(.x, probs = 0.75, na.rm = TRUE),
              max = max,
              skewness = ~ PerformanceAnalytics::skewness(.x),
              kurtosis = ~ PerformanceAnalytics::kurtosis(.x, method = "moment")
            ),
            na.rm = TRUE,
            .names = "{fn}"
          )
        )

      return(continuous_vars_summary)
    })

    # Missing output ----

    # Draw missing data pattern by DataExplorer
    cs_missing_DataExplorer_server("cs_missing_DataExplorer_module",
      csbl_vars = slice_csbl_vars
    )

    # Draw missing data pattern by visdat
    cs_missing_visdat_server("cs_missing_visdat_module",
      csbl_vars = slice_csbl_vars
    )


    # Draw missing data pattern by mice
    cs_missing_mice_server("cs_missing_mice_module",
      csbl_vars = slice_csbl_vars
    )

    # Draw missing data pattern by naniar
    cs_missing_naniar_server("cs_missing_naniar_module",
      csbl_vars = slice_csbl_vars
    )

    # Draw missing data pattern by vim
    cs_missing_vim_server("cs_missing_vim_module",
      csbl_vars = slice_csbl_vars
    )

    # Distribution output ----

    # Draw distribution plots by DataExplorer
    cs_dist_DataExplorer_server("cs_dist_DataExplorer_module",
      csbl_vars = slice_csbl_vars
    )

    # Draw distribution plots by plotly
    cs_dist_plotly_server("cs_dist_plotly_module",
      csbl_vars = slice_csbl_vars
    )

    # Draw distribution plots by PerformanceAnalytics
    cs_dist_PerformanceAnalytics_server("cs_dist_PerformanceAnalytics_module",
      csbl_vars = slice_csbl_vars
    )

    # Correlation output ----

    # Draw DataExplorer correlation plot
    cs_cor_DataExplorer_server("cs_cor_DataExplorer_module",
      csbl_vars = slice_csbl_vars
    )

    # Draw corrplot correlation plot
    cs_cor_corrplot_server("cs_cor_corrplot_module",
      csbl_vars = slice_csbl_vars
    )

    # Draw GGally correlation plot
    cs_cor_GGally_server("cs_cor_GGally_module",
      csbl_vars = slice_csbl_vars
    )

    # Draw correlationfunnel correlation plot
    cs_cor_correlationfunnel_server(
      "cs_cor_correlationfunnel_module",
      csbl_vars = slice_csbl_vars
    )

    # Draw correlation plot by plotly
    cs_cor_plotly_server(
      "cs_cor_plotly_module",
      csbl_vars = slice_csbl_vars
    )

    # MDA output ----

    # PCA analysis
    cs_PCA_FactoMineR_server(
      "cs_PCA_FactoMineR_module",
      csbl_vars = slice_csbl_vars
    )

    # Cluster analysis
    cs_cluster_factoextra_server(
      "cs_cluster_factoextra_module",
      csbl_vars = slice_csbl_vars
    )
  })
}

#' Testing module app of cs_analysis
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @param debug A logic to enable debug or not, default is on_debug() which
#'  returns DEBUG environment variable.
#'
#' @describeIn cs_analysis  Testing App of cs_analysis.
cs_analysis_app <- function(use_online_data = FALSE, debug = on_debug()) {

  # Prepare data
  tsbl_vars <- load_tsbl_vars(use_online_data)

  ui <- fluidPage(
    cs_analysis_ui("cs_analysis_module", debug = debug)
  )
  server <- function(input, output, session) {
    slice_vars <- cs_analysis_server(
      "cs_analysis_module",
      tsbl_vars = reactive(tsbl_vars),
      debug = debug
    )
  }
  shinyApp(ui, server)
}
