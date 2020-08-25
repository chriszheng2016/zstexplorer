#' cs_cor_corrplot
#'
#' @description A shiny module for cs_cor_corrplot.
#'
#' @details
#'  The module is an UI for user to display plots of correlation
#'  by [`corrplot`][corrplot::corrplot] package.
#'
#' @name cs_cor_corrplot
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_cor_corrplot_ui("cs_cor_corrplot_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_cor_corrplot <- cs_cor_corrplot_server("cs_cor_corrplot_module")
#' }
#'
#' # Run testing App for integration testing
#' cs_cor_corrplot_app()
#' }
#'
NULL

#' UI function of cs_cor_corrplot
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_cor_corrplot  UI function of cs_cor_corrplot.
#' @importFrom shiny NS tagList
cs_cor_corrplot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        3,
        selectInput(
          inputId = ns("corrplot_upper_plot"),
          label = strong("Upper Plot:"),
          choices = c(
            "circle", "square", "ellipse", "number", "shade",
            "color", "pie"
          ),
          selected = "circle"
        ),
        selectInput(
          inputId = ns("corrplot_lower_plot"),
          label = strong("Lower Plot:"),
          choices = c(
            "circle", "square", "ellipse", "number", "shade",
            "color", "pie"
          ),
          selected = "number"
        )
      ),
      column(
        3,
        selectInput(
          inputId = ns("cor_method"),
          label = strong("cor method:"),
          choices = c("pearson", "spearman")
        ),
        selectInput(
          inputId = ns("cor_use"),
          label = strong("cor use:"),
          choices = c(
            "everything",
            "all.obs",
            "complete.obs",
            "na.or.complete",
            "pairwise.complete.obs"
          ),
          selected = "pairwise.complete.obs"
        )
      ),
      column(
        3,
        selectInput(
          inputId = ns("corrplot_order"),
          label = strong("plot method:"),
          choices = c(
            "original",
            "AOE", "FPC", "hclust"
          ),
          selected = "hclust"
        ),
        selectInput(
          inputId = ns("corrplot_hclust_method"),
          label = strong("hclust.method:"),
          choices = c(
            "complete", "ward", "ward.D", "ward.D2",
            "single", "average", "mcquitty", "median",
            "centroid"
          )
        )
      ),
      column(
        3,
        selectInput(
          inputId = ns("corrplot_plotCI"),
          label = strong("plotCI:"),
          choices = c("n", "square", "circle", "rect")
        ),
        sliderInput(
          inputId = ns("corrplot_addrect"),
          label = strong("Segment Rects:"),
          min = 0,
          max = 5,
          value = 3,
          step = 1
        )
      )
    ),

    box(
      title = "Corrlation Plot", status = "primary",
      solidHeader = TRUE, collapsible = TRUE, width = 12,
      collapsed = FALSE,
      plotOutput(ns("corrplot_corrlation_plot"))
    )

  )
}

#' Server function of cs_cor_corrplot
#'
#' @describeIn cs_cor_corrplot  Server function of cs_cor_corrplot.
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @return * Server function doesn't return value.
cs_cor_corrplot_server <- function(id, csbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(csbl_vars))

    output$corrplot_corrlation_plot <- renderPlot({

      csbl_continous_vars <- csbl_vars() %>%
        dplyr::select(where(is.numeric))

      cor_method <- input$cor_method
      cor_use <- input$cor_use

      # Compute correlation matrix
      cor_matrix <- cor(csbl_continous_vars,
        use = cor_use,
        method = cor_method
      )

      # Test correlation matrix
      if (cor_method == "pearson") {
        cor_test <- corrplot::cor.mtest(csbl_continous_vars,
          conf.level = 0.95,
          use = cor_use,
          method = cor_method
        )
      } else {
        # cor_test only available to "pearson"
        cor_test <- NULL
      }

      # Draw a customized corrplot
      corrplot_mixed(cor_matrix,
        cor_test = cor_test,
        lower = input$corrplot_lower_plot,
        upper = input$corrplot_upper_plot,
        order = input$corrplot_order,
        hclust.method = input$corrplot_hclust_method,
        addrect = input$corrplot_addrect,
        plotCI = input$corrplot_plotCI
      )
    })
  })
}

#' Testing module app of cs_cor_corrplot
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_cor_corrplot  Testing App of cs_cor_corrplot.
cs_cor_corrplot_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data)

  ui <- fluidPage(
    cs_cor_corrplot_ui("cs_cor_corrplot_module")
  )
  server <- function(input, output, session) {
    cs_cor_corrplot_server("cs_cor_corrplot_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}

# Internal functions ------------------------------------------------------

#' Customized corrplot
#'
#' @param cor_matrix Matrix, the correlation matrix to visualize.
#' @param cor_test A list of matrix return by [corrplot::cor.mtest()] which
#'   contains:
#'
#' * p:	Square matrix of size FxF with p-values as cells.
#' * lowCI:	Square matrix of size FxF, each cell represents the lower part of
#'  a confidence interval.
#' * uppCI:	Square matrix of size FxF, each cell represents the upper part of
#'  a confidence interval.
#' @param lower   A Character, the visualization method for the lower triangular
#'   correlation matrix.
#' @param upper   A Character, the visualization method for the upper triangular
#'   correlation matrix.
#' @param lower.col Passed as col parameter to the lower matrix.
#' @param upper.col Passed as col parameter to the upper matrix.
#' @param order Character, the ordering method of the correlation matrix.
#'       "original" for original order (default).
#'          * "AOE" for the angular order of the eigenvectors.
#'          * "FPC" for the first principal component order.
#'          * "hclust" for the hierarchical clustering order.
#'          * "alphabet" for alphabetical order.
#' See order parameter in [corrplot::corrplot()].
#' @param hclust.method  A Character, the agglomeration method to be used when
#'   order is [stats::hclust()]. This should be one of "ward", "ward.D",
#'   "ward.D2", "single", "complete", "average", "mcquitty", "median" or
#'   "centroid".See hclust.method parameter in [corrplot::corrplot()].
#' @param addrect A Integer, the number of rectangles draws on the graph
#'   according to the hierarchical cluster, only valid when order is `hclust`.
#'   If NULL (default), then add no rectangles.
#' @param plotCI 	See the plotCI parameter in [corrplot::corrplot()].
#' @param bg  The background color.
#' @param mar See [graphics::par()].
#' @param ...  Additional arguments for [corrplot::corrplot()]'s wrappers.
#'
#' @return   (Invisibly) returns a reordered correlation matrix.
#'
#' @examples
#' \dontrun{
#'
#' # Prepare cor matrix
#' cor_matrix <- cor(mtcars)
#' cor_test <- corrplot::cor.mtest(cor_matrix)
#'
#' # Plot a customized corplot wich cluster segments
#' corrplot_mixed(cor_matrix,
#'   cor_test = cor_test,
#'   order = "hclust", addrect = 3
#' )
#'
#' # Plot a customized corplot wich cluster segments and CI.
#' corrplot_mixed(cor_matrix,
#'   cor_test = cor_test,
#'   order = "hclust", addrect = 3,
#'   plotCI = "rect"
#' )
#' }
#'
corrplot_mixed <- function(cor_matrix,
                           cor_test = NULL,
                           lower = "number", upper = "circle",
                           lower.col = NULL, upper.col = NULL,
                           order = c(
                             "original",
                             "AOE", "FPC", "hclust", "alphabet"
                           ),
                           hclust.method = c(
                             "complete", "ward", "ward.D", "ward.D2",
                             "single", "average", "mcquitty", "median",
                             "centroid"
                           ),
                           addrect = NULL,
                           plotCI = c("n", "square", "circle", "rect"),
                           bg = "white",
                           mar = c(0, 0, 0, 0), ...) {
  adjust_plotCI <- function(plotCI, method) {

    # plotCI is only valid when method is "circle" or "square"
    if (method %in% c("circle", "square")
    ) {
      return(plotCI)
    }
    return("n")
  }
  plotCI_lower <- adjust_plotCI(plotCI, lower)
  plotCI_upper <- adjust_plotCI(plotCI, upper)
  oldpar <- par(mar = mar, bg = "white")
  on.exit(par(oldpar), add = TRUE)

  # Upper part corrplot
  corrplot::corrplot(cor_matrix,
    type = "full",
    method = upper,
    diag = FALSE,
    col = upper.col,
    tl.pos = "d", cl.pos = "r",
    order = order,
    p.mat = cor_test$p, insig = "pch", pch.col = "red", sig.level = 0.05,
    addrect = addrect, rect.col = "navy",
    plotCI = plotCI_upper,
    lowCI.mat = cor_test$lowCI, uppCI.mat = cor_test$uppCI
  )

  # Lower part of corrplot
  corrplot::corrplot(cor_matrix,
    add = TRUE,
    type = "lower",
    method = lower,
    diag = FALSE,
    col = lower.col,
    tl.pos = "n", cl.pos = "n",
    order = order,
    p.mat = cor_test$p, insig = "pch", pch.col = "red", sig.level = 0.05,
    addrect = addrect, rect.col = "navy",
    plotCI = plotCI_lower,
    lowCI.mat = cor_test$lowCI, uppCI.mat = cor_test$uppCI
  )

  invisible(cor_matrix)
}
