#' cs_dist_plotly
#'
#' @description A shiny module for cs_dist_plotly.
#'
#' @details
#'  The module is an UI for user to display plots of ...
#'  by [`package_abc`][package_abc::package_abc] package.
#'
#' @name cs_dist_plotly
#'
#' @param id  An ID string of module to connecting UI function and Server
#'   function.
#'
#'
#' @examples
#' \dontrun{
#' # Set up control UI in app UI
#' ui <- fluidPage(
#'   cs_dist_plotly_ui("cs_dist_plotly_module")
#' )
#'
#' # Call control server in App server
#' server <- function(input, output, session) {
#'   cs_dist_plotly <- cs_dist_plotly_server(
#'     "cs_dist_plotly_module",
#'     csbl_vars = reactive(csbl_vars)
#'   )
#' }
#'
#' # Run testing App for integration testing
#' cs_dist_plotly_app()
#' }
#'
NULL

#' UI function of cs_dist_plotly
#'
#' @return * UI function doesn't return value.
#'
#' @describeIn cs_dist_plotly  UI function of cs_dist_plotly.
#' @importFrom shiny NS tagList
cs_dist_plotly_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      position = "right",
      sidebarPanel(
        width = 2,
        selectInput(
          inputId = ns("discrete_var"),
          label = strong("Discrete var:"),
          choices = ""
        ),
        selectInput(
          inputId = ns("continuous_var"),
          label = strong("Continuous var:"),
          choices = ""
        ),
        selectInput(
          inputId = ns("plot_method"),
          label = strong("Plot method:"),
          choices = c("plot_ly", "ggplot")
        ),
        actionButton(
          inputId = ns("clear_selection"),
          label = strong("Clear selection")
        )
      ),
      mainPanel(
        width = 10,
        fluidRow(
          box(
            title = "Discrete Varable", status = "primary",
            solidHeader = TRUE, collapsible = TRUE, width = 6,
            plotly::plotlyOutput(ns("discrete_freqbar"), height = 260)
          ),
          box(
            title = "Continuous Varable", status = "primary",
            solidHeader = TRUE, collapsible = TRUE, width = 6,
            tabsetPanel(
              id = ns("continuous_plot_tabs"),
              type = "tabs",
              tabPanel(
                "Boxplot",
                plotly::plotlyOutput(ns("continuous_boxplot"), height = 220)
              ),
              tabPanel(
                "Histogram",
                plotly::plotlyOutput(ns("continuous_hist"), height = 220)
              ),
              tabPanel(
                "Density",
                plotly::plotlyOutput(ns("continuous_density"), height = 220)
              ),
              tabPanel(
                "qq-plot",
                plotly::plotlyOutput(ns("continuous_qq"), height = 220)
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Selected Data ", status = "primary",
            solidHeader = TRUE, collapsible = TRUE, width = 12,
            DT::dataTableOutput(ns("selected_table"))
          )
        )
      )
    )
  )
}

#' Server function of cs_dist_plotly
#'
#' @param csbl_vars A tibble of vars of cross-section.
#'
#' @describeIn cs_dist_plotly  Server function of cs_dist_plotly.
#' @return * Server function return a data frame of ...
cs_dist_plotly_server <- function(id, csbl_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate parameters
    assertive::assert_all_are_true(is.reactive(csbl_vars))

    # Logic reactive ----

    # Record selection of plotly controls
    select_indicator <- reactiveValues()

    # Return selected data by plotly controls
    select_data <- reactive({

      # Build up final selection from plotly controls' selection
      final_selection <- rep(TRUE, NROW(csbl_vars()))
      for (control_selection in reactiveValuesToList(select_indicator)) {
        # Apply selection from a control only if it had some selection
        if (any(control_selection)) {
          final_selection <- final_selection & control_selection
        }
      }

      # Get selected data
      csbl_vars() %>%
        dplyr::filter(final_selection)
    })


    # Map the brush limits of plotly controls to selection ----

    # Map selection from discrete_freqbar
    observeEvent(plotly::event_data("plotly_selected",
      source = "discrete_freqbar"
    ), {
      origin_data <- csbl_vars()
      evt <- plotly::event_data("plotly_selected",
        source = "discrete_freqbar"
      )
      if (!is.null(evt)) {
        var_column <- origin_data[[isolate(input$discrete_var)]]
        if (is.character(evt$x)) {
          selection <- var_column %in% evt$x
        } else {
          selection <- var_column %in% sort(unique(var_column))[evt$x]
        }
        select_indicator$discrete_var <- selection
      }
    })


    # Map selection from continuous_boxplot
    observeEvent(plotly::event_data("plotly_brushed",
      source = "continuous_boxplot"
    ), {
      origin_data <- csbl_vars()
      evt <- plotly::event_data("plotly_brushed",
        source = "continuous_boxplot"
      )
      if (!is.null(evt)) {
        selection <- between(
          origin_data[[isolate(input$continuous_var)]],
          evt$x[[1]], evt$x[[2]]
        )
        select_indicator$continuous_var <- selection
      }
    })

    # Map selection from continuous_hist
    observeEvent(plotly::event_data("plotly_brushed",
      source = "continuous_hist"
    ), {
      origin_data <- csbl_vars()
      evt <- plotly::event_data("plotly_brushed",
        source = "continuous_hist"
      )
      if (!is.null(evt)) {
        selection <- between(
          origin_data[[isolate(input$continuous_var)]],
          evt$x[[1]], evt$x[[2]]
        )
        select_indicator$continuous_var <- selection
      }
    })

    # Map selection from continuous_density
    observeEvent(plotly::event_data("plotly_brushed",
      source = "continuous_density"
    ), {
      origin_data <- csbl_vars()
      evt <- plotly::event_data("plotly_brushed",
        source = "continuous_density"
      )
      if (!is.null(evt)) {
        selection <- between(
          origin_data[[isolate(input$continuous_var)]],
          evt$x[[1]], evt$x[[2]]
        )
        select_indicator$continuous_var <- selection
      }
    })

    # Map selection from continuous_qq
    observeEvent(plotly::event_data("plotly_brushed",
                                    source = "continuous_qq"
    ), {
      origin_data <- csbl_vars()
      evt <- plotly::event_data("plotly_brushed",
                                source = "continuous_qq"
      )
      if (!is.null(evt)) {
        browser()
        selection <- between(
          origin_data[[isolate(input$continuous_var)]],
          evt$y[[1]], evt$y[[2]]
        )
        select_indicator$continuous_var <- selection
      }
    })

    # Clear maping selection of ploty controls
    observeEvent(input$clear_selection, {
      select_indicator$discrete_var <- rep(
        FALSE,
        length(select_indicator$discrete_var)
      )
      select_indicator$continuous_var <- rep(
        FALSE,
        length(select_indicator$continuous_var)
      )
    })

    # Update controls ----

    # Update UI with dataset and user inputs
    observe({
      discrete_vars <- csbl_vars() %>%
        dplyr::select(where(~ !is.numeric(.x))) %>%
        names()

      continuous_vars <- csbl_vars() %>%
        dplyr::select(where(~ is.numeric(.x))) %>%
        names()

      # Set choices for select inputs
      updateSelectInput(
        session = session, inputId = "discrete_var",
        choices = discrete_vars
      )

      updateSelectInput(
        session = session, inputId = "continuous_var",
        choices = continuous_vars
      )
    })

    # Plot vars distribution ----
    output$discrete_freqbar <- plotly::renderPlotly({
      # focus_vars <- csbl_vars() %>%
      #   dplyr::select(where(~ !is.numeric(.x)))

      csbl_vars() %>%
        freqbar_plotly(
          var_name = req(input$discrete_var),
          ds_vars_compare = select_data(),
          plot_method = input$plot_method,
          source_id = "discrete_freqbar"
        ) %>%
        plotly::layout(
          dragmode = "select",
          selectdirection = "h"
        ) %>%
        plotly::event_register(event = "plotly_brushed")
    })

    output$continuous_boxplot <- plotly::renderPlotly({
      csbl_vars() %>%
        boxplot_plotly(
          var_name = req(input$continuous_var),
          ds_vars_compare = select_data(),
          plot_method = input$plot_method,
          source_id = "continuous_boxplot"
        ) %>%
        plotly::layout(
          dragmode = "select",
          selectdirection = "h"
        ) %>%
        plotly::event_register(event = "plotly_brushed") %>%
        plotly::event_register(event = "plotly_doubleclick")
    })

    output$continuous_hist <- plotly::renderPlotly({
      csbl_vars() %>%
        hist_plotly(
          var_name = req(input$continuous_var),
          ds_vars_compare = select_data(),
          plot_method = input$plot_method,
          source_id = "continuous_hist"
        ) %>%
        plotly::layout(
          dragmode = "select",
          selectdirection = "h"
        ) %>%
        plotly::event_register(event = "plotly_brushed")
    })

    output$continuous_density <- plotly::renderPlotly({
      csbl_vars() %>%
        density_plotly(
          var_name = req(input$continuous_var),
          ds_vars_compare = select_data(),
          plot_method = input$plot_method,
          source_id = "continuous_density"
        ) %>%
        plotly::layout(
          dragmode = "select",
          selectdirection = "h"
        ) %>%
        plotly::event_register(event = "plotly_brushed")
    })


    output$continuous_qq <- plotly::renderPlotly({

      csbl_vars() %>%
        qqplot_plotly(
          var_name = req(input$continuous_var),
          ds_vars_compare = select_data(),
          plot_method = input$plot_method,
          source_id = "continuous_qq"
        ) %>%
        plotly::layout(
          dragmode = "select",
          selectdirection = "d"
        ) %>%
        plotly::event_register(event = "plotly_brushed")

    })

    output$selected_table <- DT::renderDataTable({
      continuous_vars <- csbl_vars() %>%
        dplyr::select(where(~ is.numeric(.x))) %>%
        names()

      DT::datatable(
        select_data(),
        options = list(
          columnDefs = list(list(className = "dt-center")),
          pageLength = 5
        )
      ) %>%
        DT::formatRound(columns = continuous_vars, digits = 2)
    })
  })
}

#' Testing module app of cs_dist_plotly
#'
#' @param use_online_data A logical to determine whether to use test data from
#'  database or not. Default FALSE means to use achieved data for tests.
#'
#' @describeIn cs_dist_plotly  Testing App of cs_dist_plotly.
cs_dist_plotly_app <- function(use_online_data = FALSE) {

  # Prepare data
  csbl_vars <- load_csbl_vars(use_online_data = FALSE)

  ui <- fluidPage(
    cs_dist_plotly_ui("cs_dist_plotly_module")
  )
  server <- function(input, output, session) {
    cs_dist_plotly_server(
      "cs_dist_plotly_module",
      csbl_vars = reactive(csbl_vars)
    )
  }
  shinyApp(ui, server)
}

# Internal functions ------------------------------------------------------

# Comparing Scale of filling color for ggplot2
gg_scale_compare <- function(type = c("fill", "color"),
                             name = "",
                             base_id = "Origin",
                             compare_id = "Select",
                             base_color = "seagreen",
                             compare_color = "red",
                             base_label = base_id,
                             compare_label = compare_id) {
  values <- c(base_color, compare_color)
  names(values) <- c(base_id, compare_id)
  labels <- c(base_label, compare_label)
  names(labels) <- c(base_id, compare_id)

  type <- match.arg(type)
  switch(type,
    "fill" = {
      scale_fill_manual(
        name = name,
        values = values,
        labels = labels,
      )
    },
    "color" = {
      scale_color_manual(
        name = name,
        values = values,
        labels = labels,
      )
    }
  )
}

# Plot frequency bar chart for a discrete variable in data frame.
freqbar_plotly <- function(ds_vars, var_name,
                           ds_vars_compare = NULL,
                           plot_method = c("plot_ly", "ggplot"),
                           source_id = paste0("freqbar_", var_name)) {

  # Validate parameters
  assertive::assert_is_inherited_from(ds_vars, c("tbl_df", "tbl", "data.frame"))
  assertive::assert_is_character(var_name)
  assertive::assert_is_a_non_empty_string(var_name)
  assertive::assert_all_are_true(var_name %in% names(ds_vars))
  assertive::assert_is_character(source_id)
  assertive::assert_is_a_non_empty_string(source_id)
  if (!is.null(ds_vars_compare)) {
    assertive::assert_is_inherited_from(
      ds_vars_compare,
      c("tbl_df", "tbl", "data.frame")
    )
    assertive::assert_all_are_true(var_name %in% names(ds_vars_compare))
  }

  plot_method <- match.arg(plot_method)
  switch(plot_method,
    "plot_ly" = {
      # Original plot
      p <- ds_vars %>%
        plotly::plot_ly(
          x = as.formula(paste0("~", var_name)),
          name = var_name,
          source = source_id
        ) %>%
        plotly::add_histogram(name = "Origin")

      # Comparing plot
      if (!is.null(ds_vars_compare) &&
        (NROW(ds_vars_compare) > 0) &&
        (NROW(ds_vars_compare) != NROW(ds_vars))
      ) {
        p <- p %>%
          plotly::add_histogram(
            name = "Select",
            data = ds_vars_compare,
            color = I("red")
          )
      }

      p <- p %>%
        plotly::layout(barmode = "overlay")
    },
    "ggplot" = {
      p <- ds_vars %>%
        ggplot(aes(x = .data[[var_name]])) +
        gg_scale_compare(
          type = "fill",
          base_id = "Origin", compare_id = "Select"
        ) +
        labs(y = NULL)

      # Add original plot
      p <- p +
        geom_bar(aes(fill = "Origin"), color = NA) +
        theme(
          axis.text.x = element_text(angle = -90)
        )

      # Add comparing plot
      if (!is.null(ds_vars_compare) &&
        (NROW(ds_vars_compare) > 0) &&
        (NROW(ds_vars_compare) != NROW(ds_vars))
      ) {
        p <- p +
          geom_bar(
            data = ds_vars_compare,
            aes(fill = "Select"), color = NA
          )
      }

      p <- plotly::ggplotly(p, source = source_id)
    }
  )

  return(p)
}

# Plot boxplot for a continuous variable in data frame.
boxplot_plotly <- function(ds_vars, var_name,
                           ds_vars_compare = NULL,
                           plot_method = c("plot_ly", "ggplot"),
                           source_id = paste0("boxplot_", var_name)) {

  # Validate parameters
  assertive::assert_is_inherited_from(ds_vars, c("tbl_df", "tbl", "data.frame"))
  assertive::assert_is_character(var_name)
  assertive::assert_is_a_non_empty_string(var_name)
  assertive::assert_is_character(source_id)
  assertive::assert_all_are_true(var_name %in% names(ds_vars))
  assertive::assert_is_a_non_empty_string(source_id)
  if (!is.null(ds_vars_compare)) {
    assertive::assert_is_inherited_from(
      ds_vars_compare,
      c("tbl_df", "tbl", "data.frame")
    )
    assertive::assert_all_are_true(var_name %in% names(ds_vars_compare))
  }

  plot_method <- match.arg(plot_method)
  plotly_chart <- switch(plot_method,
    "plot_ly" = {
      # Original plot
      p <- ds_vars %>%
        plotly::plot_ly(
          x = as.formula(paste0("~", var_name)),
          source = source_id,
          alpha = 0.1, boxpoints = "suspectedoutliers"
        ) %>%
        plotly::add_boxplot(name = "Origin")

      # Comparing plot
      if (!is.null(ds_vars_compare) &&
        (NROW(ds_vars_compare) > 0) &&
        (NROW(ds_vars_compare) != NROW(ds_vars))
      ) {
        p <- p %>%
          plotly::add_boxplot(
            name = "Select",
            data = ds_vars_compare,
            color = I("red")
          )
      }

      p <- p %>%
        plotly::layout(barmode = "overlay")
    },
    "ggplot" = {
      ds_vars <- ds_vars %>%
        dplyr::mutate(var = "Origin")

      p <- ds_vars %>%
        ggplot(aes(y = .data[[var_name]], x = var)) +
        gg_scale_compare(
          type = "fill",
          base_id = "Origin", compare_id = "Select"
        ) +
        labs(x = NULL)

      # Add original plot
      p <- p +
        geom_boxplot(
          aes(fill = "Origin"),
          alpha = 0.5,
          outlier.fill = "Origin",
          outlier.alpha = 0.5
        )

      # Add comparing plot
      if (!is.null(ds_vars_compare) &&
        (NROW(ds_vars_compare) > 0) &&
        (NROW(ds_vars_compare) != NROW(ds_vars))
      ) {
        ds_vars_compare <- ds_vars_compare %>%
          dplyr::mutate(var = "Select")

        p <- p +
          geom_boxplot(
            data = ds_vars_compare,
            aes(fill = "Select"), alpha = 0.5
          )
      }

      p <- p + coord_flip()


      p <- plotly::ggplotly(p, source = source_id)
    }
  )

  return(p)
}

# Plot histogram for a continuous variable in data frame.
hist_plotly <- function(ds_vars, var_name,
                        ds_vars_compare = NULL,
                        plot_method = c("plot_ly", "ggplot"),
                        source_id = paste0("hist_", var_name)) {

  # Validate parameters
  assertive::assert_is_inherited_from(ds_vars, c("tbl_df", "tbl", "data.frame"))
  assertive::assert_is_character(var_name)
  assertive::assert_is_a_non_empty_string(var_name)
  assertive::assert_is_character(source_id)
  assertive::assert_all_are_true(var_name %in% names(ds_vars))
  assertive::assert_is_a_non_empty_string(source_id)
  if (!is.null(ds_vars_compare)) {
    assertive::assert_is_inherited_from(
      ds_vars_compare,
      c("tbl_df", "tbl", "data.frame")
    )
    assertive::assert_all_are_true(var_name %in% names(ds_vars_compare))
  }

  binwidth_fun <- function(x) {
    2 * IQR(x) / (length(x)^(1 / 3))
  }

  plot_method <- match.arg(plot_method)
  plotly_chart <- switch(plot_method,
    "plot_ly" = {
      # Original plot
      p <- ds_vars %>%
        plotly::plot_ly(
          x = as.formula(paste0("~", var_name)),
          source = source_id
        ) %>%
        plotly::add_histogram(name = "Origin")

      # Comparing plot
      if (!is.null(ds_vars_compare) &&
        (NROW(ds_vars_compare) > 0) &&
        (NROW(ds_vars_compare) != NROW(ds_vars))
      ) {
        p <- p %>%
          plotly::add_histogram(
            name = "Select",
            data = ds_vars_compare,
            color = I("red")
          )
      }

      p <- p %>%
        plotly::layout(barmode = "overlay")
    },
    "ggplot" = {
      p <- ds_vars %>%
        ggplot(aes(x = .data[[var_name]])) +
        gg_scale_compare(
          type = "fill",
          base_id = "Origin", compare_id = "Select"
        ) +
        labs(y = NULL)

      # Add original plot
      p <- p +
        geom_histogram(
          aes(fill = "Origin"),
          color = NA,
          binwidth = binwidth_fun
        )

      # Add comparing plot
      if (!is.null(ds_vars_compare) &&
        (NROW(ds_vars_compare) > 0) &&
        (NROW(ds_vars_compare) != NROW(ds_vars))
      ) {
        p <- p +
          geom_histogram(
            data = ds_vars_compare,
            aes(fill = "Select"), color = NA,
            binwidth = binwidth_fun
          )
      }

      p <- plotly::ggplotly(p, source = source_id)
    }
  )

  return(p)
}

# Plot density for a continuous variable in data frame.
density_plotly <- function(ds_vars, var_name,
                           ds_vars_compare = NULL,
                           plot_method = c("plot_ly", "ggplot"),
                           source_id = paste0("density_", var_name)) {

  # Validate parameters
  assertive::assert_is_inherited_from(ds_vars, c("tbl_df", "tbl", "data.frame"))
  assertive::assert_is_character(var_name)
  assertive::assert_is_a_non_empty_string(var_name)
  assertive::assert_is_character(source_id)
  assertive::assert_all_are_true(var_name %in% names(ds_vars))
  assertive::assert_is_a_non_empty_string(source_id)
  if (!is.null(ds_vars_compare)) {
    assertive::assert_is_inherited_from(
      ds_vars_compare,
      c("tbl_df", "tbl", "data.frame")
    )
    assertive::assert_all_are_true(var_name %in% names(ds_vars_compare))
  }

  # distribution for plotting reference line
  ds_origin_reference <- tibble::tibble(
    !!var_name := rnorm(
      n = NROW(ds_vars[[var_name]]),
      mean = mean(ds_vars[[var_name]], na.rm = TRUE),
      sd = sd(ds_vars[[var_name]], na.rm = TRUE)
    )
  )

  ds_compare_reference <- if (!is.null(ds_vars_compare)) {
    tibble::tibble(
      !!var_name := rnorm(
        n = NROW(ds_vars_compare[[var_name]]),
        mean = mean(ds_vars_compare[[var_name]], na.rm = TRUE),
        sd = sd(ds_vars_compare[[var_name]], na.rm = TRUE)
      )
    )
  } else {
    NULL
  }


  plot_method <- match.arg(plot_method)
  plotly_chart <- switch(plot_method,
    "plot_ly" = {
      # Original plot
      fit_origin <- density(na.omit(ds_vars[[var_name]]))
      fit_origin_reference <- density(ds_origin_reference[[var_name]])
      p <- plotly::plot_ly(
        source = source_id
      ) %>%
        plotly::add_lines(
          x = fit_origin$x, y = fit_origin$y,
          name = "Origin",
          color = I("steelblue"),
        ) %>%
        plotly::add_lines(
          x = fit_origin_reference$x, y = fit_origin_reference$y,
          name = "Origin-Normal",
          color = I("steelblue"),
          line = list(dash = "dot"),
        )

      # Comparing plot
      if (!is.null(ds_vars_compare) &&
        (NROW(ds_vars_compare) > 0) &&
        (NROW(ds_vars_compare) != NROW(ds_vars))
      ) {
        fit_compare <- density(na.omit(ds_vars_compare[[var_name]]))
        fit_compare_reference <- density(ds_compare_reference[[var_name]])

        p <- p %>%
          plotly::add_lines(
            x = fit_compare$x, y = fit_compare$y,
            name = "Select",
            color = I("red")
          ) %>%
          plotly::add_lines(
            x = fit_compare_reference$x, y = fit_compare_reference$y,
            name = "Select-Normal",
            color = I("red"),
            line = list(dash = "dot"),
          )
      }

      p <- p %>%
        plotly::layout(barmode = "overlay")
    },
    "ggplot" = {
      p <- ds_vars %>%
        ggplot(aes(x = .data[[var_name]])) +
        geom_vline(xintercept = 0, size = 0.2) +
        gg_scale_compare(
          type = "color",
          base_id = "Origin", compare_id = "Select"
        ) +
        labs(y = NULL)

      # Add original plot
      p <- p +
        geom_density(
          aes(color = "Origin"),
          fill = NA
        ) +
        geom_density(
          data = ds_origin_reference,
          aes(color = "Origin"), fill = NA,
          linetype = "dotted"
        )

      # Add comparing plot
      if (!is.null(ds_vars_compare) &&
        (NROW(ds_vars_compare) > 0) &&
        (NROW(ds_vars_compare) != NROW(ds_vars))
      ) {
        p <- p +
          geom_density(
            data = ds_vars_compare,
            aes(color = "Select"), fill = NA
          ) +
          geom_density(
            data = ds_compare_reference,
            aes(color = "Select"), fill = NA,
            linetype = "dotted"
          )
      }

      p <- plotly::ggplotly(p, source = source_id)
    }
  )

  return(p)
}

# Plot qq plot for a continuous variable in data frame.
qqplot_plotly <- function(ds_vars, var_name,
                          ds_vars_compare = NULL,
                          plot_method = c("plot_ly", "ggplot"),
                          source_id = paste0("qqplot_", var_name)) {

  # Validate parameters
  assertive::assert_is_inherited_from(ds_vars, c("tbl_df", "tbl", "data.frame"))
  assertive::assert_is_character(var_name)
  assertive::assert_is_a_non_empty_string(var_name)
  assertive::assert_is_character(source_id)
  assertive::assert_all_are_true(var_name %in% names(ds_vars))
  assertive::assert_is_a_non_empty_string(source_id)
  if (!is.null(ds_vars_compare)) {
    assertive::assert_is_inherited_from(
      ds_vars_compare,
      c("tbl_df", "tbl", "data.frame")
    )
    assertive::assert_all_are_true(var_name %in% names(ds_vars_compare))
  }


  plot_method <- match.arg(plot_method)
  plotly_chart <- switch(plot_method,
    "plot_ly" = {
      #rlang::abort("plotly doesn't have implementation for qq plot.")

      # Original plot
      qq_origin <- qqnorm(ds_vars[[var_name]], plot.it = FALSE)

      p <- plotly::plot_ly(
        source = source_id,
        alpha = 0.5
      ) %>%
        plotly::add_markers(
          x = qq_origin$x, y = qq_origin$y,
          name = "Origin",
          color = I("steelblue"),
        )

      # Comparing plot
      if (!is.null(ds_vars_compare) &&
          (NROW(ds_vars_compare) > 0) &&
          (NROW(ds_vars_compare) != NROW(ds_vars))
      ) {
        qq_compare <- qqnorm(ds_vars_compare[[var_name]], plot.it = FALSE)
        p <- p %>%
          plotly::add_markers(
            x = qq_compare$x, y = qq_compare$y,
            name = "Select",
            color = I("red")
          )
      }
    },
    "ggplot" = {

      p <- ds_vars %>%
        ggplot(aes(sample = .data[[var_name]])) +
        gg_scale_compare(
          type = "fill",
          base_id = "Origin", compare_id = "Select"
        ) +
        gg_scale_compare(
          type = "color",
          base_id = "Origin", compare_id = "Select"
        ) +
        labs(y = NULL)

      # Add original plot
      p <- p +
        stat_qq(
          aes(fill = "Origin"),
          color = NA, alpha = 0.5
        ) +
        stat_qq_line(aes(color = "Origin"), show.legend = FALSE)

      # Add comparing plot
      if (!is.null(ds_vars_compare) &&
        (NROW(ds_vars_compare) > 0) &&
        (NROW(ds_vars_compare) != NROW(ds_vars))
      ) {
        p <- p +
          stat_qq(
            data = ds_vars_compare,
            aes(fill = "Select"), color = NA, alpha = 0.5
          ) +
          stat_qq_line(
            data = ds_vars_compare,
            aes(color = "Select"), show.legend = FALSE)
      }

      p <- plotly::ggplotly(p, source = source_id)
    }
  )

  return(p)
}
