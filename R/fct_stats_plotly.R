# plot functions for statistic ploting by plotly ----

#
#' Set scale of filling or color for comparison in plot of ggplot2
#'
#' This is a warrper of [ggplot2::scale_fill_manual()] and
#' [ggplot2::scale_color_manual()] to set two colors for comparing series.
#'
#' @param type  A character of setting type, "fill" for
#' [ggplot2::scale_fill_manual], "color" for [ggplot2::scale_color_manual()].
#' @param name  A character of name to display in title of legend box.
#' @param base_id  A character of name for base series.
#' @param compare_id A character of name for comparing series.
#' @param base_color A character of color for base series.
#' @param compare_color A character of color for comparing series.
#' @param base_label  A character of label for base series to display in legend.
#' default is base_id.
#' @param compare_label  A character of label for comparing series to display
#' in legend. default is compare_id.
#'
#' @family stats_plotly

#' @return
#' @examples
#' \dontrun{

#'    p <- ds_vars %>%
#'      ggplot(aes(x = ~QR)) +
#'      gg_scale_compare(
#'        type = "fill",
#'        base_id = "Origin", compare_id = "Select"
#'      ) +
#'      labs(y = NULL)
#'
#'    # Add original plot
#'    p <- p +
#'      geom_histogram(
#'        aes(fill = "Origin"),
#'        color = NA
#'      )
#'    # Add comparing plot
#'      p <- p +
#'        geom_histogram(
#'          data = ds_vars_compare,
#'          aes(fill = "Select"), color = NA,
#'        )
#' }
#'

#' @export
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


#' Interactively plot stats chart by plotly
#'
#' Plot statistic chart interactively by [`plotly`][plotly::plotly] package,
#'  an (MIT licensed) web-based interactive charting library.
#' @name stats_plotly
NULL


#  Plot frequency bar chart for a discrete variable in data frame.
#' @param ds_vars  A data.frame of data for plotting.
#' @param var_name A character of variable name.
#' @param ds_vars_compare A data.frame of data for plotting reference.
#' @param plot_method A character of method to plotting, "plot_ly" means
#'  to use plotly function, "ggplot" means to use [[ggplot2::ggplot()]] +
#'  [plotly::ggplotly()].
#' @param source_id a character string of length 1. Match the value of
#'  this string with the source argument in [plotly::event_data()] to retrieve
#'  the event data corresponding to a specific plot
#'  (shiny apps can have multiple plots).
#'
#' @family stats_plotly
#'
#' @return A plotly ojbect.
#' @describeIn stats_plotly frequency bar chart for a discrete variable
#' in data frame.
#' @export
freqbar_plotly <- function(ds_vars,
                           var_name,
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
#' @describeIn stats_plotly boxplot for a continuous variable in data frame.
#' @export
boxplot_plotly <- function(ds_vars,
                           var_name,
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
        gg_scale_compare(
          type = "color",
          base_id = "Origin", compare_id = "Select"
        ) +
        labs(x = NULL)

      # Add original plot
      p <- p +
        geom_boxplot(
          aes(color = "Origin"),
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
            aes(color = "Select")
          )
      }

      p <- p + coord_flip()


      p <- plotly::ggplotly(p, source = source_id)
    }
  )

  return(p)
}

# Plot histogram for a continuous variable in data frame.
#' @describeIn stats_plotly histogram for a continuous variable in data frame.
#' @export
hist_plotly <- function(ds_vars,
                        var_name,
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

  # Function to compute bins width for histogram
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
#' @describeIn stats_plotly density plot for a continuous variable in data frame.
#' @export
density_plotly <- function(ds_vars,
                           var_name,
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
#' @describeIn stats_plotly qq plot for a continuous variable in data frame.
#' @export
qqplot_plotly <- function(ds_vars,
                          var_name,
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
      # rlang::abort("plotly doesn't have implementation for qq plot.")

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
            aes(color = "Select"), show.legend = FALSE
          )
      }

      p <- plotly::ggplotly(p, source = source_id)
    }
  )

  return(p)
}

# Plot scatter plot for two continuous variable in data frame.
#' @param x_var_name A character of name of variable x.
#' @param y_var_name A character of name of variable y.
#' @describeIn stats_plotly scatter plot for two continuous variable in data frame.
#' @export
scatter_plotly <- function(ds_vars,
                           x_var_name,
                           y_var_name,
                           ds_vars_compare = NULL,
                           plot_method = c("plot_ly", "ggplot"),
                           source_id = paste0(
                             "scatter_", x_var_name, "_", y_var_name
                           )) {

  # Validate parameters
  assertive::assert_is_inherited_from(ds_vars, c("tbl_df", "tbl", "data.frame"))
  assertive::assert_is_character(x_var_name)
  assertive::assert_is_a_non_empty_string(x_var_name)
  assertive::assert_is_character(y_var_name)
  assertive::assert_is_a_non_empty_string(y_var_name)
  assertive::assert_is_identical_to_true(x_var_name != y_var_name)
  assertive::assert_is_character(source_id)
  assertive::assert_all_are_true(x_var_name %in% names(ds_vars))
  assertive::assert_all_are_true(y_var_name %in% names(ds_vars))
  assertive::assert_is_a_non_empty_string(source_id)
  if (!is.null(ds_vars_compare)) {
    assertive::assert_is_inherited_from(
      ds_vars_compare,
      c("tbl_df", "tbl", "data.frame")
    )
    assertive::assert_all_are_true(x_var_name %in% names(ds_vars_compare))
    assertive::assert_all_are_true(y_var_name %in% names(ds_vars_compare))
  }

  plot_method <- match.arg(plot_method)
  plotly_chart <- switch(plot_method,
    "plot_ly" = {
      # Original plot
      fit_origin <- loess(as.formula(paste0(y_var_name, "~", x_var_name)),
        data = ds_vars
      )
      ds_fit_origin <- data.frame(
        x = as.vector(fit_origin$x),
        fited = fit_origin$fitted
      )
      p <- ds_vars %>%
        plotly::plot_ly(
          x = as.formula(paste0("~", x_var_name)),
          y = as.formula(paste0("~", y_var_name)),
          source = source_id
        ) %>%
        plotly::add_markers(
          name = "Origin",
          alpha = 0.5
        ) %>%
        plotly::add_lines(
          name = "Origin-smooth",
          x = ~x, y = ~fited,
          data = ds_fit_origin,
          color = I("steelblue"),
          line = list(dash = "dashdot")
        )

      # Comparing plot
      if (!is.null(ds_vars_compare) &&
        (NROW(ds_vars_compare) > 0) &&
        (NROW(ds_vars_compare) != NROW(ds_vars))
      ) {
        fit_compare <- loess(as.formula(paste0(y_var_name, "~", x_var_name)),
          data = ds_vars_compare
        )

        ds_fit_compare <- data.frame(
          x = as.vector(fit_compare$x),
          fited = fit_compare$fitted
        )

        p <- p %>%
          plotly::add_markers(
            name = "Select",
            data = ds_vars_compare,
            color = I("red"),
            alpha = 0.5
          ) %>%
          plotly::add_lines(
            name = "Select-smooth",
            x = ~x, y = ~fited,
            data = ds_fit_compare,
            color = I("red"),
            line = list(dash = "dashdot")
          )
      }

      p <- p %>%
        plotly::layout(barmode = "overlay")
    },
    "ggplot" = {
      p <- ds_vars %>%
        ggplot(aes(
          x = .data[[x_var_name]],
          y = .data[[y_var_name]]
        )) +
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
        geom_point(
          na.rm = TRUE,
          aes(fill = "Origin"),
          color = NA,
          alpha = 0.5
        ) +
        geom_smooth(
          formula = y ~ x,
          na.rm = TRUE,
          method = "loess", se = FALSE,
          aes(color = "Origin"),
          show.legend = FALSE,
          linetype = "dotdash",
          size = 0.5
        )


      # Add comparing plot
      if (!is.null(ds_vars_compare) &&
        (NROW(ds_vars_compare) > 0) &&
        (NROW(ds_vars_compare) != NROW(ds_vars))
      ) {
        p <- p +
          geom_point(
            data = ds_vars_compare,
            na.rm = TRUE,
            aes(fill = "Select"), color = NA,
            alpha = 0.5
          ) +
          geom_smooth(
            formula = y ~ x,
            data = ds_vars_compare,
            na.rm = TRUE,
            method = "loess", se = FALSE,
            aes(color = "Select"),
            show.legend = FALSE,
            linetype = "dotdash",
            size = 0.5
          )
      }

      p <- plotly::ggplotly(p, source = source_id)
    }
  )

  return(p)
}

# Plot combo chart for a continuous variable and a discrete in data frame.
#' @param continuous_var_name A character of name of continuous variable.
#' @param discrete_var_name A character of name of discrete variable.
#' @param top_levels A integer of top levels to display, default 5 means levels
#'  with top 5 most frequencies will be dispaly.
#' @describeIn stats_plotly combo chart for a continuous variable and a discrete
#'  in data frame.
#' @export
combochat_plotly <- function(ds_vars,
                             continuous_var_name,
                             discrete_var_name,
                             ds_vars_compare = NULL,
                             top_levels = 5,
                             plot_method = c("plot_ly", "ggplot"),
                             source_id = paste0(
                               "scatter_", continuous_var_name, "_", discrete_var_name
                             )) {

  # Validate parameters
  assertive::assert_is_inherited_from(ds_vars, c("tbl_df", "tbl", "data.frame"))
  assertive::assert_is_character(continuous_var_name)
  assertive::assert_is_a_non_empty_string(continuous_var_name)
  assertive::assert_is_character(discrete_var_name)
  assertive::assert_is_a_non_empty_string(discrete_var_name)
  assertive::assert_is_identical_to_true(continuous_var_name != discrete_var_name)
  assertive::assert_is_character(source_id)
  assertive::assert_all_are_true(continuous_var_name %in% names(ds_vars))
  assertive::assert_all_are_true(discrete_var_name %in% names(ds_vars))
  assertive::assert_is_a_non_empty_string(source_id)
  if (!is.null(ds_vars_compare)) {
    assertive::assert_is_inherited_from(
      ds_vars_compare,
      c("tbl_df", "tbl", "data.frame")
    )
    assertive::assert_all_are_true(continuous_var_name %in% names(ds_vars_compare))
    assertive::assert_all_are_true(discrete_var_name %in% names(ds_vars_compare))
  }

  ds_vars <- ds_vars %>%
    dplyr::mutate(levels = forcats::fct_lump_n(
      .data[[discrete_var_name]],
      n = top_levels,
    )) %>%
    dplyr::mutate(
      levels = forcats::fct_reorder(levels,
        .data[[continuous_var_name]],
        .fun = median, na.rm = TRUE, .desc = FALSE
      )
    )

  plot_method <- match.arg(plot_method)
  plotly_chart <- switch(plot_method,
    "plot_ly" = {
      # Original plot
      p <- ds_vars %>%
        plotly::plot_ly(
          x = as.formula(paste0("~", continuous_var_name)),
          source = source_id,
          alpha = 0.01, boxpoints = FALSE
        ) %>%
        plotly::add_boxplot(
          y = ~levels,
          name = "Origin@levels"
        ) %>%
        plotly::add_boxplot(
          name = "Origin"
        )


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

      # Add original plot
      p <- ds_vars %>%
        ggplot(aes(y = .data[[continuous_var_name]]),
               alpha = 0.5,
               outlier.alpha = 0.01) +
        gg_scale_compare(
          type = "fill",
          base_id = "Origin", compare_id = "Select"
        ) +
        gg_scale_compare(
          type = "color",
          base_id = "Origin", compare_id = "Select"
        ) +
        labs(x = NULL)

      p <- p +
        geom_boxplot(
          aes( x = var, color = "Origin"),
          outlier.shape = NA
        ) +
        geom_boxplot(
          aes( x = levels, color = "Origin"),
          #outlier.shape = NA,
          outlier.color = "red",
          outlier.fill = "red",
          outlier.alpha = 0.01
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
            aes(x = var, color = "Select"),
            outlier.shape = NA
          )
      }

      p <- p + coord_flip()

      p <- plotly::ggplotly(p, source = source_id)
    }
  )

  return(p)
}
