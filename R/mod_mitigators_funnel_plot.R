#' Calculate funnel plot data
#'
#' Calculates the limits for a funnel plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
generate_rates_funnel_data <- function(df) {
  df <- dplyr::arrange(df, .data$denominator)

  cl <- df$national_rate[[1]] # centre line
  stdev <- sqrt(cl / df$denominator)
  z_i <- (df$rate - cl) / stdev

  mr <- abs(diff(z_i)) # moving range
  ulmr <- 3.267 * mean(mr, na.rm = TRUE) # upper limit for moving range
  amr <- mean(mr[mr < ulmr], na.rm = TRUE) # average moving range, excluding outliers

  sigma_z <- amr / 1.128

  sd_fn <- \(x) sqrt(cl / x) * sigma_z
  cl_fn <- \(s) \(x) cl + s * sd_fn(x)

  calculations <- list(
    cl = cl,
    z = (df$rate - cl) / sd_fn(df$denominator),
    lcl3 = cl_fn(-3), # lower control limit
    ucl3 = cl_fn(3), # upper control limit
    lcl2 = cl_fn(-2),
    ucl2 = cl_fn(2)
  )

  structure(
    df,
    class = c("nhp_funnel_plot", class(df)),
    funnel_calculations = calculations
  )
}

#' @export
plot.nhp_funnel_plot <- function(x, plot_range, interval, x_axis_title, ...) {
  funnel_calculations <- attr(x, "funnel_calculations")

  cl_line_type <- "dashed"
  cl_colour <- "black"

  cl2_line_type <- "dashed"
  cl2_colour <- "black"

  cl3_line_type <- "dashed"
  cl3_colour <- "black"

  plot_x_range <- c(0, max(x$denominator) * 1.05)
  function_x_range <- plot_x_range * 1.2

  x |>
    ggplot2::ggplot(ggplot2::aes(.data$denominator, .data$rate)) +
    interval +
    ggplot2::geom_hline(
      yintercept = funnel_calculations$cl,
      colour = cl_colour,
      linetype = cl_line_type
    ) +
    ggplot2::geom_function(
      fun = funnel_calculations$lcl2,
      colour = cl2_colour,
      linetype = cl2_line_type,
      xlim = function_x_range
    ) +
    ggplot2::geom_function(
      fun = funnel_calculations$ucl2,
      colour = cl2_colour,
      linetype = cl2_line_type,
      xlim = function_x_range
    ) +
    ggplot2::geom_function(
      fun = funnel_calculations$lcl3,
      colour = cl3_colour,
      linetype = cl3_line_type,
      xlim = function_x_range
    ) +
    ggplot2::geom_function(
      fun = funnel_calculations$ucl3,
      colour = cl3_colour,
      linetype = cl3_line_type,
      xlim = function_x_range
    ) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$is_peer)) +
    ggrepel::geom_text_repel(
      data = dplyr::filter(x, !is.na(.data$is_peer)),
      ggplot2::aes(label = .data$provider, colour = .data$is_peer),
      max.overlaps = Inf # include all labels
    ) +
    ggplot2::scale_colour_manual(
      values = c("TRUE" = "black", "FALSE" = "red"),
      na.value = "lightgrey"
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_x_continuous(labels = scales::comma_format()) +
    ggplot2::coord_cartesian(xlim = plot_x_range, ylim = plot_range) +
    ggplot2::labs(x = x_axis_title) +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none",
      panel.background = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line("#9d928a", linetype = "dotted")
    )
}
