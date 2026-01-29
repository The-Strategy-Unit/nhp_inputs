#' Create a rates trend plot
#'
#' Generates a line plot showing trends in rates over time with the baseline
#' year highlighted.
#'
#' @param trend_data A data frame containing rate data with columns for fyear
#'   (financial year) and rate.
#' @param baseline_year The baseline financial year to highlight.
#' @param plot_range Numeric vector of length 2 specifying y-axis limits.
#' @param y_axis_title Title for the y-axis.
#' @param x_axis_title Title for the x-axis.
#' @param number_format Function to format axis numbers.
#' @param interval ggplot2 layer to add interval visualization.
#'
#' @return A ggplot2 object representing the rates trend plot.
#' @noRd
rates_trend_plot <- function(
  trend_data,
  baseline_year,
  plot_range,
  y_axis_title,
  x_axis_title,
  number_format,
  interval
) {
  ggplot2::ggplot(
    trend_data,
    ggplot2::aes(as.factor(.data$fyear), .data$rate, group = 1)
  ) +
    interval +
    ggplot2::geom_line() +
    ggplot2::geom_point(
      data = \(.x) dplyr::filter(.x, .data$fyear == baseline_year),
      colour = "red"
    ) +
    ggrepel::geom_text_repel(
      data = \(.x) dplyr::filter(.x, .data$fyear == baseline_year),
      ggplot2::aes(label = "baseline", colour = "red")
    ) +
    ggplot2::scale_y_continuous(
      name = y_axis_title,
      labels = number_format
    ) +
    ggplot2::coord_cartesian(ylim = plot_range) +
    ggplot2::scale_x_discrete(
      labels = \(.x) stringr::str_replace(.x, "^(\\d{4})(\\d{2})$", "\\1/\\2")
    ) +
    ggplot2::labs(x = x_axis_title) +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line("#9d928a", linetype = "dotted")
    )
}
