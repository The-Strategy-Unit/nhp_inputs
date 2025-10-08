#' Create a boxplot of rates data
#'
#' Generates a boxplot visualization with beeswarm points showing the
#' distribution of rates data.
#'
#' @param trend_data A data frame containing rate data with columns for rate
#'   and is_peer indicator.
#' @param plot_range Numeric vector of length 2 specifying y-axis limits.
#' @param interval ggplot2 layer to add interval visualization.
#'
#' @return A ggplot2 object representing the rates boxplot.
#' @noRd
rates_boxplot <- function(trend_data, plot_range, interval) {
  trend_data |>
    ggplot2::ggplot(ggplot2::aes(x = "", y = .data$rate)) +
    interval +
    ggplot2::geom_boxplot(alpha = 0.2, outlier.shape = NA) +
    ggbeeswarm::geom_quasirandom(ggplot2::aes(colour = .data$is_peer)) +
    ggplot2::scale_colour_manual(
      values = c("TRUE" = "black", "FALSE" = "red"),
      na.value = "lightgrey"
    ) +
    ggplot2::coord_cartesian(ylim = plot_range) +
    ggplot2::labs(x = "") +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none",
      panel.background = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line("#9d928a", linetype = "dotted")
    )
}
