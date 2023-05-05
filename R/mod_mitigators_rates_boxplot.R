rates_boxplot <- function(trend_data, plot_range, interval) {
  ggplot2::ggplot(trend_data, ggplot2::aes(x = "", y = .data$rate)) +
    interval +
    ggplot2::geom_boxplot(alpha = 0.2, outlier.shape = NA) +
    ggbeeswarm::geom_quasirandom(ggplot2::aes(colour = .data$is_peer)) +
    ggplot2::scale_colour_manual(values = c("TRUE" = "black", "FALSE" = "red")) +
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
