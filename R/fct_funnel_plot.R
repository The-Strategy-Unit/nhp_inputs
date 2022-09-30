#' Calculate funnel plot data
#'
#' Calculates the limits for a funnel plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
generate_rates_funnel_data <- function(data) {
  peer_rates <- data |>
    dplyr::filter(is.na(.data$peer)) |>
    dplyr::select(
      .data$fyear,
      mean = .data$rate
    )

  funnel_data <- data |>
    tidyr::drop_na(.data$peer) |>
    dplyr::inner_join(peer_rates, by = c("fyear")) |>
    dplyr::group_by(.data$fyear) |>
    dplyr::mutate(
      sdev_pop_i = sqrt(abs(.data$mean) / .data$n),
      z = (.data$rate - .data$mean) / .data$sdev_pop_i,
      sigz = sd(.data$z, na.rm = TRUE),
      cl2 = 2 * .data$sdev_pop_i * .data$sigz,
      cl3 = 3 * .data$sdev_pop_i * .data$sigz,
      lower2 = .data$mean - .data$cl2,
      lower3 = .data$mean - .data$cl3,
      upper2 = .data$mean + .data$cl2,
      upper3 = .data$mean + .data$cl3
    ) |>
    dplyr::ungroup()

  structure(funnel_data, class = c("nhp_funnel_plot", class(funnel_data)))
}

#' @export
plot.nhp_funnel_plot <- function(x, plot_range, ...) {
  ggplot2::ggplot(x, ggplot2::aes(.data$n, .data$rate)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$lower2), linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = .data$lower3), linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = .data$upper2), linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = .data$upper3), linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = .data$mean), linetype = "dashed") +
    ggplot2::geom_point(ggplot2::aes(colour = .data$is_peer)) +
    ggrepel::geom_text_repel(ggplot2::aes(label = .data$peer, colour = .data$is_peer)) +
    ggplot2::scale_colour_manual(values = c("TRUE" = "black", "FALSE" = "red")) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_y_continuous(limits = plot_range) +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none",
      panel.background = ggplot2::element_blank()
    )
}
