#' Calculate funnel plot data
#'
#' Calculates the limits for a funnel plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
calculate_funnel_plot_data <- function(data, peers, provider, fyear, strategy) {
  peers <- dplyr::filter(peers, .data$procode == .env$provider)

  dsr <- data |>
    dplyr::semi_join(peers, by = c("procode3" = "peer")) |>
    dplyr::filter(.data$fyear == .env$fyear, .data$strategy == .env$strategy)

  dsr_rates <- dplyr::bind_rows(
    dsr,
    dsr |>
      dplyr::summarise(
        dplyr::across(
          c(.data$sample_rate, .data$pop_catch, .data$pop_euro),
          sum
        ),
        .groups = "keep"
      )
  ) |>
    dplyr::group_by(.data$procode3) |>
    dplyr::mutate(
      rate_local = .data$sample_rate / .data$pop_catch,
      std_number = .data$rate_local * .data$pop_euro
    ) |>
    dplyr::summarise(
      dplyr::across(.data$pop_catch, sum),
      std_rate = sum(.data$std_number) / sum(.data$pop_euro),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$procode3)

  peer_rate <- dplyr::filter(dsr_rates, is.na(.data$procode3))$std_rate

  funnel_data <- dsr_rates |>
    tidyr::drop_na(.data$procode3) |>
    dplyr::mutate(
      sdev_pop_i = sqrt(abs(.env$peer_rate) / .data$pop_catch),
      z = (.data$std_rate - .env$peer_rate) / .data$sdev_pop_i,
      sigz = sd(.data$z, na.rm = TRUE),
      cl2 = 2 * .data$sdev_pop_i * .data$sigz,
      cl3 = 3 * .data$sdev_pop_i * .data$sigz,
      lower2 = peer_rate - .data$cl2,
      lower3 = peer_rate - .data$cl3,
      upper2 = peer_rate + .data$cl2,
      upper3 = peer_rate + .data$cl3,
      mean = .env$peer_rate,
      is_peer = .data$procode3 != .env$provider
    )

  structure(funnel_data, class = c("nhp_funnel_plot", class(funnel_data)))
}

#' @export
plot.nhp_funnel_plot <- function(x, ...) {
  ggplot2::ggplot(x, ggplot2::aes(.data$pop_catch, .data$std_rate)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$is_peer)) +
    ggrepel::geom_label_repel(ggplot2::aes(label = .data$procode3)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$lower2), linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = .data$lower3), linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = .data$upper2), linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = .data$upper3), linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = .data$mean), linetype = "dashed") +
    ggplot2::scale_colour_manual(values = c("TRUE" = "black", "FALSE" = "red")) +
    ggplot2::theme(legend.position = "none")
}
