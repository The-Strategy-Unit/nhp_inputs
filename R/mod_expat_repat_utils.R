mod_expat_repat_trend_plot <- function(df, include, values, start_year, title, scale = 10) {
  v <- dplyr::filter(df, .data$fyear == start_year)$pcnt

  ymax_ci <- values[[2]] * v / 100

  interval <- if (include) {
    ggplot2::annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = values[[1]] * v / 100,
      ymax = ymax_ci,
      colour = "#f9bf07",
      fill = ggplot2::alpha("#f9bf07", 0.2),
      na.rm = TRUE
    )
  }

  df |>
    ggplot2::ggplot(ggplot2::aes(as.factor(.data$fyear), .data$pcnt, group = 1)) +
    interval +
    ggplot2::geom_line() +
    ggplot2::geom_point(
      data = \(.x) dplyr::filter(.x, .data$fyear == start_year),
      colour = "red"
    ) +
    ggplot2::scale_x_discrete(
      labels = \(.x) stringr::str_replace(.x, "^(\\d{4})(\\d{2})$", "\\1/\\2")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      limits = c(0, max(ceiling(v * 2 * scale) / scale, ymax_ci))
    ) +
    ggplot2::labs(
      x = "Financial Year",
      y = title
    ) +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line("#9d928a", linetype = "dotted")
    )
}

mod_expat_repat_local_split_plot <- function(df, providers, dataset, start_year) {
  providers_df <- tibble::enframe(
    providers,
    value = "provider",
    name = "provider_name"
  )

  this_provider_name <- providers_df |>
    dplyr::filter(.data$provider == dataset) |>
    dplyr::pull(.data$provider_name)

  # in the call to ggplot2::after_scale we will always get a check warning for no visible binding for colour:
  # create a value temporarily to hide this
  colour <- NULL

  df |>
    dplyr::left_join(providers_df, by = c("provider")) |>
    dplyr::arrange(.data$provider_name) |>
    dplyr::mutate(
      dplyr::across(
        "provider_name",
        \(.x) {
          .x |>
            forcats::fct_na_value_to_level("Other") |>
            forcats::fct_relevel(this_provider_name) |>
            forcats::fct_relevel("Other", after = Inf)
        }
      ),
      label = glue::glue(
        .sep = "\n",
        "{.data$provider_name}",
        "{scales::comma(.data$n)} ({scales::percent(.data$pcnt)})"
      ),
    ) |>
    dplyr::arrange(dplyr::desc(.data$provider_name)) |>
    dplyr::mutate(
      label_pos = cumsum(.data$n) - .data$n / 2
    ) |>
    ggplot2::ggplot(ggplot2::aes(1, .data$n)) +
    ggplot2::geom_col(
      ggplot2::aes(
        colour = .data$provider_name,
        fill = ggplot2::after_scale(ggplot2::alpha(colour, 0.4)) # nolint
      ),
      position = "stack"
    ) +
    ggplot2::geom_label(
      ggplot2::aes(y = .data$label_pos, label = .data$label),
      fill = "#ffffff"
    ) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}

mod_expat_repat_nonlocal_n <- function(df) {
  df |>
    ggplot2::ggplot(
      ggplot2::aes(
        as.factor(.data$fyear),
        .data$n
      )
    ) +
    ggplot2::geom_col(position = "stack", colour = "#f9bf07", fill = "#fef2cd") +
    ggplot2::scale_x_discrete(
      labels = \(.x) stringr::str_replace(.x, "^(\\d{4})(\\d{2})$", "\\1/\\2")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma
    ) +
    ggplot2::labs(
      x = "Financial Year",
      y = "Number of spells delivered to non-local ICB residents"
    ) +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line("#9d928a", linetype = "dotted")
    )
}


mod_expat_repat_nonlocal_icb_map <- function(df) {
  pal <- leaflet::colorNumeric( # nolint
    viridis::viridis_pal()(3),
    df$pcnt
  )

  leaflet::leaflet(df) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(
      color = "#000000",
      weight = 1,
      opacity = 1,
      fillColor = ~ pal(pcnt),
      popup = ~ glue::glue("{icb22nm}: {n} ({scales::percent(pcnt)})")
    )
}
