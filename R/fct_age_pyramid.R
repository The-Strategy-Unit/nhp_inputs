#' Create an age pyramid plot
#'
#' Generates a population pyramid visualization showing age and sex distribution.
#'
#' @param age_data A data frame containing age group, sex, and population count data.
#'   Expected columns: age_group, sex (1=Males, 2=Females), and n (count).
#'
#' @return A ggplot2 object representing the age pyramid.
#' @noRd
age_pyramid <- function(age_data) {
  age_data |>
    dplyr::mutate(
      dplyr::across("n", `*`, ifelse(.data$sex == 1, -1, 1)),
      dplyr::across(
        "sex",
        ~ forcats::fct_recode(as.character(.x), "Males" = "1", "Females" = "2")
      )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        .data$n,
        .data$age_group,
        colour = .data$sex,
        fill = ggplot2::after_scale(ggplot2::alpha(.data$colour, 0.4))
      )
    ) +
    ggplot2::geom_col(position = "stack", width = 1, na.rm = TRUE) +
    ggplot2::scale_colour_manual(
      values = c("Males" = "#5881c1", "Females" = "#ec6555")
    ) +
    ggplot2::scale_x_continuous(labels = purrr::compose(scales::comma, abs)) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(NULL)
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.background = ggplot2::element_blank()
    )
}
