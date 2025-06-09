#' Plot Activity Rate by IMD Quintile
#' @param dat A data.frame. The inequalities dataset filtered by scheme and
#'     year.
#' @param hrg_code Character. The four-character HRG code, like 'LB42'.
#' @param option Character. Correction optino chosen by the user.
#' @return A ggplot2 object.
mod_plot_rate_by_quintile <- function(
  dat,
  hrg_code,
  option = c("no_change", "zero_sum", "level_up", "level_down")
) {
  option <- match.arg(option)

  hrg_dat <- dat |> dplyr::filter(sushrg_trimmed == hrg_code)

  hrg_plot <- hrg_dat |>
    ggplot2::ggplot() +
    ggplot2::geom_point(
      ggplot2::aes(
        x = imd_quintile,
        y = activity_rate
      )
    ) +
    ggplot2::ylim(
      # we want the more extreme of the actual and fitted values, so that lines
      # for level_up and level_down will be visible on the plot canvas
      pmin(min(hrg_dat[["fitted_line"]]), min(hrg_dat[["activity_rate"]])),
      pmax(max(hrg_dat[["fitted_line"]]), max(hrg_dat[["activity_rate"]]))
    ) +
    ggplot2::labs(x = "IMD quintile", y = "Activity rate") +
    ggplot2::theme_minimal()

  hrg_plot <- hrg_plot +
    ggplot2::geom_abline(
      slope = hrg_dat |> dplyr::pull(slope) |> unique(),
      intercept = hrg_dat |> dplyr::pull(intercept) |> unique(),
      colour = "grey"
    )

  if (option != "no_change") {
    if (option == "zero_sum") y_val <- mean(hrg_dat[["activity_rate"]])
    if (option == "level_down") y_val <- min(hrg_dat[["fitted_line"]])
    if (option == "level_up") y_val <- max(hrg_dat[["fitted_line"]])

    hrg_plot <- hrg_plot +
      ggplot2::geom_abline(
        slope = 0,
        intercept = y_val,
        colour = "red"
      )
  }

  hrg_plot
}
