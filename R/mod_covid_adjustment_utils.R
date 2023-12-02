mod_covid_adjustment_table <- function(covid_adjustment) {
  covid_adjustment |>
    purrr::map_depth(2, ~ purrr::set_names(.x, c("low", "high"))) |>
    purrr::map(tibble::enframe) |>
    dplyr::bind_rows(.id = "activity_type") |>
    dplyr::mutate(
      dplyr::across(
        "activity_type",
        ~ forcats::fct_recode(
          .x,
          "Accident and Emergency" = "aae",
          "Inpatients" = "ip",
          "Outpatients" = "op"
        )
      )
    ) |>
    tidyr::unnest_wider("value") |>
    gt::gt(rowname_col = "name", groupname_col = "activity_type") |>
    gt::tab_spanner(
      "80% Confidence Interval",
      columns = c("low", "high")
    ) |>
    gt::cols_label(
      "low" = "Low Estimate",
      "high" = "High Estimate"
    ) |>
    gt::tab_options(
      row_group.border.top.width = gt::px(2),
      row_group.border.top.color = "black",
      row_group.border.bottom.color = "black",
      row_group.background.color = "#686f73"
    )
}
