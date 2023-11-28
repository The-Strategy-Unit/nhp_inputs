mod_non_demographic_adjustment_table <- function(non_demographic_adjustment) {
  non_demographic_adjustment |>
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
      "Adjustment factor",
      columns = c("low", "high")
    ) |>
    gt::cols_label(
      "low" = "Low adjustment",
      "high" = "High adjustment"
    ) |>
    gt::tab_options(
      row_group.border.top.width = gt::px(2),
      row_group.border.top.color = "black",
      row_group.border.bottom.color = "black",
      row_group.background.color = "#686f73"
    )
}

nda_groups <- list(
  "ip" = list(
    name = "Inpatients",
    values = c(
      "elective" = "Elective",
      "non-elective" = "Non-Elective",
      "maternity" = "Maternity"
    )
  ),
  "op" = list(
    name = "Outpatients",
    values = c(
      "first" = "First",
      "followup" = "Follow-up",
      "procedure" = "Procedure"
    )
  ),
  "aae" = list(
    name = "Accident and Emergency",
    values = c(
      "ambulance" = "Ambulance",
      "walk-in" = "Walk-in"
    )
  )
)
