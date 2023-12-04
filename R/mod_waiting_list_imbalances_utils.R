mod_waiting_list_imbalances_table <- function(df) {
  readRDS(app_sys("app", "data", "rtt_specialties.Rds")) |>
    tibble::enframe("description", "tretspef") |>
    dplyr::inner_join(df, by = dplyr::join_by("tretspef")) |>
    dplyr::select(-"tretspef") |>
    tidyr::pivot_wider(
      names_from = "activity_type",
      values_from = c("count", "param")
    ) |>
    gt::gt(rowname_col = "description") |>
    gt::tab_spanner(
      "Inpatients",
      columns = tidyselect::ends_with("ip")
    ) |>
    gt::tab_spanner(
      "Outpatients",
      columns = tidyselect::ends_with("op")
    ) |>
    gt::cols_label(
      "count_ip" = "Baseline Count",
      "param_ip" = "Change",
      "count_op" = "Baseline Count",
      "param_op" = "Change"
    ) |>
    gt::fmt_number(
      tidyselect::starts_with("count"),
      decimals = 0
    ) |>
    gt::fmt_percent(
      tidyselect::starts_with("param")
    ) |>
    gt::sub_missing() |>
    gt::tab_options(
      row_group.border.top.width = gt::px(2),
      row_group.border.top.color = "black",
      row_group.border.bottom.color = "black",
      row_group.background.color = "#686f73"
    )
}
