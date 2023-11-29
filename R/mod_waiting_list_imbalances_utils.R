mod_waiting_list_imbalances_table <- function(df) {
  df |>
    dplyr::mutate(
      pcnt = .data[["value"]] / .data[["count"]]
    ) |>
    dplyr::select(-"change", -"value") |>
    tidyr::pivot_wider(
      names_from = "activity_type",
      values_from = c("count", "pcnt")
    ) |>
    gt::gt(rowname_col = "name", groupname_col = "activity_type") |>
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
      "pcnt_ip" = "Change",
      "count_op" = "Baseline Count",
      "pcnt_op" = "Change"
    ) |>
    gt::tab_options(
      row_group.border.top.width = gt::px(2),
      row_group.border.top.color = "black",
      row_group.border.bottom.color = "black",
      row_group.background.color = "#686f73"
    )
}
