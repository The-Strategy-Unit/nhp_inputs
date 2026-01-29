#' Create waiting list imbalances table
#'
#' Generates a formatted gt table displaying waiting list baseline counts and
#' parameter changes by specialty and activity type.
#'
#' @param df A data frame containing specialty data with columns for tretspef,
#'   activity_type, count, and param.
#'
#' @return A gt table object.
#' @noRd
mod_waiting_list_imbalances_table <- function(df) {
  rtt_specialties() |>
    dplyr::inner_join(df, c(code = "tretspef")) |>
    dplyr::select(-"code") |>
    tidyr::pivot_wider(
      names_from = "activity_type",
      values_from = c("count", "param")
    ) |>
    gt::gt(rowname_col = "specialty") |>
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
