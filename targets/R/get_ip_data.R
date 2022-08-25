get_ip_dsr_data <- function(provider_successors_last_updated, catchments, lkp_euro_2013) {
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  tbl_provider_successors <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "provider_successors"))

  tbl_age_table <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling_reference", "age_groups"))
  tbl_ip_strategies <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "strategies_grouped"))

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients")) |>
    dplyr::inner_join(tbl_provider_successors, by = c("PROCODE3" = "old_code")) |>
    dplyr::mutate(PROCODE3 = .data$new_code) |>
    dplyr::filter(.data$sex %in% c(1, 2)) |>
    dplyr::inner_join(tbl_age_table, by = c("ADMIAGE" = "age")) |>
    dplyr::inner_join(tbl_ip_strategies, by = c("EPIKEY")) |>
    dplyr::group_by(.data$FYEAR, .data$age_group, .data$SEX, .data$PROCODE3, .data$strategy) |>
    dplyr::summarise(n = n(), dplyr::across(.data$sample_rate, sum, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect() |>
    janitor::clean_names() |>
    dplyr::left_join(catchments, by = c("fyear", "sex", "age_group", "procode3" = "provider")) |>
    dplyr::mutate(dplyr::across(.data$pop_catch, tidyr::replace_na, 0)) |>
    dplyr::inner_join(lkp_euro_2013, by = c("sex", "age_group")) |>
    dplyr::group_by(.data$strategy, .data$fyear, .data$procode3) |>
    dplyr::mutate(
      rate_local = .data$n / .data$pop_catch,
      std_number = .data$rate_local * .data$pop_euro
    ) |>
    dplyr::summarise(
      std_rate = sum(.data$std_number) / sum(.data$pop_euro),
      .groups = "drop"
    ) |>
    dplyr::arrange(strategy, fyear, procode3)
}

get_ip_diag_data <- function(provider_successors_last_updated) {
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  tbl_provider_successors <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "provider_successors"))

  tbl_inpatients <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients")) |>
    dplyr::inner_join(tbl_provider_successors, by = c("PROCODE3" = "old_code")) |>
    dplyr::mutate(PROCODE3 = .data$new_code)

  tbl_ip_strategies <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "strategies_grouped"))
  tbl_inpatients_diagnoses <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients_diagnoses")) |>
    dplyr::filter(.data$DIAGORDER == 1) |>
    dplyr::mutate(
      dplyr::across(.data$DIAGNOSIS, LEFT, 3)
    )

  tbl_inpatients |>
    dplyr::inner_join(tbl_inpatients_diagnoses, by = c("FYEAR", "EPIKEY")) |>
    dplyr::inner_join(tbl_ip_strategies, by = c("EPIKEY")) |>
    dplyr::group_by(.data$FYEAR, .data$PROCODE3, .data$strategy, .data$DIAGNOSIS) |>
    dplyr::summarise(n = n(), .groups = "drop_last") |>
    dplyr::mutate(p = .data$n / sum(.data$n, na.rm = TRUE)) |>
    dbplyr::window_order(dplyr::desc(.data$n)) |>
    dplyr::filter(dplyr::row_number() <= 6) |>
    dplyr::ungroup() |>
    dplyr::collect()
}
