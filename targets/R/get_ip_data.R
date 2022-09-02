get_ip_age_sex_data <- function(provider_successors_last_updated) {
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
    dplyr::count(.data$FYEAR, .data$age_group, .data$SEX, .data$PROCODE3, .data$strategy, wt = .data$sample_rate) |>
    dplyr::collect() |>
    janitor::clean_names()
}

get_ip_dsr_data <- function(ip_age_sex, peers, catchments, lkp_euro_2013) {
  dsr <- peers |>
    # TODO: peers has duplicates?
    dplyr::distinct(.data$procode, .data$peer) |>
    dplyr::inner_join(ip_age_sex, by = c("peer" = "procode3")) |>
    dplyr::left_join(catchments, by = c("fyear", "sex", "age_group", "peer" = "provider")) |>
    dplyr::mutate(dplyr::across(.data$pop_catch, tidyr::replace_na, 0)) |>
    dplyr::inner_join(lkp_euro_2013, by = c("sex", "age_group"))

  dplyr::bind_rows(
    dsr,
    dsr |>
      dplyr::group_by(.data$procode, .data$strategy, .data$fyear) |>
      dplyr::summarise(
        dplyr::across(
          c(.data$n, .data$pop_catch, .data$pop_euro),
          sum
        ),
        .groups = "drop"
      )
  ) |>
    dplyr::group_by(.data$procode, .data$strategy, .data$fyear, .data$peer) |>
    dplyr::summarise(
      std_rate = sum(.data$n / .data$pop_catch * .data$pop_euro) / sum(.data$pop_euro),
      dplyr::across(.data$pop_catch, sum),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$procode, .data$strategy, .data$fyear, .data$peer)
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
    dplyr::mutate(p = .data$n * 1.0 / sum(.data$n, na.rm = TRUE)) |>
    dbplyr::window_order(dplyr::desc(.data$n)) |>
    dplyr::filter(dplyr::row_number() <= 6) |>
    dplyr::ungroup() |>
    dplyr::collect() |>
    janitor::clean_names()
}
