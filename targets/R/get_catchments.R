get_catchments <- function(provider_successors_last_updated, pop_year_long) {
  con <- get_con("HESData")

  tbl_age_table <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling_reference", "age_groups"))

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients")) |>
    dplyr::filter(.data$LSOA11 %LIKE% "E%") |>
    dplyr::inner_join(tbl_age_table, by = c("ADMIAGE" = "age")) |>
    dplyr::group_by(
      .data$FYEAR,
      .data$LSOA11,
      .data$age_group,
      .data$SEX
    ) |>
    dplyr::count(.data$PROCODE3) |>
    dplyr::collect() |>
    dplyr::mutate(
      tot = sum(.data$n, na.rm = TRUE),
      p = n * 1.0 / .data$tot
    ) |>
    dplyr::ungroup() |>
    janitor::clean_names() |>
    dplyr::inner_join(
      pop_year_long,
      by = c("fyear", "sex", "age_group", "lsoa11")
    ) |>
    dplyr::mutate(pop_catch = .data$pop * .data$p) |>
    dplyr::count(
      .data$fyear,
      .data$sex,
      .data$age_group,
      provider = .data$procode3,
      wt = .data$pop_catch,
      name = "pop_catch"
    ) |>
    dplyr::arrange(.data$fyear, .data$provider, .data$sex, .data$age_group)
}
