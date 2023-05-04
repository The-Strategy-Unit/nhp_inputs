get_covid_adjustment_data_ip <- function(strategies_last_updated, provider_successors_last_updated) {
  force(strategies_last_updated)
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients")) |>
    dplyr::filter(.data$fyear >= 201617, .data$fyear < 202021) |>
    dplyr::count(
      procode = .data$PROCODE3,
      group = dplyr::case_when(
        .data$ADMIMETH %LIKE% "1%" ~ "elective",
        .data$ADMIMETH %LIKE% "3%" ~ "maternity",
        .default = "non-elective"
      ),
      year = .data$FYEAR,
      month = MONTH(.data$ADMIDATE)
    ) |>
    dplyr::arrange(.data$procode, .data$group, .data$year, .data$month) |>
    dplyr::collect()
}

get_covid_adjustment_data_op <- function(strategies_last_updated, provider_successors_last_updated) {
  force(strategies_last_updated)
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "outpatients")) |>
    dplyr::filter(.data$fyear >= 201617, .data$fyear < 202021) |>
    dplyr::count(
      procode = .data$procode3,
      group = dplyr::case_when(
        .data$has_procedures == 1 ~ "procedure",
        .data$is_first == 1 ~ "first",
        .default = "followup"
      ),
      year = .data$fyear,
      month = MONTH(.data$apptdate)
    ) |>
    dplyr::arrange(.data$procode, .data$group, .data$year, .data$month) |>
    dplyr::collect()
}

get_covid_adjustment_data_aae <- function(strategies_last_updated, provider_successors_last_updated) {
  force(strategies_last_updated)
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "aae")) |>
    dplyr::filter(.data$fyear >= 201617, .data$fyear < 202021) |>
    dplyr::count(
      procode = .data$procode3,
      group = ifelse(.data$aearrivalmode == 1, "ambulance", "walk-in"),
      year = .data$fyear,
      month = MONTH(.data$arrivaldate)
    ) |>
    dplyr::arrange(.data$procode, .data$group, .data$year, .data$month) |>
    dplyr::collect()
}

get_covid_adjustment <- function(ip_df, op_df, aae_df) {
  .data <- list()

  dplyr::bind_rows(
    ip = ip_df,
    op = op_df,
    aae = aae_df,
    .id = "activity_type"
  ) |>
    dplyr::group_by(
      dplyr::across(c("procode", "activity_type", "group", "year"))
    ) |>
    dplyr::mutate(year_total = sum(.data$n, na.rm = TRUE)) |>
    dplyr::group_by(
      dplyr::across(c("procode", "activity_type", "group"))
    ) |>
    dplyr::filter(.data$month %in% c(2, 3)) |>
    dplyr::mutate(
      wdays = dplyr::case_when(
        .data$month == 3 ~ 31,
        floor(.data$year / 100) %% 4 == 0 ~ 29,
        .default = 28
      ),
      x = .data$n / .data$wdays,
      month = month.abb[.data$month]
    ) |>
    dplyr::select(
      "procode",
      "activity_type",
      "group",
      "year",
      "month",
      "x",
      "year_total"
    ) |>
    tidyr::pivot_wider(names_from = "month", values_from = "x") |>
    dplyr::mutate(
      diff = .data$Mar / .data$Feb,
      a = dplyr::lag(dplyr::cummean(.data$diff))
    ) |>
    dplyr::filter(.data$year == 201920) |>
    dplyr::mutate(
      new_mar = (.data$Feb * .data$a - .data$Mar) * 31,
      covid_adjustment = 1 + .data$new_mar / .data$year_total
    ) |>
    dplyr::ungroup() |>
    dplyr::select("procode", "activity_type", "group", "covid_adjustment") |>
    tidyr::drop_na() |>
    # convert to list
    dplyr::group_by(dplyr::across(-"covid_adjustment")) |>
    dplyr::summarise(dplyr::across("covid_adjustment", list), .groups = "drop_last") |>
    dplyr::group_nest() |>
    dplyr::mutate(dplyr::across("data", ~ purrr::map(.x, tibble::deframe))) |>
    dplyr::group_nest(.data[["procode"]]) |>
    dplyr::mutate(dplyr::across("data", ~ purrr::map(.x, tibble::deframe))) |>
    tibble::deframe()
}
