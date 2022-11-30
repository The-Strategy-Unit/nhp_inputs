
get_ip_dsr_data <- function(ip_age_sex, peers, catchments, lkp_euro_2013, strategies) {
  ip_age_sex <- ip_age_sex |>
    dplyr::filter(.data$strategy %in% strategies[["admission avoidance"]])

  dsr <- peers |>
    dplyr::inner_join(ip_age_sex, by = c("peer" = "procode")) |>
    dplyr::left_join(catchments, by = c("fyear", "sex", "age_group", "peer" = "provider")) |>
    dplyr::mutate(dplyr::across("pop_catch", tidyr::replace_na, 0)) |>
    dplyr::inner_join(lkp_euro_2013, by = c("sex", "age_group"))

  dsr <- dplyr::bind_rows(
    dsr,
    dsr |>
      dplyr::group_by(.data$procode, .data$strategy, .data$fyear) |>
      dplyr::summarise(
        dplyr::across(
          c("n", "pop_catch", "pop_euro"),
          sum
        ),
        .groups = "drop"
      )
  ) |>
    dplyr::group_by(.data$procode, .data$strategy, .data$fyear, .data$peer) |>
    dplyr::summarise(
      rate = sum(.data$n / .data$pop_catch * .data$pop_euro) / sum(.data$pop_euro),
      n = sum(.data$pop_catch),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$procode, .data$strategy, .data$fyear, .data$peer)

  dsr |>
    dplyr::group_by(.data$procode, .data$strategy, .data$fyear) |>
    dplyr::mutate(dplyr::across("rate", \(.x) .x / sum(.x * is.na(peer)))) |>
    dplyr::ungroup()
}

add_mean_rows <- function(data) {
  mean <- data |>
    dplyr::group_by(.data$fyear, .data$procode, .data$strategy) |>
    dplyr::summarise(
      rate = sum(.data$rate * .data$n) / sum(.data$n),
      dplyr::across("n", sum),
      .groups = "drop"
    )
  dplyr::bind_rows(data, mean)
}

get_mean_los_data <- function(ip_los_data, peers) {
  mean_los_reduction_strategies <- c(
    "emergency_elderly",
    "enhanced_recovery_bladder",
    "enhanced_recovery_breast",
    "enhanced_recovery_colectomy",
    "enhanced_recovery_hip",
    "enhanced_recovery_hysterectomy",
    "enhanced_recovery_knee",
    "enhanced_recovery_prostate",
    "enhanced_recovery_rectum",
    "excess_beddays_elective",
    "excess_beddays_emergency",
    "raid_ip",
    "stroke_early_supported_discharge"
  )

  ip_los_data |>
    dplyr::filter(.data$strategy %in% mean_los_reduction_strategies) |>
    tidyr::drop_na("speldur") |>
    dplyr::group_by(.data$fyear, peer = .data$procode, .data$strategy) |>
    dplyr::summarise(
      rate = sum(.data$speldur * .data$n) / sum(.data$n),
      dplyr::across("n", sum),
      .groups = "drop"
    ) |>
    dplyr::inner_join(peers, by = c("peer")) |>
    dplyr::select(
      "fyear",
      "procode",
      "strategy",
      "peer",
      "rate",
      "n"
    ) |>
    add_mean_rows()
}

get_zero_los_data <- function(ip_los_data, peers) {
  zero_los_strategies <- c(
    "ambulatory_emergency_care_low",
    "ambulatory_emergency_care_moderate",
    "ambulatory_emergency_care_high",
    "ambulatory_emergency_care_very_high"
  )

  ip_los_data |>
    dplyr::filter(.data$strategy %in% zero_los_strategies) |>
    tidyr::drop_na("speldur") |>
    dplyr::group_by(.data$fyear, peer = .data$procode, .data$strategy) |>
    dplyr::summarise(
      rate = sum((.data$speldur == 0) * .data$n) / sum(.data$n),
      dplyr::across("n", sum),
      .groups = "drop"
    ) |>
    # ensure there is no statistical disclosure
    dplyr::filter(.data$n >= 5, .data$rate * .data$n >= 5, (1 - .data$rate) * .data$n >= 5) |>
    dplyr::inner_join(peers, by = c("peer")) |>
    dplyr::select(
      "fyear",
      "procode",
      "strategy",
      "peer",
      "rate",
      "n"
    ) |>
    add_mean_rows()
}

get_preop_los_data <- function(ip_los_data, peers) {
  preop_los_strategies <- c(
    "pre-op_los_1-day",
    "pre-op_los_2-day"
  )

  con <- get_con("HESData")

  tbl_ip <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients"))
  tbl_procedures <- dplyr::tbl(con, "tbInpatientsProcedures")

  n_procedures <- tbl_ip |>
    dplyr::inner_join(tbl_procedures, by = c("EPIKEY", "FYEAR")) |>
    dplyr::filter(
      .data$ADMIMETH %LIKE% "1%",
      !.data$OPCODE %LIKE% "[UYZ]%",
      dplyr::between(.data$OPDATE, .data$ADMIDATE, .data$DISDATE)
    ) |>
    dplyr::count(.data$FYEAR, .data$PROCODE3) |>
    dplyr::collect() |>
    dplyr::ungroup() |>
    dplyr::rename(fyear = "FYEAR", procode = "PROCODE3")

  n_preops <- ip_los_data |>
    dplyr::filter(.data$strategy %in% preop_los_strategies) |>
    dplyr::count(.data$fyear, .data$procode, .data$strategy, wt = .data$n, name = "preops")

  n_procedures |>
    dplyr::mutate(strategy = list(preop_los_strategies), .before = "n") |>
    tidyr::unnest("strategy") |>
    dplyr::left_join(n_preops, by = c("fyear", "procode", "strategy")) |>
    dplyr::mutate(
      dplyr::across("preops", tidyr::replace_na, 0),
      rate = .data$preops / .data$n
    ) |>
    dplyr::rename(peer = "procode") |>
    # ensure there is no statistical disclosure
    dplyr::filter(.data$n >= 5, .data$rate * .data$n >= 5, (1 - .data$rate) * .data$n >= 5) |>
    dplyr::inner_join(peers, by = c("peer")) |>
    dplyr::select(
      "fyear",
      "procode",
      "strategy",
      "peer",
      "rate",
      "n"
    ) |>
    add_mean_rows()
}

get_bads_data <- function(ip_los_data, peers) {
  con <- get_con("HESData")

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "bads_admission_type_breakdowns")) |>
    dplyr::collect() |>
    tidyr::pivot_wider(names_from = "admission_type", values_from = "n", values_fill = 0) |>
    dplyr::mutate(
      value = ifelse(
        .data$strategy == "bads_outpatients",
        .data$daycase + .data$elective,
        .data$elective
      ),
      n = .data$daycase + .data$elective + .data$outpatients,
      rate = 1 - (.data$value / .data$n),
      split = dplyr::case_when(
        .data$strategy == "bads_outpatients_or_daycase" ~ .data$daycase / (.data$daycase + .data$outpatients),
        .data$strategy == "bads_outpatients" ~ 0,
        TRUE ~ 1
      )
    ) |>
    dplyr::select(
      fyear = "FYEAR",
      peer = "PROCODE3",
      "strategy",
      "rate",
      "n",
      "split"
    ) |>
    # ensure there is no statistical disclosure
    dplyr::filter(.data$n >= 5, .data$rate * .data$n >= 5, (1 - .data$rate) * .data$n >= 5) |>
    dplyr::inner_join(peers, by = c("peer")) |>
    dplyr::select(
      "fyear",
      "procode",
      "strategy",
      "peer",
      "rate",
      "n",
      "split"
    ) |>
    add_mean_rows()
}
