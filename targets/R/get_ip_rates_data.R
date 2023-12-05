get_ip_dsr_data <- function(ip_age_sex_data, lkp_peers, catchments, lkp_euro_2013, strategies) {
  df <- ip_age_sex_data |>
    tidyr::complete(
      tidyr::crossing(
        fyear, # nolint
        age_group, # nolint
        sex, # nolint
        strategy # nolint
      ),
      .data[["procode"]],
      fill = list(n = 0)
    ) |>
    dplyr::inner_join(
      catchments,
      by = dplyr::join_by("fyear", "sex", "age_group", "procode" == "provider"),
      relationship = "many-to-one"
    ) |>
    dplyr::inner_join(
      lkp_euro_2013,
      by = dplyr::join_by("age_group", "sex")
    )

  dplyr::bind_rows(
    df,
    df |>
      dplyr::summarise(
        procode = "NATIONAL",
        dplyr::across(c("n", "pop_catch", "pop_euro"), sum),
        .by = c("fyear", "age_group", "sex", "strategy")
      )
  ) |>
    dplyr::summarise(
      rate = sum(.data[["n"]] / .data[["pop_catch"]] * .data[["pop_euro"]]) / sum(.data[["pop_euro"]]),
      n = sum(.data[["pop_catch"]]),
      .by = c("procode", "strategy", "fyear")
    ) |>
    dplyr::rename(peer = "procode") |>
    dplyr::inner_join(
      dplyr::bind_rows(
        lkp_peers,
        lkp_peers |>
          dplyr::distinct(.data[["procode"]]) |>
          dplyr::mutate(peer = "NATIONAL")
      ),
      by = dplyr::join_by("peer"),
      relationship = "many-to-many"
    ) |>
    dplyr::relocate("procode", .before = "peer") |>
    dplyr::mutate(
      dplyr::across(
        "peer",
        \(.x) ifelse(.x == "NATIONAL", NA, .x)
      )
    ) |>
    dplyr::mutate(
      dplyr::across("rate", \(.x) .x * 10^3)
    )
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
    dplyr::inner_join(peers, by = c("peer"), relationship = "many-to-many") |>
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
      rate = sum((.data$speldur > 0) * .data$n) / sum(.data$n),
      dplyr::across("n", sum),
      .groups = "drop"
    ) |>
    # ensure there is no statistical disclosure
    dplyr::filter(.data$n >= 5, .data$rate * .data$n >= 5, (1 - .data$rate) * .data$n >= 5) |>
    dplyr::inner_join(peers, by = c("peer"), relationship = "many-to-many") |>
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
    dplyr::left_join(
      n_preops,
      by = c("fyear", "procode", "strategy")
    ) |>
    dplyr::mutate(
      dplyr::across("preops", ~ tidyr::replace_na(.x, 0)),
      rate = .data$preops / .data$n
    ) |>
    dplyr::rename(peer = "procode") |>
    # ensure there is no statistical disclosure
    dplyr::filter(.data$n >= 5, .data$rate * .data$n >= 5, (1 - .data$rate) * .data$n >= 5) |>
    dplyr::inner_join(
      peers,
      by = c("peer"),
      relationship = "many-to-many"
    ) |>
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
    janitor::clean_names() |>
    dplyr::mutate(
      target_type = ifelse(
        stringr::str_starts(.data[["strategy"]], "bads_daycase"),
        "daycase",
        "outpatients"
      ),
      v = ifelse(
        .data[["admission_type"]] == .data[["target_type"]],
        0,
        .data[["n"]]
      )
    ) |>
    dplyr::summarise(
      dplyr::across(c("n", "v"), sum),
      rate = .data[["v"]] / .data[["n"]],
      .by = c("fyear", "procode3", "strategy")
    ) |>
    # ensure there is no statistical disclosure
    dplyr::filter(
      .data$n >= 5,
      .data[["n"]] - .data[["v"]] >= 5
    ) |>
    dplyr::rename("peer" = "procode3") |>
    dplyr::inner_join(
      peers,
      by = c("peer"),
      relationship = "many-to-many"
    ) |>
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
