get_ip_dsr_data <- function(ip_age_sex_data, lkp_peers, catchments, lkp_euro_2013, strategies) {
  df <- ip_age_sex_data |>
    dplyr::filter(.data$strategy %in% strategies[["admission avoidance"]]) |>
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
    "general_los_reduction_elective",
    "general_los_reduction_emergency",
    "raid_ip",
    "stroke_early_supported_discharge",
    "virtual_wards_activity_avoidance_ari",
    "virtual_wards_activity_avoidance_heart_failure"
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

get_day_procedures_data <- function(ip_los_data, peers) {
  force(ip_los_data) # purely to take a dependency in targets

  con <- get_con("HESData")

  tb_dp <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling_reference", "day_procedure_opcs_codes"))

  tb_ipp <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients_procedures")) |>
    dplyr::filter(.data[["OPORDER"]] == 1) |>
    dplyr::select(-"FYEAR")

  tb_ip <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients")) |>
    dplyr::inner_join(tb_ipp, by = dplyr::join_by("EPIKEY")) |>
    dplyr::inner_join(tb_dp, by = dplyr::join_by("OPCODE" == "procedure_code")) |>
    dplyr::filter(
      .data[["ADMIMETH"]] %LIKE% "1%",
      .data[["CLASSPAT"]] %in% c("1", "2")
    )

  tb_opp <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "outpatients_procedures")) |>
    dplyr::filter(.data[["oporder"]] == 1)

  tb_op <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "outpatients")) |>
    dplyr::inner_join(tb_opp, by = dplyr::join_by("attendkey")) |>
    dplyr::inner_join(tb_dp, by = dplyr::join_by("opcode" == "procedure_code"))


  ip <- tb_ip |>
    dplyr::count(
      .data[["FYEAR"]],
      .data[["PROCODE3"]],
      .data[["CLASSPAT"]],
      .data[["type"]]
    ) |>
    dplyr::filter(
      .data[["fyear"]] >= 201112
    ) |>
    dplyr::collect() |>
    janitor::clean_names()

  op <- tb_op |>
    dplyr::count(
      .data[["fyear"]],
      .data[["procode3"]],
      .data[["type"]]
    ) |>
    dplyr::filter(
      .data[["fyear"]] >= 201112
    ) |>
    dplyr::collect()

  dplyr::bind_rows(
    op,
    dplyr::mutate(
      ip,
      v = ifelse(
        stringr::str_ends(.data[["type"]], "dc") & .data[["classpat"]] == "2",
        0,
        .data[["n"]]
      )
    )
  ) |>
    dplyr::summarise(
      .by = c("fyear", "procode3", "type"),
      dplyr::across(c("v", "n"), \(.x) sum(.x, na.rm = TRUE))
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
    dplyr::transmute(
      .data[["fyear"]],
      .data[["procode"]],
      strategy = paste0("day_procedures_", .data[["type"]]),
      .data[["peer"]],
      rate = .data[["v"]] / .data[["n"]],
      .data[["n"]]
    ) |>
    add_mean_rows()
}
