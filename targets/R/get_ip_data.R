get_ip_age_sex_data <- function(strategies_last_updated, provider_successors_last_updated) {
  force(strategies_last_updated)
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  tbl_age_table <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling_reference", "age_groups"))
  tbl_ip_strategies <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "strategies"))

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients")) |>
    dplyr::filter(.data$sex %in% c("1", "2")) |>
    dplyr::inner_join(tbl_age_table, by = c("ADMIAGE" = "age")) |>
    dplyr::inner_join(tbl_ip_strategies, by = c("EPIKEY")) |>
    dplyr::count(
      .data$FYEAR,
      .data$age_group,
      .data$SEX,
      procode = .data$PROCODE3,
      .data$strategy,
      wt = .data$sample_rate
    ) |>
    dplyr::arrange(.data$age_group) |>
    dplyr::collect() |>
    janitor::clean_names() |>
    # ensure there is no statistical disclosure
    dplyr::group_by(.data$fyear, .data$procode, .data$strategy) |>
    dplyr::filter(sum(.data$n) >= 5) |>
    dplyr::ungroup() |>
    dplyr::mutate(dplyr::across("age_group", forcats::fct_inorder)) |>
    dplyr::arrange(.data$fyear, .data$procode, .data$strategy, .data$age_group, .data$sex)
}

get_ip_diag_data <- function(strategies_last_updated, provider_successors_last_updated) {
  force(strategies_last_updated)
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  tbl_inpatients <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients"))

  tbl_ip_strategies <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "strategies"))
  tbl_inpatients_diagnoses <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients_diagnoses")) |>
    dplyr::filter(.data$DIAGORDER == 1) |>
    dplyr::mutate(
      dplyr::across("DIAGNOSIS", \(.x) LEFT(.x, 3)) # nolint
    )

  tbl_inpatients |>
    dplyr::inner_join(tbl_inpatients_diagnoses, by = c("FYEAR", "EPIKEY")) |>
    dplyr::inner_join(tbl_ip_strategies, by = c("EPIKEY")) |>
    dplyr::group_by(.data$FYEAR, .data$PROCODE3, .data$strategy, .data$DIAGNOSIS) |>
    dplyr::summarise(n = sum(.data$sample_rate, na.rm = TRUE), .groups = "drop_last") |>
    dplyr::mutate(p = .data$n * 1.0 / sum(.data$n, na.rm = TRUE)) |>
    dbplyr::window_order(dplyr::desc(.data$n)) |>
    dplyr::filter(
      dplyr::row_number() <= 6,
      .data$n >= 5
    ) |>
    dplyr::ungroup() |>
    dplyr::collect() |>
    janitor::clean_names() |>
    dplyr::rename(procode = "procode3")
}

get_ip_los_data <- function(strategies_last_updated, provider_successors_last_updated) {
  force(strategies_last_updated)
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  tbl_inpatients <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients"))

  tbl_strategy_lookups <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling_reference", "strategy_lookups")) |>
    dplyr::filter(.data$strategy_type == "los reduction")

  tbl_ip_strategies <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "strategies")) |>
    dplyr::semi_join(tbl_strategy_lookups, by = c("strategy"))

  tbl_inpatients |>
    dplyr::inner_join(tbl_ip_strategies, by = c("EPIKEY")) |>
    dplyr::count(.data$FYEAR, procode = .data$PROCODE3, .data$strategy, .data$SPELDUR) |>
    dplyr::ungroup() |>
    dplyr::collect() |>
    janitor::clean_names()
}

get_ip_wli_data <- function(strategies_last_updated, provider_successors_last_updated, rtt_specialties) {
  force(strategies_last_updated)
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  tbl_inpatients <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients"))

  tbl_inpatients |>
    dplyr::filter(.data[["ADMIMETH"]] == "11", .data[["FYEAR"]] >= 201819) |>
    dplyr::group_by(.data[["FYEAR"]], .data[["PROCODE3"]]) |>
    dplyr::count(.data[["TRETSPEF"]]) |>
    dplyr::collect() |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(
        "tretspef",
        ~ dplyr::case_when(
          .x %in% rtt_specialties ~ .x,
          stringr::str_detect(.x, "^1(?!80|9[02])") ~
            "Other (Surgical)",
          stringr::str_detect(.x, "^(1(80|9[02])|[2346]|5(?!60)|83[134])") ~
            "Other (Medical)",
          TRUE ~ "Other"
        )
      )
    ) |>
    dplyr::count(.data[["tretspef"]], wt = .data[["n"]], name = "ip") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      dplyr::across("ip", ~ ifelse(.x < 5, 0, .x))
    )
}

get_ip_baseline_data <- function(provider_successors_last_updated, rtt_specialties) {
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients")) |>
    dplyr::mutate(
      group = dplyr::case_when(
        .data[["ADMIMETH"]] %LIKE% "1%" ~ "elective",
        .data[["ADMIMETH"]] %LIKE% "3%" ~ "maternity",
        .default = "non-elective"
      )
    ) |>
    dplyr::filter(.data[["FYEAR"]] >= 201819) |>
    dplyr::group_by(
      dplyr::across(
        c(
          "FYEAR",
          "PROCODE3",
          "group"
        )
      )
    ) |>
    dplyr::count(.data[["TRETSPEF"]]) |>
    dplyr::collect() |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(
        "tretspef",
        ~ dplyr::case_when(
          .data[["group"]] == "maternity" ~ "Other (Medical)",
          .x %in% rtt_specialties ~ .x,
          stringr::str_detect(.x, "^1(?!80|9[02])") ~
            "Other (Surgical)",
          stringr::str_detect(.x, "^(1(80|9[02])|[2346]|5(?!60)|83[134])") ~
            "Other (Medical)",
          TRUE ~ "Other"
        )
      )
    ) |>
    dplyr::count(.data[["tretspef"]], wt = .data[["n"]])
}
