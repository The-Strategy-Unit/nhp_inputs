get_aae_data <- function(provider_successors_last_updated) {
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  tbl_age_table <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling_reference", "age_groups"))

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "aae")) |>
    dplyr::inner_join(tbl_age_table, c("activage" = "age")) |>
    dplyr::mutate(
      is_adult = .data$activage >= 18,
      is_ambulance = .data$aearrivalmode == "1"
    ) |>
    dplyr::group_by(
      .data$fyear,
      .data$procode3,
      .data$age_group,
      .data$sex,
      .data$is_ambulance,
      .data$is_adult
    ) |>
    dplyr::summarise(
      dplyr::across(
        c(
          "is_low_cost_referred_or_discharged",
          "is_left_before_treatment",
          "is_frequent_attender"
        ),
        sum,
        na.rm = TRUE
      ),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      subgroup = paste(
        ifelse(.data$is_adult, "adult", "child"),
        ifelse(.data$is_ambulance, "ambulance", "walk-in"),
        sep = "_"
      )
    ) |>
    dplyr::select(-"is_adult", -"is_ambulance") |>
    dplyr::rename(
      "low_cost_discharged" = "is_low_cost_referred_or_discharged",
      "left_before_seen" = "is_left_before_treatment",
      "frequent_attenders" = "is_frequent_attender"
    ) |>
    tidyr::pivot_longer(
      c(
        "low_cost_discharged",
        "left_before_seen",
        "frequent_attenders"
      )
    ) |>
    dplyr::arrange(.data$age_group) |>
    dplyr::transmute(
      .data$fyear,
      .data$procode3,
      dplyr::across("age_group", forcats::fct_inorder),
      .data$sex,
      strategy = glue::glue("{.data$name}_({.data$subgroup})"),
      .data$value,
      .data$n
    )
}

get_aae_age_sex_data <- function(aae_data) {
  aae_data |>
    dplyr::select(-"n") |>
    dplyr::rename("n" = "value", "procode" = "procode3")
}

get_aae_diag_data <- function(provider_successors_last_updated) {
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  tbl_aae <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "aae"))

  tbl_aae_diagnoses <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "aae_diagnoses")) |>
    dplyr::filter(.data$diagorder == 1) |>
    dplyr::select("aekey", "diagnosis" = "diag_2")

  tbl_aae |>
    dplyr::inner_join(tbl_aae_diagnoses, by = c("aekey")) |>
    dplyr::mutate(
      is_adult = .data$activage >= 18,
      is_ambulance = .data$aearrivalmode == "1"
    ) |>
    dplyr::group_by(
      .data$fyear,
      .data$procode3,
      .data$diagnosis,
      .data$is_ambulance,
      .data$is_adult
    ) |>
    dplyr::summarise(
      dplyr::across(
        c(
          "is_low_cost_referred_or_discharged",
          "is_left_before_treatment",
          "is_frequent_attender"
        ),
        sum,
        na.rm = TRUE
      ),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      subgroup = paste(
        ifelse(.data$is_adult, "adult", "child"),
        ifelse(.data$is_ambulance, "ambulance", "walk-in"),
        sep = "_"
      )
    ) |>
    dplyr::select(-"is_adult", -"is_ambulance") |>
    dplyr::rename(
      "low_cost_discharged" = "is_low_cost_referred_or_discharged",
      "left_before_seen" = "is_left_before_treatment",
      "frequent_attenders" = "is_frequent_attender"
    ) |>
    tidyr::pivot_longer(
      c(
        "low_cost_discharged",
        "left_before_seen",
        "frequent_attenders"
      )
    ) |>
    dplyr::group_by(
      .data$fyear,
      .data$procode3,
      strategy = glue::glue("{.data$name}_({.data$subgroup})")
    ) |>
    dplyr::select(-"name", -"n", -"subgroup") |>
    dplyr::rename("n" = "value", "procode" = "procode3") |>
    dplyr::mutate(p = .data$n / sum(.data$n)) |>
    dplyr::slice_max(order_by = .data$n, n = 6) |>
    dplyr::filter(.data$n > 5) |>
    dplyr::relocate("strategy", .before = "diagnosis") |>
    dplyr::ungroup()
}

get_aae_rates <- function(aae_data, peers) {
  aae_data |>
    dplyr::group_by(
      .data$fyear,
      peer = .data$procode3,
      .data$strategy
    ) |>
    dplyr::summarise(
      dplyr::across(c("value", "n"), sum),
      .groups = "drop"
    ) |>
    dplyr::inner_join(peers, by = c("peer")) |>
    dplyr::transmute(
      .data$fyear,
      .data$procode,
      .data$peer,
      .data$strategy,
      rate = .data$value / .data$n,
      .data$n
    ) |>
    add_mean_rows()
}
