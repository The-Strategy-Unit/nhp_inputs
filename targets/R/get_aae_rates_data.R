get_aae_data <- function(provider_successors_last_updated) {
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "aae")) |>
    dplyr::mutate(
      is_adult = .data$activage >= 18,
      is_ambulance = .data$aearrivalmode == "1"
    ) |>
    dplyr::group_by(.data$fyear, .data$procode3, .data$is_ambulance, .data$is_adult) |>
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
    dplyr::collect()
}

get_aae_rates <- function(aae_data, peers) {
  aae_data |>
    dplyr::rename(peer = "procode3") |>
    dplyr::inner_join(peers, by = c("peer")) |>
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
    dplyr::transmute(
      .data$fyear,
      .data$procode,
      .data$peer,
      strategy = paste0(
        .data$name,
        "_(",
        ifelse(.data$is_adult, "adult", "child"),
        "_",
        ifelse(.data$is_ambulance, "ambulance", "walk-in"),
        ")"
      ),
      rate = .data$value / .data$n,
      .data$n
    )
}
