get_repat_local_ip_data <- function(rtt_specialties, provider_successors_last_updated, ccg_to_icb_last_updated) {
  force(provider_successors_last_updated)
  force(ccg_to_icb_last_updated)
  con <- get_con("HESData")

  tbl <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients")) |>
    dplyr::group_by(.data$FYEAR, .data$icb22cdh, .data$ADMIMETH, .data$TRETSPEF)

  icb <- tbl |>
    dplyr::count(name = "icb_n") |>
    dplyr::collect() |>
    dplyr::ungroup() |>
    janitor::clean_names()

  provider <- tbl |>
    dplyr::filter(.data$is_main_icb == 1) |>
    dplyr::count(.data$PROCODE3, name = "provider_n") |>
    dplyr::collect() |>
    dplyr::ungroup() |>
    janitor::clean_names() |>
    dplyr::rename(procode = "procode3")

  provider |>
    dplyr::inner_join(icb, by = c("fyear", "icb22cdh", "admimeth", "tretspef")) |>
    dplyr::mutate(
      admigroup = dplyr::case_when(
        stringr::str_starts(.data$admimeth, "1") ~ "elective",
        stringr::str_starts(.data$admimeth, "3") ~ "maternity",
        TRUE ~ "non-elective"
      ),
      specialty = dplyr::case_when(
        .data$tretspef %in% rtt_specialties ~ .data$tretspef,
        stringr::str_detect(.data$tretspef, "^1(?!80|9[02])") ~ "Other (Surgical)",
        stringr::str_detect(.data$tretspef, "^(1(80|9[02])|[2346]|5(?!60)|83[134])") ~ "Other (Medical)",
        TRUE ~ "Other"
      )
    ) |>
    dplyr::group_by(.data$fyear, .data$icb22cdh, .data$procode, .data$admigroup, .data$specialty) |>
    dplyr::summarise(dplyr::across(tidyselect::ends_with("_n"), sum), .groups = "drop") |>
    dplyr::mutate(pcnt = .data$provider_n / .data$icb_n)
}

get_repat_local_op_data <- function(rtt_specialties, provider_successors_last_updated, ccg_to_icb_last_updated) {
  force(provider_successors_last_updated)
  force(ccg_to_icb_last_updated)
  con <- get_con("HESData")

  tbl <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "outpatients")) |>
    dplyr::group_by(.data$fyear, .data$icb22cdh, .data$tretspef)

  icb <- tbl |>
    dplyr::count(name = "icb_n") |>
    dplyr::collect() |>
    dplyr::ungroup() |>
    janitor::clean_names()

  provider <- tbl |>
    dplyr::filter(.data$is_main_icb == 1) |>
    dplyr::count(.data$procode3, name = "provider_n") |>
    dplyr::collect() |>
    dplyr::ungroup() |>
    janitor::clean_names() |>
    dplyr::rename(procode = "procode3")

  provider |>
    dplyr::inner_join(icb, by = c("fyear", "icb22cdh", "tretspef")) |>
    dplyr::mutate(
      specialty = dplyr::case_when(
        .data$tretspef %in% rtt_specialties ~ .data$tretspef,
        stringr::str_detect(.data$tretspef, "^1(?!80|9[02])") ~ "Other (Surgical)",
        stringr::str_detect(.data$tretspef, "^(1(80|9[02])|[2346]|5(?!60)|83[134])") ~ "Other (Medical)",
        TRUE ~ "Other"
      )
    ) |>
    dplyr::group_by(.data$fyear, .data$icb22cdh, .data$procode, .data$specialty) |>
    dplyr::summarise(dplyr::across(tidyselect::ends_with("_n"), sum), .groups = "drop") |>
    dplyr::mutate(pcnt = .data$provider_n / .data$icb_n)
}

get_repat_local_aae_data <- function(provider_successors_last_updated, ccg_to_icb_last_updated) {
  force(provider_successors_last_updated)
  force(ccg_to_icb_last_updated)
  con <- get_con("HESData")

  tbl <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "aae")) |>
    dplyr::mutate(is_ambulance = ifelse(.data$aearrivalmode == "1", TRUE, FALSE)) |>
    dplyr::group_by(.data$fyear, .data$icb22cdh, .data$is_ambulance)

  icb <- tbl |>
    dplyr::count(name = "icb_n") |>
    dplyr::collect() |>
    dplyr::ungroup() |>
    janitor::clean_names()

  provider <- tbl |>
    dplyr::filter(.data$is_main_icb == 1) |>
    dplyr::count(.data$procode3, name = "provider_n") |>
    dplyr::collect() |>
    dplyr::ungroup() |>
    janitor::clean_names() |>
    dplyr::rename(procode = "procode3")

  provider |>
    dplyr::inner_join(icb, by = c("fyear", "icb22cdh", "is_ambulance")) |>
    dplyr::mutate(pcnt = .data$provider_n / .data$icb_n)
}
