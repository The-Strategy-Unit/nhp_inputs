get_repat_local_ip_data <- function(rtt_specialties, provider_successors_last_updated, ccg_to_icb_last_updated) {
  force(provider_successors_last_updated)
  force(ccg_to_icb_last_updated)
  con <- get_con("HESData")

  df <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients")) |>
    dplyr::mutate(
      procode = ifelse(
        .data$is_main_icb == 1,
        .data$PROCODE3,
        "Other"
      )
    ) |>
    dplyr::count(.data$FYEAR, .data$icb22cdh, .data$procode, .data$ADMIMETH, .data$TRETSPEF) |>
    dplyr::collect() |>
    dplyr::ungroup() |>
    janitor::clean_names()

  df |>
    dplyr::filter(.data$procode != "Other") |>
    dplyr::distinct(.data$procode, .data$icb22cdh) |>
    dplyr::inner_join(
      df |>
        dplyr::rename(provider = "procode"),
      by = c("icb22cdh")
    ) |>
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
    dplyr::group_by(.data$procode, .data$icb22cdh, .data$fyear, .data$admigroup, .data$specialty, .data$provider) |>
    dplyr::summarise(dplyr::across("n", sum), .groups = "drop_last") |>
    dplyr::mutate(pcnt = .data$n / sum(.data$n)) |>
    dplyr::ungroup()
}

get_repat_local_op_data <- function(rtt_specialties, provider_successors_last_updated, ccg_to_icb_last_updated) {
  force(provider_successors_last_updated)
  force(ccg_to_icb_last_updated)
  con <- get_con("HESData")

  df <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "outpatients")) |>
    dplyr::mutate(
      procode = ifelse(
        .data$is_main_icb == 1,
        .data$procode3,
        "Other"
      )
    ) |>
    dplyr::count(.data$fyear, .data$icb22cdh, .data$procode, .data$tretspef) |>
    dplyr::collect() |>
    dplyr::ungroup() |>
    janitor::clean_names()

  df |>
    dplyr::filter(.data$procode != "Other") |>
    dplyr::distinct(.data$procode, .data$icb22cdh) |>
    dplyr::inner_join(
      df |>
        dplyr::rename(provider = "procode"),
      by = c("icb22cdh")
    ) |>
    dplyr::mutate(
      specialty = dplyr::case_when(
        .data$tretspef %in% rtt_specialties ~ .data$tretspef,
        stringr::str_detect(.data$tretspef, "^1(?!80|9[02])") ~ "Other (Surgical)",
        stringr::str_detect(.data$tretspef, "^(1(80|9[02])|[2346]|5(?!60)|83[134])") ~ "Other (Medical)",
        TRUE ~ "Other"
      )
    ) |>
    dplyr::group_by(.data$procode, .data$icb22cdh, .data$fyear, .data$specialty, .data$provider) |>
    dplyr::summarise(dplyr::across("n", sum), .groups = "drop_last") |>
    dplyr::mutate(pcnt = .data$n / sum(.data$n)) |>
    dplyr::ungroup()
}

get_repat_local_aae_data <- function(provider_successors_last_updated, ccg_to_icb_last_updated) {
  force(provider_successors_last_updated)
  force(ccg_to_icb_last_updated)
  con <- get_con("HESData")

  df <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "aae")) |>
    dplyr::mutate(
      is_ambulance = ifelse(.data$aearrivalmode == "1", TRUE, FALSE),
      procode = ifelse(
        .data$is_main_icb == 1,
        .data$procode3,
        "Other"
      )
    ) |>
    dplyr::count(.data$fyear, .data$icb22cdh, .data$procode, .data$is_ambulance) |>
    dplyr::collect() |>
    dplyr::ungroup() |>
    janitor::clean_names()

  df |>
    dplyr::filter(.data$procode != "Other") |>
    dplyr::distinct(.data$procode, .data$icb22cdh) |>
    dplyr::inner_join(
      df |>
        dplyr::rename(provider = "procode"),
      by = c("icb22cdh")
    ) |>
    dplyr::group_by(.data$procode, .data$icb22cdh, .data$fyear, .data$is_ambulance) |>
    dplyr::mutate(pcnt = .data$n / sum(.data$n)) |>
    dplyr::ungroup()
}

get_expat_repat_data <- function(repat_local_ip_data,
                                 repat_local_op_data,
                                 repat_local_aae_data) {
  dplyr::bind_rows(
    .id = "type",
    repat_local = dplyr::bind_rows(
      .id = "activity_type",
      ip = repat_local_ip_data,
      op = repat_local_op_data,
      aae = repat_local_aae_data
    )
  ) |>
    dplyr::group_nest(.data$procode, .data$type, .data$activity_type) |>
    dplyr::mutate(
      dplyr::across("data", purrr::map, janitor::remove_empty, which = "cols")
    ) |>
    dplyr::group_nest(.data$procode, .data$type) |>
    dplyr::mutate(
      dplyr::across("data", purrr::map, tibble::deframe)
    ) |>
    dplyr::group_nest(.data$procode) |>
    dplyr::mutate(
      dplyr::across("data", purrr::map, tibble::deframe)
    ) |>
    tibble::deframe()
}
