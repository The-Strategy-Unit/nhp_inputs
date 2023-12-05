get_op_data <- function(provider_successors_last_updated) {
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  tbl_age_table <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling_reference", "age_groups"))

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "outpatients")) |>
    dplyr::inner_join(tbl_age_table, c("apptage" = "age")) |>
    dplyr::group_by(
      .data$fyear,
      .data$age_group,
      .data$sex,
      .data$procode3,
      .data$is_surgical_specialty,
      .data$is_adult,
      .data$is_tele_appointment
    ) |>
    dplyr::summarise(
      dplyr::across(
        c("is_first", "is_cons_cons_ref"),
        \(.x) sum(.x, na.rm = TRUE)
      ),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      subgroup = paste0(
        ifelse(.data$is_adult, "adult", "child"),
        "_",
        ifelse(.data$is_surgical_specialty, "", "non-"),
        "surgical"
      )
    ) |>
    dplyr::select(-"is_surgical_specialty", -"is_adult")
}

get_op_wli_data <- function(provider_successors_last_updated, rtt_specialties) {
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "outpatients")) |>
    dplyr::filter(.data[["fyear"]] >= 201819) |>
    dplyr::group_by(.data[["fyear"]], .data[["procode3"]]) |>
    dplyr::count(.data[["tretspef"]]) |>
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
    dplyr::count(.data[["tretspef"]], wt = .data[["n"]], name = "op") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      dplyr::across("op", ~ ifelse(.x < 5, 0, .x))
    )
}

get_op_diag_data <- function(op_age_sex_data) {
  # TODO: outpatients doesn't have much, if any, diagnosis coding. but the save process needs to have a set of results
  # for diagnoses. For now, we can use the age/sex data summarised to give us a placeholder table
  op_age_sex_data |>
    dplyr::count(.data[["fyear"]], .data[["procode"]], .data[["strategy"]], wt = .data[["n"]]) |>
    dplyr::transmute(
      .data$fyear,
      .data$procode,
      diagnosis = "R69",
      .data$strategy,
      .data$n,
      p = 1
    )
}


get_op_age_sex_data <- function(op_data) {
  strategies <- tibble::tribble(
    ~name, ~strategy,
    "is_cons_cons_ref", "consultant_to_consultant_reduction",
    "is_first", "followup_reduction",
    "is_tele_appointment", "convert_to_tele"
  )

  dplyr::bind_rows(
    op_data |>
      dplyr::filter(.data$is_tele_appointment == 0) |>
      dplyr::select(-"n") |>
      tidyr::pivot_longer(c("is_first", "is_cons_cons_ref"), values_to = "n"),
    op_data |>
      dplyr::filter(.data$is_tele_appointment == 1) |>
      dplyr::count(
        .data$fyear,
        .data$procode3,
        .data$age_group,
        .data$sex,
        .data$subgroup,
        wt = .data$n
      ) |>
      dplyr::mutate(name = "is_tele_appointment")
  ) |>
    dplyr::inner_join(strategies, by = "name") |>
    dplyr::arrange(.data$age_group) |>
    dplyr::transmute(
      .data$fyear,
      procode = .data$procode3,
      strategy = glue::glue("{strategy}_{subgroup}"),
      dplyr::across("age_group", forcats::fct_inorder),
      .data$sex,
      .data$n
    ) |>
    dplyr::arrange(.data$fyear, .data$procode, .data$strategy, .data$age_group, .data$sex)
}

get_op_convert_to_tele_data <- function(op_data, peers) {
  op_data |>
    dplyr::rename(peer = "procode3") |>
    dplyr::inner_join(
      peers,
      by = c("peer"),
      relationship = "many-to-many"
    ) |>
    dplyr::group_by(
      .data$fyear,
      .data$procode,
      .data$peer,
      strategy = glue::glue("convert_to_tele_{.data$subgroup}")
    ) |>
    dplyr::summarise(
      rate = 1 - sum(.data$n * .data$is_tele_appointment) / sum(.data$n),
      n = sum(.data$n),
      .groups = "drop"
    ) |>
    add_mean_rows()
}

get_op_consultant_to_consultant_reduction <- function(op_data, peers) {
  op_data |>
    dplyr::filter(.data$is_tele_appointment == 0) |>
    dplyr::group_by(.data$fyear, peer = .data$procode3, .data$subgroup) |>
    dplyr::summarise(
      dplyr::across(c("is_cons_cons_ref", "n"), sum),
      .groups = "drop"
    ) |>
    dplyr::inner_join(
      peers,
      by = c("peer"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      .data$fyear,
      .data$procode,
      .data$peer,
      strategy = glue::glue("consultant_to_consultant_reduction_{.data$subgroup}"),
      rate = .data$is_cons_cons_ref / .data$n,
      .data$n
    ) |>
    add_mean_rows()
}

get_op_followup_reduction <- function(op_data, peers) {
  op_data |>
    dplyr::filter(.data$is_tele_appointment == 0) |>
    dplyr::group_by(.data$fyear, peer = .data$procode3, .data$subgroup) |>
    dplyr::summarise(
      dplyr::across(c("is_first", "n"), sum),
      .groups = "drop"
    ) |>
    dplyr::inner_join(
      peers,
      by = c("peer"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      .data$fyear,
      .data$procode,
      .data$peer,
      strategy = glue::glue("followup_reduction_{.data$subgroup}"),
      rate = .data$is_first / .data$n,
      .data$n
    ) |>
    add_mean_rows() |>
    dplyr::mutate(
      firsts = .data$rate * .data$n,
      followups = .data$n - .data$firsts,
      rate = .data$followups / .data$firsts
    ) |>
    dplyr::select(-"firsts", -"followups")
}
