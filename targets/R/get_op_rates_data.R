get_op_data <- function(provider_successors_last_updated) {
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  tbl_age_table <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling_reference", "age_groups"))

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "outpatients")) |>
    dplyr::inner_join(tbl_age_table, c("apptage" = "age")) |>
    dplyr::filter(.data$has_procedures == 0) |>
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
        c(
          "is_first",
          "is_cons_cons_ref"
        ),
        sum,
        na.rm = TRUE
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


get_op_diag_data <- function(provider_successors_last_updated) {
  force(provider_successors_last_updated)

  strategies <- purrr::cross_df(
    list(
      strategy = c(
        "convert_to_tele",
        "consultant_to_consultant_reduction",
        "followup_reduction"
      ),
      subgroup = c(
        "adult_non-surgical",
        "adult_surgical",
        "child_surgical",
        "child_non-surgical"
      )
    )
  )

  con <- get_con("HESData")

  tbl_outpatients <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "outpatients"))

  tbl_outpatients_diagnoses <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "outpatients_diagnoses")) |>
    dplyr::filter(.data$diagorder == 1) |>
    dplyr::mutate(
      dplyr::across("diagnosis", LEFT, 3)
    )

  tbl_outpatients |>
    dplyr::inner_join(tbl_outpatients_diagnoses, by = c("attendkey")) |>
    dplyr::filter(.data$is_tele_appointment == 0) |>
    dplyr::group_by(.data$fyear, .data$procode3, .data$diagnosis, .data$is_surgical_specialty, .data$is_adult) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop_last") |>
    dplyr::mutate(p = .data$n * 1.0 / sum(.data$n, na.rm = TRUE)) |>
    dbplyr::window_order(dplyr::desc(.data$n)) |>
    dplyr::filter(
      dplyr::row_number() <= 6,
      .data$n >= 5
    ) |>
    dplyr::ungroup() |>
    dplyr::collect() |>
    janitor::clean_names() |>
    dplyr::rename(procode = "procode3") |>
    dplyr::mutate(
      subgroup = paste0(
        ifelse(.data$is_adult, "adult", "child"),
        "_",
        ifelse(.data$is_surgical_specialty, "", "non-"),
        "surgical"
      )
    ) |>
    dplyr::inner_join(strategies, by = "subgroup") |>
    dplyr::transmute(
      .data$fyear,
      .data$procode,
      .data$diagnosis,
      strategy = glue::glue("{strategy}_({subgroup})"),
      .data$n,
      .data$p
    )
}


get_op_age_sex_data <- function(op_data) {
  strategies <- purrr::cross_df(
    list(
      strategy = c(
        "convert_to_tele",
        "consultant_to_consultant_reduction",
        "followup_reduction"
      ),
      subgroup = c(
        "adult_non-surgical",
        "adult_surgical",
        "child_surgical",
        "child_non-surgical"
      )
    )
  )

  op_data |>
    dplyr::filter(.data$is_tele_appointment == 0) |>
    dplyr::count(.data$fyear, .data$age_group, .data$sex, procode = .data$procode3, .data$subgroup, wt = .data$n) |>
    dplyr::inner_join(strategies, by = "subgroup") |>
    dplyr::transmute(
      .data$fyear,
      .data$procode,
      strategy = glue::glue("{strategy}_({subgroup})"),
      dplyr::across("age_group", forcats::fct_inorder),
      .data$sex,
      .data$n
    ) |>
    dplyr::arrange(.data$fyear, .data$procode, .data$strategy, .data$age_group, .data$sex)
}

get_op_convert_to_tele_data <- function(op_data, peers) {
  op_data |>
    dplyr::rename(peer = "procode3") |>
    dplyr::inner_join(peers, by = c("peer")) |>
    dplyr::group_by(
      .data$fyear,
      .data$procode,
      .data$peer,
      strategy = glue::glue("convert_to_tele_({.data$subgroup})")
    ) |>
    dplyr::summarise(
      rate = sum(.data$n * .data$is_tele_appointment) / sum(.data$n),
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
    dplyr::inner_join(peers, by = c("peer")) |>
    dplyr::transmute(
      .data$fyear,
      .data$procode,
      .data$peer,
      strategy = glue::glue("consultant_to_consultant_reduction_({.data$subgroup})"),
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
    dplyr::inner_join(peers, by = c("peer")) |>
    dplyr::transmute(
      .data$fyear,
      .data$procode,
      .data$peer,
      strategy = glue::glue("followup_reduction_({.data$subgroup})"),
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
