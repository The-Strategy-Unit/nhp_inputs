get_op_data <- function(provider_successors_last_updated) {
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "outpatients")) |>
    dplyr::filter(.data$has_procedures == 0) |>
    dplyr::group_by(
      .data$fyear,
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
    dplyr::rename(peer = "procode3") |>
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
    dplyr::rename(peer = "procode3") |>
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
