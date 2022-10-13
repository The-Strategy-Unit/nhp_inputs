get_op_data <- function(provider_successors_last_updated) {
  force(provider_successors_last_updated)

  con <- get_con("HESData")

  tbl_op <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "outpatients"))

  f2f_appts <- tbl_op |>
    dplyr::filter(.data$is_tele_appointment == 0, .data$has_procedures == 0) |>
    dplyr::group_by(.data$fyear, .data$procode3, .data$is_surgical_specialty, .data$is_adult) |>
    dplyr::summarise(
      dplyr::across(
        c(
          "followup_reduction" = "is_first",
          "consultant_to_consultant_reduction" = "is_cons_cons_ref"
        ),
        sum,
        na.rm = TRUE
      ),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      dplyr::across("followup_reduction", ~ (.data$n - .x) / .x),
      dplyr::across("consultant_to_consultant_reduction", ~ .x / .data$n)
    ) |>
    tidyr::pivot_longer(
      c("followup_reduction", "consultant_to_consultant_reduction"),
      names_to = "strategy",
      values_to = "rate"
    )

  tele_appts <- tbl_op |>
    dplyr::filter(.data$has_procedures == 0) |>
    dplyr::group_by(.data$fyear, .data$procode3, .data$is_surgical_specialty, .data$is_adult) |>
    dplyr::summarise(
      strategy = "convert_to_tele",
      rate = sum(.data$is_tele_appointment, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(dplyr::across("rate", ~ .x / .data$n))

  dplyr::bind_rows(f2f_appts, tele_appts) |>
    dplyr::rename(procode = "procode3") |>
    dplyr::mutate(
      subgroup = paste0(
        ifelse(.data$is_adult, "adult", "child"),
        "_",
        ifelse(.data$is_surgical_specialty, "", "non-"),
        "surgical"
      ),
      .after = "strategy"
    ) |>
    dplyr::relocate("n", .after = "rate") |>
    dplyr::select(-tidyselect::starts_with("is_"))
}
