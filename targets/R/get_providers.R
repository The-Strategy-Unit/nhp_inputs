get_providers <- function(lkp_peers, lkp_provider_names2, lkp_provider_names) {
  lkp_peers |>
    dplyr::distinct(.data$procode) |>
    dplyr::left_join(lkp_provider_names2, by = "procode") |>
    dplyr::left_join(lkp_provider_names, by = "procode") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dplyr::across("trust_name", tidyr::replace_na, .data$trust_name)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$trust_name) |>
    dplyr::mutate(
      dplyr::across("trust_name", stringr::str_to_title),
      dplyr::across("trust_name", stringr::str_replace, "Nhs", "NHS"),
      dplyr::across("trust_name", stringr::str_c, " (", .data$procode, ")")
    ) |>
    dplyr::select("trust_name", "procode") |>
    dplyr::distinct() |>
    tibble::deframe()
}
