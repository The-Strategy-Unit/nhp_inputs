get_providers <- function(lkp_peers, lkp_provider_names2, lkp_provider_names) {
  lkp_peers |>
    dplyr::distinct(.data$procode) |>
    dplyr::left_join(lkp_provider_names2, by = "procode") |>
    dplyr::left_join(lkp_provider_names, by = "procode") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dplyr::across("trust_name", \(.x) tidyr::replace_na(.x, .data$trust_name))
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$trust_name) |>
    dplyr::mutate(
      dplyr::across("trust_name", \(.x) {
        .x |>
          stringr::str_to_title() |>
          stringr::str_replace("Nhs", "NHS") |>
          stringr::str_c(" (", .data$procode, ")")
      })
    ) |>
    dplyr::select("trust_name", "procode") |>
    dplyr::distinct() |>
    tibble::deframe()
}
