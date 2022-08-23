get_lkp_provider_names <- function() {
  con <- get_con("Reference")

  dplyr::tbl(con, dbplyr::in_schema("dbo", "DIM_tbOrganisation")) |>
    dplyr::filter(.data$TypeDescription == "NHS Trust") |>
    dplyr::distinct(.data$OrganisationName, .data$OrganisationCode) |>
    dplyr::collect() |>
    janitor::clean_names() |>
    dplyr::filter(stringr::str_detect(.data$organisation_code, "^R")) |>
    dplyr::mutate(
      dplyr::across(.data$organisation_code, stringr::str_sub, 1, 3),
      dplyr::across(.data$organisation_name, stringr::str_to_title),
      dplyr::across(.data$organisation_name, stringr::str_replace, "Nhs", "NHS")
    ) |>
    dplyr::rename(procode = .data$organisation_code)
}
