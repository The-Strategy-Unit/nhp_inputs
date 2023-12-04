get_lkp_provider_names <- function() {
  con <- get_con("Reference")

  dplyr::tbl(con, dbplyr::in_schema("dbo", "DIM_tbOrganisation")) |>
    dplyr::filter(.data$TypeDescription == "NHS Trust") |>
    dplyr::distinct(.data$OrganisationName, .data$OrganisationCode) |>
    dplyr::collect() |>
    janitor::clean_names() |>
    dplyr::filter(stringr::str_detect(.data$organisation_code, "^R")) |>
    dplyr::mutate(
      dplyr::across("organisation_code", \(.x) stringr::str_sub(.x, 1, 3)),
      dplyr::across(
        "organisation_name",
        \(.x) .x |>
          stringr::str_to_title() |>
          stringr::str_replace("Nhs", "NHS")
      )
    ) |>
    dplyr::rename(procode = "organisation_code")
}
