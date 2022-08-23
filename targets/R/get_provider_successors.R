get_provider_successors <- function(ods_successors, list_providers) {
  ods_successors |>
    dplyr::semi_join(list_providers, by = c("org_code" = "PROCODE3")) |>
    dplyr::group_by(.data$org_code) |>
    # TODO: address multiple successors
    dplyr::filter(row_number() == 1) |>
    dplyr::ungroup()
}
