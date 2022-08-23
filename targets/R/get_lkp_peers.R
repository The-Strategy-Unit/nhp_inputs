get_lkp_peers <- function(lkp_peers_file, provider_successors) {
  lkp_peers <- readxl::read_excel(lkp_peers_file, sheet = "Default peers") |>
    janitor::remove_empty(which = "cols") |>
    janitor::clean_names()

  # if a provider is succeeded by two, keep just the first successor
  provider_successors <- provider_successors |>
    dplyr::group_by(.data$org_code) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::select(-.data$date)

  lkp_peers |>
    dplyr::mutate(org_code = .data$procode) |>
    dplyr::rows_update(
      provider_successors |>
        dplyr::rename(procode = .data$succ_org_code) |>
        dplyr::semi_join(lkp_peers, by = c("org_code" = "procode")),
      by = "org_code"
    ) |>
    dplyr::mutate(org_code = .data$peer) |>
    dplyr::rows_update(
      provider_successors |>
        dplyr::rename(peer = .data$succ_org_code) |>
        dplyr::semi_join(lkp_peers, by = c("org_code" = "peer")),
      by = "org_code"
    ) |>
    dplyr::select(-.data$org_code)
}
